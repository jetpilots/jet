
// No #includes in headers

#ifndef HAVE_JET_BASE
#include "../../../jet/modules/base.h"
#include "../../../jet/modules/sys_time.h"
#endif

typedef struct XMLAttr XMLAttr;
typedef struct XMLNode XMLNode;
typedef struct XMLParser XMLParser;
static const int istep = 2;

monostatic const char* const spaces
    = "                                                 ";
#define xmlassert(par, expr)                                                   \
    if (!(expr))                                                               \
        fprintf(stderr, "%s:%d: assertion failed:\n    %s\nin %s:%d:%d.\n",    \
            __FILE__, __LINE__, #expr, par->filename, par->line, par->col),    \
            exit(1);

// if you want to use nan tagging
// for json / xml keep list<node>
// for yaml keep list<node*>
// for json/xml the value IS the list item, so no extra storage (no ptr to
// node) for yaml you may need refs to primitives (num/bool) so this cant
// happen

// XML is fastest, keeps tag,attr,text/children,line/col (32B)
// FXL has same structs like XML
// HTML keeps tag,attrs,text/children,line/col,id,classlist (48B)
// FML has same structs like HTML
// ^ this is a good idea, HTML has lots of stuff that XML doesnt need
// besides FML can integrate with styles/scripts which XML doesnt need
// rename all
// FMLDocument -> HTML
// FMLStyles -> CSS
// FMLScript -> JS
struct XMLAttr {
    const char* key;
    const char* val;
};

struct XMLNode {
    const char* tag;
    List(XMLAttr) * attributes;
    union {
        const char* text;
        List(XMLNode) * children;
    };
}; // not keeping line/col here, keep them on the parent stack along with ptr.
// for HTML you need them in the node, since validation could print err msgs.

struct XMLParser {
    const char* filename;
    int line, col;
    char *data, *end;
    char* pos;
};

// this MKSTAT should not go here, but in runtime
MKSTAT(XMLNode)
MKSTAT(XMLAttr)
MKSTAT(XMLParser)

monostatic XMLNode* XMLNode_new(const char* tag) {
    XMLNode* ret = NEW(XMLNode);
    ret->tag = tag;
    return ret;
}

monostatic XMLNode* XMLNode_newText(const char* text) {
    XMLNode* ret = NEW(XMLNode);
    ret->tag = NULL;
    ret->text = text;
    return ret;
}

monostatic XMLAttr* XMLAttr_new(const char* key, const char* value) {
    XMLAttr* ret = NEW(XMLAttr);
    ret->key = key;
    ret->val = value;
    return ret;
}

monostatic XMLParser* XMLParser_fromStringClone(const char* str) {
    XMLParser* par = NEW(XMLParser);
    size_t len = CString_length(str);
    par->data = pstrndup(str, len);
    par->pos = par->data;
    par->end = par->data + len;
    par->line = 1;
    return par;
}

monostatic XMLParser* XMLParser_fromFile(char* filename) {
    size_t flen = CString_length(filename);

    struct stat sb;
    if (stat(filename, &sb) != 0) {
        eprintf("jetXml: file '%s' not found.\n", filename);
        return NULL;
    } else if (S_ISDIR(sb.st_mode)) {
        eprintf(
            "jetXml: '%s' is a folder; only files are accepted.\n", filename);
        return NULL;
    } else if (access(filename, R_OK) == -1) {
        eprintf("jetXml: no permission to read file '%s'.\n", filename);
        return NULL;
    }

    FILE* file = fopen(filename, "r");
    assert(file);

    XMLParser* ret = NEW(XMLParser);

    ret->filename = filename;
    // ret->noext = CString_noext(filename);
    fseek(file, 0, SEEK_END);
    const size_t size = ftell(file);

    // if (size < FILE_SIZE_MAX) {
    ret->data = (char*)malloc(size);
    fseek(file, 0, SEEK_SET);
    if (fread(ret->data, size, 1, file) != 1) {
        eprintf("jetXml: the whole file '%s' could not be read.\n", filename);
        fclose(file);
        free(ret->data);
        return NULL;
        // would leak if ret was malloc'd directly, but we have a pool
    }
    ret->end = ret->data + size;
    ret->pos = ret->data;
    ret->line = 1;

    fclose(file);
    return ret;
}

monostatic bool isAnyOf(char ch, char* chars) {
    while (*chars)
        if (*chars++ == ch) return true;
    return false;
}

monostatic const char* findchars_fast(const char* buf, const char* buf_end,
    const char* chars, size_t chars_size) {
    // *found = 0;
#if __SSE4_2__
    if (likely(buf_end - buf >= 16)) {
        __m128i chars16 = _mm_loadu_si128((const __m128i*)chars);

        size_t left = (buf_end - buf) & ~15;
        do {
            __m128i b16 = _mm_loadu_si128((void*)buf);
            int r = _mm_cmpestri(chars16, chars_size, b16, 16,
                _SIDD_LEAST_SIGNIFICANT | _SIDD_CMP_EQUAL_ANY
                    | _SIDD_UBYTE_OPS);
            if (unlikely(r != 16)) {
                buf += r;
                // *found = 1;
                break;
            }
            buf += 16;
            left -= 16;
        } while (likely(left != 0));
    }
    return buf;
#else
    return buf + strcspn(buf, chars); // strpbrk(buf, chars);
#endif
}

monostatic List(XMLAttr) * XMLParser_parseAttrs(XMLParser* parser) {
    List(XMLAttr)* list = NULL;
    List(XMLAttr)** listp = &list;

    while (*parser->pos //
        && *parser->pos != '>' //
        && *parser->pos != '/' //
        && *parser->pos != '?' //
        // TODO: just use isAnyOf below and test if it is right
        && !isAnyOf(*parser->pos, ">/?")) {
        const char* name = parser->pos;
        while (*parser->pos != '=') parser->pos++;
        *parser->pos++ = 0; // trample =

        char* markers;
        if (isAnyOf(*parser->pos, "'\"")) {
            markers = (*parser->pos == '"') ? "\"" : "'";
            parser->pos++;
        } else {
            markers = " >/?\t\n";
        }

        const char* value = parser->pos;

        while (!isAnyOf(*parser->pos, markers)) parser->pos++;
        if (!isAnyOf(*parser->pos, "/?>"))
            *parser->pos++ = 0; // trample the marker if " or ' or spaces
        // ^ DON't trample the markers / or > here, because in the case of
        // e.g. <meta name=content-type content=utf8/> it will trample the /
        // not allowing the calling parseTags to detect that the tag is
        // self-closing. Let the caller trample.

        while (isAnyOf(*parser->pos, " \t\n")) {
            if (*parser->pos == '\n') {
                parser->line++;
                parser->col = 0;
            }
            parser->col++; // why incrememnt at each iter? set a backref to the
                           // pos at which col started and then subtract once
                           // later
            *parser->pos++ = 0; // skip and trample whitespace
        }
        XMLAttr* attr = XMLAttr_new(name, value);
        listp = PtrList_append(listp, attr);
    }
    //    if (*parser->pos == '/') *parser->pos++ = 0;
    //    assert(*parser->pos == '>');
    //    *parser->pos++ = 0;
    // don't consume ending />, leave it at that
    return list;
}

monostatic List(XMLNode) * XMLParser_parseTags(XMLParser* parser) {
    List(XMLNode)* list = NULL;
    List(XMLNode)** listp = &list;

    while (parser->pos < parser->end) {
        switch (*parser->pos) {
        case ' ':
        case '\t':
            parser->col++;
            parser->pos++;
            break;
        case '\n':
            parser->line++;
            parser->col = 0;
            *parser->pos++ = 0;
            break;
        case '<': {
            *parser->pos++ = 0;

            bool noChild = false;

            if (*parser->pos == '/') { // closing tag here means empty content
                parser->pos++; // note that will be past </
                return list; // null if no elements were found at this level
            } else if (!strncasecmp(parser->pos, "[CDATA[", 6)) { // cdata
            } else { // opening tag
                XMLNode* node = XMLNode_new(parser->pos);
                while (!isAnyOf(*parser->pos, " />\n\t")) parser->pos++; // SSE?

                if (isAnyOf(*parser->pos, " \t\n")) {
                    // TODO can you not refactor this if + while?
                    if (*parser->pos == '\n') {
                        parser->line++;
                        parser->col = 0;
                    }
                    parser->col++;
                    *parser->pos++ = 0;
                    while (isAnyOf(*parser->pos, " \t\n")) {
                        if (*parser->pos == '\n') {
                            parser->line++;
                            parser->col = 0;
                        }
                        parser->col++;
                        parser->pos++;
                    }
                    node->attributes = XMLParser_parseAttrs(parser);
                }
                while (*parser->pos == ' ') parser->pos++;

                switch (*parser->pos) {
                case '/':
                case '?':
                    *parser->pos++ = 0;
                    xmlassert(parser, *parser->pos == '>');
                    noChild = true;
                //  fall through
                case '>':
                    // tag has ended. parse children
                    *parser->pos++ = 0;
                    if (!noChild) {
                        node->children = XMLParser_parseTags(parser);

                        char* closingTag = parser->pos;
                        while (*parser->pos != '>') // SSE?
                            parser->pos++; // seek to end of closing tag
                        // ^ TODO: line may need to be incremented here
                        *parser->pos++ = 0;

#ifndef FP_XML_SKIP_CLOSING_CHECKS // this is about 10% runtime for a large file
                        if (!*closingTag) {
                            printf("error: found end of file, expected "
                                   "</%s>\n",
                                node->tag);
                            exit(1);
                        } else if (strcasecmp(closingTag, node->tag)) {
                            printf("%s:%d:%d: found </%s>, expected </%s>\n",
                                parser->filename, parser->line, parser->col,
                                closingTag, node->tag);
                            exit(1);
                        }
#endif
                    }
                    break;
                default:
                    eprintf("%s:%d:%d: unexpected '%c' (\"%.16s\"...)\n",
                        parser->filename, parser->line, parser->col,
                        *parser->pos, parser->pos);
                    break;
                }

                listp = PtrList_append(listp, node);
            }
        } break;

        default:

        {
            char* text = parser->pos;
            // printf("oops2: unexpected '%c' (\"%.16s...\")\n",
            // *parser->pos, parser->pos);
            while (*parser->pos != '<' and parser->pos < parser->end) {
                if (*parser->pos == '\n') {
                    parser->line++;
                    parser->col = 0;
                }
                parser->pos++;
            } // parser->pos = findchars_fast(parser->pos, parser->end, "<",
            // 1); relying on the </ detector state to trample the <
            XMLNode* textNode = XMLNode_newText(text);
            listp = PtrList_append(listp, textNode);
        }
        }
    }
    if (parser->pos < parser->end) printf("error: data unparsed\n");
    return list;
}

monostatic void FMLAttr_print(XMLAttr* attr, int indent) {
    // const char* quo = strpbrk(attr->val, " =&") || 1 ? "'" : "";
    if (strcmp(attr->key, "id") && strcmp(attr->key, "class"))
        if (strcmp(attr->val, "no") && strcmp(attr->val, "yes")
            && (*attr->val < '0' //
                || *attr->val > '9' //
                || strpbrk(attr->val, "-: ")))
            printf(" %s='%s'", attr->key, attr->val);
        else
            printf(" %s=%s", attr->key, attr->val);
}

monostatic void XMLAttr_print(XMLAttr* attr, int indent) {
    printf(" %s=\"%s\"", attr->key, attr->val);
}
monostatic void XMLNode_print(XMLNode* node, int indent);
monostatic void XMLNodeList_print(List(XMLNode) * nodeList, int indent) {
    foreach (XMLNode*, childNode, nodeList)
        XMLNode_print(childNode, indent);
}
monostatic void FMLNode_print(XMLNode* node, int indent, bool skipws);
monostatic void FMLNodeList_print(
    List(XMLNode) * nodeList, int indent, bool skipws) {
    foreachn(XMLNode*, childNode, li, nodeList) {
        FMLNode_print(childNode, indent, skipws);
        if (li->next) printf("\n%.*s", indent, spaces);
    }
}
monostatic void XMLNode_print(XMLNode* node, int indent) {
    if (node->tag) {
        printf("%.*s<%s%s", indent, spaces, node->tag,
            node->attributes     ? ""
                : node->children ? ">\n"
                                 : "/>\n");
        foreach (XMLAttr*, attr, node->attributes)
            XMLAttr_print(attr, indent);
        if (node->attributes) printf("%s\n", node->children ? ">" : " />");
        XMLNodeList_print(node->children, indent + 2);
        if (node->children) printf("%.*s</%s>\n", indent, spaces, node->tag);
    } else {
        printf("%.*s%s\n", indent, spaces, node->text);
    }
}

monostatic void FMLStr_print(const char* str, int indent, bool skipws) {
    // if (! skipws)
    //     printf("`\n%.*s", indent, spaces);
    // else
    printf("`");
    const char* c = str;
    while (*c) {
        switch (*c) {
        case '\n':
            if (!skipws) printf("\n%.*s", indent, spaces);
            break;
            // fallthru
        case ' ':
        case '\t':
            putc(*c, stdout);
            if (skipws) {
                while (isAnyOf(*c, " \n\t")) c++;
                continue;
            }
            break;
        case '`':
            printf("\\`");
            break;
        // case '\0':
        //     return;
        case '&':
        default:
            putc(*c, stdout);
        }
        c++;
    }
    printf("`");

    // printf("%.*s`\n", indent, spaces);
    // puts('`');
}
monostatic void FMLAttrList_print(List(XMLAttr) * attributes, int indent) {
    foreach (XMLAttr*, attr, attributes) //
        if (!strcmp(attr->key, "id")) {
            printf(" #%s", attr->val);
            break;
        }

    foreach (XMLAttr*, attr, attributes) //
        if (!strcmp(attr->key, "class")) {
            char* now = attr->val;
            char* nxt = strchr(attr->val, ' ');
            while (nxt) {
                printf(" .%.*s", nxt - now, now);
                now = nxt + 1;
                nxt = strchr(now, ' ');
            }
            if (*now) printf(" .%s", now);

            break;
        }

    foreach (XMLAttr*, attr, attributes)
        FMLAttr_print(attr, indent);
}

monostatic void FMLNode_print(XMLNode* node, int indent, bool skipws) {
    if (node->tag) {
        printf("%s", node->tag);

        FMLAttrList_print(node->attributes, indent);
        // if (node->attributes)
        // PARENT MUST PRINT INDENTATION if you want to collapse single-child
        // tags with no attributes like div > div > div and so on.
        bool skipw = strcmp(node->tag, "script") && strcmp(node->tag, "style");
        if (node->attributes or (node->children && node->children->next)) {
            if (node->children) {
                printf("\n%.*s", indent + istep, spaces);
                FMLNodeList_print(node->children, indent + istep, skipw);
            }
        } else {
            if (node->children) {
                printf(" > ");
                // printf(" > ", indent, spaces);
                FMLNodeList_print(node->children, indent + istep, skipw);
            }
        } // if (node->children) printf("%.*s</%s>\n", indent, spaces,
          // node->tag);
    } else {
        // TODO: split the node->text by newlines and indent each line
        // this always needs to be quoted!!!!

        FMLStr_print(node->text, indent, skipws);
        // printf("%.*s`%s`\n", indent, spaces, node->text);
    }
}
//   -- import yy-y/io/file.fml as head // disallow such names
//   -- macro og locale=en_US &name='' type=website url='' image='' imgw=0
//      meta property='og:locale' content=$locale
//      meta property='og:type' content=$type
//   -- end

#include "XML.h"

int main(int argc, char* argv[]) {
    //     char* xmlstr = "" //
    //                    "<xml>" //
    //                    "  <head>" //
    //                    "    <title attr=value attr2=\"value2 what>\">" //
    //                    "    </title>" //
    //                    "    <meta name=content-type content=utf-8/>" //
    //                    "  </head>" //
    //                    "</xml>"; //
    //     "<foot>" //
    //     "</foot>";
    //     xmlstr = "<meta name=content-type content=utf-8/><meta name=keywords
    //     content='rail,train,goods'/>"; xmlstr = "<a>"; xmlstr = "<a></a>";

    // xmlstr = "<?xml version='1.0'?>"
    //          "<Tests xmlns='http://www.adatum.com'>"
    //          "<Test TestId='0001' TestType='CMD'>"
    //          "<Name>Convert number to string</Name>"
    //          "<CommandLine>Examp1.EXE</CommandLine>"
    //          "<Input>1</Input>"
    //          "<Output>One</Output>"
    //          "</Test>"
    //          "<Test TestId='0002' TestType='CMD'>"
    //          "<Name>Find succeeding characters</Name>"
    //          "<CommandLine>Examp2.EXE</CommandLine>"
    //          "<Input>abc</Input>"
    //          "<Output>def</Output>"
    //          "</Test>"
    //          "<Test TestId='0003' TestType='GUI'>"
    //          "<Name>Convert multiple numbers to strings</Name>"
    //          "<CommandLine>Examp2.EXE /Verbose</CommandLine>"
    //          "<Input>123</Input>"
    //          "<Output>One Two Three</Output>"
    //          "</Test>"
    //          "<Test TestId='0004' TestType='GUI'>"
    //          "<Name>Find correlated key</Name>"
    //          "<CommandLine>Examp3.EXE</CommandLine>"
    //          "<Input>a1</Input>"
    //          "<Output>b1</Output>"
    //          "</Test>"
    //          "<Test TestId='0005' TestType='GUI'>"
    //          "<Name>Count characters</Name>"
    //          "<CommandLine>FinalExamp.EXE</CommandLine>"
    //          "<Input>This is a test</Input>"
    //          "<Output>14</Output>"
    //          "</Test>"
    //          "<Test TestId='0006' TestType='GUI'>"
    //          "<Name>Another Test</Name>"
    //          "<CommandLine>Examp2.EXE</CommandLine>"
    //          "<Input>Test Input</Input>"
    //          "<Output>10</Output>"
    //          "</Test>"
    //          "</Tests>";

    // XMLParser *par = XMLParser_fromStringClone(xmlstr);
    if (argc < 2) {
        printf("usage: %s <filename>\n", argv[0]);
        exit(1);
    }

    sys_time_Time t0; // = sys_time_getTime();
    XMLParser* par = XMLParser_fromFile(argv[1]);
    double tms; // = sys_time_clockSpanMicro(t0) / 1.0e3;
    // eprintf("\e[1mread time:\e[0m %.1f ms (%.2f GB/s)\n", tms,
    //     1 / ((tms / 1e3) * 1e9 / (par->end - par->data))); // sw.print();

    t0 = sys_time_getTime();
    List(XMLNode)* parsed = XMLParser_parseTags(par);
    if (argc > 2 && *argv[2] == 'x') XMLNodeList_print(parsed, 0);
    if (argc > 2 && *argv[2] == 'f') FMLNodeList_print(parsed, 0, 0);

    tms = sys_time_clockSpanMicro(t0) / 1.0e3;

    eputs("-------------------------------------------------------"
          "\n");
    allocstat(XMLAttr);
    allocstat(XMLParser);
    allocstat(XMLNode);
    allocstat(PtrList);
    eputs("-------------------------------------------------------"
          "\n");
    eprintf("*** Total size of nodes                     = %7d B\n",
        gPool->usedTotal);
    eprintf("*** Space allocated for nodes               = %7d B\n",
        gPool->capTotal);
    eprintf("*** Node space utilisation                  = %7.1f %%\n",
        gPool->usedTotal * 100.0 / gPool->capTotal);
    eputs("-------------------------------------------------------"
          "\n");
    eprintf("*** File size                               = %7lu B\n",
        par->end - par->data);
    eprintf("*** Node size to file size ratio            = %7.1f x\n",
        gPool->usedTotal * 1.0 / (par->end - par->data));
    eputs("-------------------------------------------------------"
          "\n");
    eprintf("*** Space used for strings                  = %7u B\n",
        sPool->usedTotal);
    eprintf("*** Allocated for strings                   = %7u B\n",
        sPool->capTotal);
    eprintf("*** Space utilisation                       = %7.1f %%\n",
        sPool->usedTotal * 100.0 / sPool->capTotal);
    eputs("-------------------------------------------------------"
          "\n");
    eputs("\e[1mMemory-related calls\e[0m\n");
    eprintf("  calloc: %-7d | malloc: %-7d | realloc: %-7d\n",
        globals__callocCount, globals__mallocCount, globals__reallocCount);
    eprintf("  strlen: %-7d | strdup: %-7d |\n", globals__strlenCount,
        globals__strdupCount);

    eprintf("\e[1mTime elapsed:\e[0m %.1f ms (%.2f GB/s)\n", tms,
        1 / ((tms / 1e3) * 1e9 / (par->end - par->data))); // sw.print();

    return 0;
}
