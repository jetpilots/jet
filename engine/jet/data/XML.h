
// No #includes in headers

#ifndef HAVE_JET_BASE
#include "../../../jet/modules/base.h"
#include "../../../jet/modules/sys_time.h"
#endif

typedef struct xml_attr_t xml_attr_t;
typedef struct xml_node_t xml_node_t;
typedef struct xml_parser_t xml_parser_t;
static const int istep = 2;

jet_static const char* const spaces
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
struct xml_attr_t {
    const char* key;
    const char* val;
};

struct xml_node_t {
    const char* tag;
    list_t(xml_attr_t) * attributes;
    union {
        const char* text;
        list_t(xml_node_t) * children;
    };
}; // not keeping line/col here, keep them on the parent stack along with ptr.
// for HTML you need them in the node, since validation could print err msgs.

struct xml_parser_t {
    const char* filename;
    int line, col;
    char *data, *end;
    char* pos;
};

// this MKSTAT should not go here, but in runtime
MKSTAT(xml_node_t)
MKSTAT(xml_attr_t)
MKSTAT(xml_parser_t)

jet_static xml_node_t* xml_node_new(const char* tag) {
    xml_node_t* ret = NEW(xml_node_t);
    ret->tag = tag;
    return ret;
}

jet_static xml_node_t* xml_node_newText(const char* text) {
    xml_node_t* ret = NEW(xml_node_t);
    ret->tag = NULL;
    ret->text = text;
    return ret;
}

jet_static xml_attr_t* xml_attr_new(const char* key, const char* value) {
    xml_attr_t* ret = NEW(xml_attr_t);
    ret->key = key;
    ret->val = value;
    return ret;
}

jet_static xml_parser_t* xml_parser_fromStringClone(const char* str) {
    xml_parser_t* par = NEW(xml_parser_t);
    size_t len = CString_length(str);
    par->data = pstrndup(str, len);
    par->pos = par->data;
    par->end = par->data + len;
    par->line = 1;
    return par;
}

jet_static xml_parser_t* xml_parser_fromFile(char* filename) {
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

    xml_parser_t* ret = NEW(xml_parser_t);

    ret->filename = filename;
    fseek(file, 0, SEEK_END);
    const size_t size = ftell(file);

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

jet_static bool isAnyOf(char ch, char* chars) {
    while (*chars)
        if (*chars++ == ch) return true;
    return false;
}

jet_static const char* findchars_fast(const char* buf, const char* buf_end,
    const char* chars, size_t chars_size) {
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
                break;
            }
            buf += 16;
            left -= 16;
        } while (likely(left != 0));
    }
    return buf;
#else
    return buf + strcspn(buf, chars);
#endif
}

jet_static list_t(xml_attr_t) * xml_parser_parseAttrs(xml_parser_t* parser) {
    list_t(xml_attr_t)* list = NULL;
    list_t(xml_attr_t)** listp = &list;

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
        xml_attr_t* attr = xml_attr_new(name, value);
        listp = list_push(listp, attr);
    }
    //    if (*parser->pos == '/') *parser->pos++ = 0;
    //    assert(*parser->pos == '>');
    //    *parser->pos++ = 0;
    // don't consume ending />, leave it at that
    return list;
}

jet_static list_t(xml_node_t) * xml_parser_parseTags(xml_parser_t* parser) {
    list_t(xml_node_t)* list = NULL;
    list_t(xml_node_t)** listp = &list;

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
                xml_node_t* node = xml_node_new(parser->pos);
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
                    node->attributes = xml_parser_parseAttrs(parser);
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
                        node->children = xml_parser_parseTags(parser);

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

                listp = list_push(listp, node);
            }
        } break;

        default:

        {
            char* text = parser->pos;
            while (*parser->pos != '<' and parser->pos < parser->end) {
                if (*parser->pos == '\n') {
                    parser->line++;
                    parser->col = 0;
                }
                parser->pos++;
            }
            xml_node_t* textNode = xml_node_newText(text);
            listp = list_push(listp, textNode);
        }
        }
    }
    if (parser->pos < parser->end) printf("error: data unparsed\n");
    return list;
}

jet_static void fml_attr_print(xml_attr_t* attr, int indent) {
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

jet_static void xml_attr_print(xml_attr_t* attr, int indent) {
    printf(" %s=\"%s\"", attr->key, attr->val);
}
jet_static void xml_node_print(xml_node_t* node, int indent);
jet_static void xml_node_list_print(list_t(xml_node_t) * nodeList, int indent) {
    foreach (xml_node_t*, childNode, nodeList)
        xml_node_print(childNode, indent);
}
jet_static void fml_node_print(xml_node_t* node, int indent, bool skipws);
jet_static void fml_node_list_print(
    list_t(xml_node_t) * nodeList, int indent, bool skipws) {
    foreachn(xml_node_t*, childNode, li, nodeList) {
        fml_node_print(childNode, indent, skipws);
        if (li->next) printf("\n%.*s", indent, spaces);
    }
}
jet_static void xml_node_print(xml_node_t* node, int indent) {
    if (node->tag) {
        printf("%.*s<%s%s", indent, spaces, node->tag,
            node->attributes     ? ""
                : node->children ? ">\n"
                                 : "/>\n");
        foreach (xml_attr_t*, attr, node->attributes)
            xml_attr_print(attr, indent);
        if (node->attributes) printf("%s\n", node->children ? ">" : " />");
        xml_node_list_print(node->children, indent + 2);
        if (node->children) printf("%.*s</%s>\n", indent, spaces, node->tag);
    } else {
        printf("%.*s%s\n", indent, spaces, node->text);
    }
}

jet_static void fml_str_print(const char* str, int indent, bool skipws) {
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
        case '`': printf("\\`"); break;
        case '&':
        default: putc(*c, stdout);
        }
        c++;
    }
    printf("`");
}
jet_static void fml_attr_list_print(
    list_t(xml_attr_t) * attributes, int indent) {
    foreach (xml_attr_t*, attr, attributes) //
        if (!strcmp(attr->key, "id")) {
            printf(" #%s", attr->val);
            break;
        }

    foreach (xml_attr_t*, attr, attributes) //
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

    foreach (xml_attr_t*, attr, attributes)
        fml_attr_print(attr, indent);
}

jet_static void fml_node_print(xml_node_t* node, int indent, bool skipws) {
    if (node->tag) {
        printf("%s", node->tag);

        fml_attr_list_print(node->attributes, indent);
        // if (node->attributes)
        // PARENT MUST PRINT INDENTATION if you want to collapse single-child
        // tags with no attributes like div > div > div and so on.
        bool skipw = strcmp(node->tag, "script") && strcmp(node->tag, "style");
        if (node->attributes or (node->children && node->children->next)) {
            if (node->children) {
                printf("\n%.*s", indent + istep, spaces);
                fml_node_list_print(node->children, indent + istep, skipw);
            }
        } else {
            if (node->children) {
                printf(" > ");
                fml_node_list_print(node->children, indent + istep, skipw);
            }
        } // if (node->children) printf("%.*s</%s>\n", indent, spaces,
          // node->tag);
    } else {
        // TODO: split the node->text by newlines and indent each line
        // this always needs to be quoted!!!!

        fml_str_print(node->text, indent, skipws);
        // printf("%.*s`%s`\n", indent, spaces, node->text);
    }
}

#include "XML.h"

int main(int argc, char* argv[]) {
    if (argc < 2) {
        printf("usage: %s <filename>\n", argv[0]);
        exit(1);
    }

    sys_time_Time t0;
    xml_parser_t* par = xml_parser_fromFile(argv[1]);
    double tms;

    t0 = sys_time_getTime();
    list_t(xml_node_t)* parsed = xml_parser_parseTags(par);
    if (argc > 2 && *argv[2] == 'x') xml_node_list_print(parsed, 0);
    if (argc > 2 && *argv[2] == 'f') fml_node_list_print(parsed, 0, 0);

    tms = sys_time_clockSpanMicro(t0) / 1.0e3;

    eputs("-------------------------------------------------------"
          "\n");
    allocstat(xml_attr_t);
    allocstat(xml_parser_t);
    allocstat(xml_node_t);
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
