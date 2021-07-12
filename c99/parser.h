
typedef struct IssueMgr {
  char* filename;
  uint16_t errCount, warnCount, errLimit;
  uint8_t lastError /*enum type*/, warnUnusedVar : 1, warnUnusedFunc : 1,
    warnUnusedType : 1, warnUnusedArg : 1, hasParseErrors : 1;
} IssueMgr;

typedef struct Parser {
  char* filename; // mod/submod/xyz/mycode.ch
  // char* moduleName; // mod.submod.xyz.mycode
  // char* mangledName; // mod_submod_xyz_mycode
  // char* capsMangledName; // MOD_SUBMOD_XYZ_MYCODE
  char *data, *end;
  // char* noext;
  PtrArray orig; // holds lines of original source for error reports

  Token token; // current
  IssueMgr issues;
  List(Module) * mods;

  CompilerMode mode;
  // JetOpts opts;

  double elap, oelap, elap_tot;

  bool generateCommentExprs; // set to false when compiling, set to
                             // true when linting
  bool godMode;              //(only for me) inline C using unaryminus, etc

  // set these whenever use is detected (e.g. during resolveTypes or parsing
  // literals)
  struct {
    bool complex : 1, json : 1, yaml : 1, xml : 1, html : 1, http : 1,
      ftp : 1, imap : 1, pop3 : 1, smtp : 1, frpc : 1, fml : 1, fbin : 1,
      rational : 1, polynomial : 1, regex : 1, datetime : 1, colour : 1,
      range : 1, table : 1, ui : 1;
  } requires;
} Parser;
MKSTAT(Parser)

Parser* Parser_new_() {
  IFDEBUG(_allocTotal_Parser++);
  return Pool_alloc(gPool, sizeof(struct Parser));
}
// static const int sgr = sizeof(Compiler);

// #define STR(x) STR_(x)
// #define STR_(x) #x

// static const char* const banner = //
//     "________     _____  _\n"
//     "______(_)______  /_ _|  The next-gen language of computing\n"
//     "_____  /_  _ \\  __/ _|  %s %s %4d-%02d-%02d\n"
//     "____  / /  __/ /_  __|\n"
//     "___  /  \\___/\\__/ ___|  https://github.com/jetpilots/jet\n"
//     "/___/ ______________________________________________________\n\n";

static void par_fini(Parser* parser) {
  free(parser->data);
  free(parser->orig.ref[0]);
  // free(parser->noext);
  // free(parser->modName);
  // free(parser->mangledName);
  // free(parser->capsMangledName);
}
#define FILE_SIZE_MAX 1 << 24

long recordNewlines(Parser* parser) {
  // push a new entry to get hold of the current source line later
  // this is the pointer in the original (unmodified) buffer
  char* cptr = parser->orig.ref[0];
  char* cend = cptr + (parser->end - parser->data);
  long lines = 1;
  for (char* c = cptr; c < cend; c++) {
    if (*c == '\n') {
      *c = 0;
      lines++;
      arr_push(&parser->orig, c + 1);
    }
  }
  return lines;
}

static Parser* par_fromFile(
  char* filename, bool skipws, CompilerMode mode) {
  size_t flen = cstr_len(filename);

  // Error: the file might not end in .jet
  if (!cstr_endsWith(filename, flen, ".jet", 4)) {
    // char* newname
    filename = cstr_interp_s(256, "%s.jet", filename);
    // char fbuf[256];
    // snprintf(fbuf,254,"%s.jet",filename);
    // fbuf[255]=0;
    // eprintf("cjet: invalid filename:\n"
    //         "      %s\n"
    //         "looking instead for:\n"
    //         "      %s.jet\n",
    //     filename);
    // return NULL;
  }

  struct stat sb;
  if (stat(filename, &sb) != 0) { // the file might not exist
    eprintf("jet: file '%s' not found.\n", filename);
    return NULL;
  } else if (S_ISDIR(sb.st_mode)) { // might really be a folder
    eprintf("jet: '%s' is a folder; only files are accepted.\n", filename);
    return NULL;
  } else if (access(filename, R_OK) == -1) { // permissions for the file
    eprintf("jet: no permission to read file '%s'.\n", filename);
    return NULL;
  }

  FILE* file = fopen(filename, "r");
  assert(file);
  fseek(file, 0, SEEK_END);
  const size_t size = ftell(file) + 2;

  Parser* ret = NULL;
  // 2 null chars, so we can always lookahead
  if (size < FILE_SIZE_MAX) {
    char* data = malloc(size);
    data[size - 1] = 0;
    data[size - 2] = 0;

    fseek(file, 0, SEEK_SET);
    if (fread(data, size - 2, 1, file) != 1) {
      eprintf("jet: error: file '%s' could not be read\n", filename);
      fclose(file);
      return NULL;
      // would leak if ret was malloc'd directly, but we have a pool
    }
    fclose(file);

    ret = NEW(Parser);
    ret->filename = filename;
    ret->data = data;
    ret->end = ret->data + size - 2;
    ret->orig = (PtrArray) {};
    ret->godMode = 1;

    arr_push(&ret->orig, strndup(data, size));
    ret->token = (Token) { //
      .pos = ret->data,
      .skipWhiteSpace = skipws,
      .mergeArrayDims = false,
      .kind = tkUnknown,
      .line = 1,
      .col = 1
    };
    ret->issues = (IssueMgr) { .errLimit = 50000 };
    ret->mode = mode;
    ret->generateCommentExprs = (ret->mode == PMLint);

    // If you ar not linting, even a single error is enough to stop and tell
    // the user to LINT THE DAMN FILE FIRST.
    if (ret->mode != PMLint) ret->issues.errLimit = 1;
    recordNewlines(ret);

    if (ret->orig.used > 65535) {
      eprintf("%s: error: too many lines (%u); limit is 65000\n", filename,
        ret->orig.used);
      par_fini(ret);
      ret = NULL;
    }
  } else {
    eprintf("%s: error: file with %zu MB exceeds 16 MB limit\n", filename,
      (size - 2) / 1024 / 1024);
  }
  return ret;
}

static Parser* par_fromStdin(bool skipws, CompilerMode mode) {

  FILE* file = stdin;

  size_t bread = 0;

  char* data = malloc(64 KB);
  String str = {};
  do {
    bread = fread(data, 1, 64 KB, file);
    String_appendChars(&str, data, bread);
  } while (bread == 64 KB);
  String_appendChars(&str, "\0\0", 2);

  Parser* ret = NEW(Parser);
  ret->filename = "stdin";
  ret->data = str.ref;
  ret->end = ret->data + str.len - 2;
  ret->orig = (PtrArray) {};
  ret->godMode = 1;

  arr_push(&ret->orig, strndup(data, str.len + 1));
  ret->token = (Token) { //
    .pos = ret->data,
    .skipWhiteSpace = skipws,
    .mergeArrayDims = false,
    .kind = tkUnknown,
    .line = 1,
    .col = 1
  };
  ret->issues = (IssueMgr) { .errLimit = 50000 };
  ret->mode = mode;
  ret->generateCommentExprs = (ret->mode == PMLint);

  // If you ar not linting, even a single error is enough to stop and tell
  // the user to LINT THE DAMN FILE FIRST.
  if (ret->mode != PMLint) ret->issues.errLimit = 1;
  recordNewlines(ret);

  if (ret->orig.used > 65535) {
    eprintf("stdin: error: too many lines (%u); limit is 65000\n",
      ret->orig.used);
    par_fini(ret);
    ret = NULL;
  }
  if (str.len >= 1 << 24) {
    eprintf("stdin: error: input %d B exceeds 16 MB limit\n", str.len);
    par_fini(ret);
    ret = NULL;
  }
  return ret;
}

static bool par_matches(Parser* parser, TokenKind expected);
