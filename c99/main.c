#include "jet/base.h"

#define STEP 4

#include "types.h"
#include "token.h"

#define JOIN(x, y) x##y

#define NAME_CLASS(T) const char* JOIN(T, _typeName) = #T;

static const char* const spaces = //
    "                                                                     ";

#include "ast.h"

static TypeSpec* TypeSpec_new(TypeTypes tt, CollectionTypes ct) {
    TypeSpec* ret = NEW(TypeSpec);
    ret->typeType = tt;
    ret->collectionType = ct;
    return ret;
}

static const char* TypeSpec_name(TypeSpec* self) {
    switch (self->typeType) {
    case TYUnresolved: return self->name;
    case TYObject: return self->type->name;
    default: return TypeType_name(self->typeType);
    }
    // what about collectiontype???
}

// The name of this type spec as it will appear in the generated C code.
static const char* TypeSpec_cname(TypeSpec* self) {
    switch (self->typeType) {
    case TYUnresolved: return self->name;
    case TYObject: return self->type->name;
    default: return TypeType_name(self->typeType);
    }
    // what about collectiontype???
}

static const char* getDefaultValueForType(TypeSpec* type) {
    if (!type) return "";
    switch (type->typeType) {
    case TYUnresolved:
        unreachable(
            "unresolved: '%s' at %d:%d", type->name, type->line, type->col);
        return "ERROR_ERROR_ERROR";
    case TYString: return "\"\"";
    default: return "0";
    }
}

static Expr* Expr_fromToken(const Token* self) {
    Expr* ret = NEW(Expr);
    ret->kind = self->kind;
    ret->line = self->line;
    ret->col = self->col;

    ret->prec = TokenKind_getPrecedence(ret->kind);
    if (ret->prec) {
        ret->rassoc = TokenKind_isRightAssociative(ret->kind);
        ret->unary = TokenKind_isUnary(ret->kind);
    }

    exprsAllocHistogram[ret->kind]++;

    switch (ret->kind) {
    // case tkKeyword_cheater:
    case tkKeyword_for:
    case tkKeyword_while:
    case tkKeyword_if:
    case tkKeyword_end:
    case tkKeyword_enum:
    case tkKeyword_match:
    case tkKeyword_case:
    case tkKeyword_function:
    case tkKeyword_declare:
    case tkKeyword_test:
    case tkKeyword_check:
    case tkKeyword_not:
    case tkKeyword_notin:
    case tkKeyword_and:
    case tkKeyword_yes:
    case tkKeyword_no:
    case tkKeyword_nil:
    case tkKeyword_or:
    case tkKeyword_in:
    case tkKeyword_do:
    case tkKeyword_then:
    case tkKeyword_as:
    case tkKeyword_else:
    case tkKeyword_elif:
    case tkKeyword_type:
    case tkKeyword_return:
    case tkKeyword_result:
    case tkKeyword_extends:
    case tkKeyword_var:
    case tkKeyword_let:
    case tkKeyword_import:
    case tkIdentifier:
    case tkArgumentLabel:
    case tkFunctionCall:
    case tkSubscript:
    case tkObjectInit:
    case tkNumber:
    case tkString:
    case tkRawString:
    case tkRegexp:
    case tkMultiDotNumber:
    case tkLineComment: // Comments go in the Jet like regular stmts
        ret->string = self->pos;
        break;
    default:;
    }
    // the '!' will be trampled
    if (ret->kind == tkLineComment) ret->string++;
    // turn all 1.0234[DdE]+01 into 1.0234e+01.
    if (ret->kind == tkNumber) {
        CString_tr_ip_len(ret->string, 'd', 'e', self->matchlen);
        CString_tr_ip_len(ret->string, 'D', 'e', self->matchlen);
        CString_tr_ip_len(ret->string, 'E', 'e', self->matchlen);
    }
    return ret;
}

static bool Expr_throws(Expr* self) { // NOOO REMOVE This func and set the
                                      // throws flag recursively like the
    // other flags (during e.g. the type resolution dive)
    if (!self) return false;
    switch (self->kind) {
    case tkNumber:
    case tkMultiDotNumber:
    case tkRawString:
    case tkRegexp:
    case tkIdentifier:
    case tkIdentifierResolved:
    case tkString:
    case tkLineComment: return false;
    case tkFunctionCall:
    case tkFunctionCallResolved:
        return true; // self->func->throws;
        // actually  only if the func really throws
    case tkSubscript:
    case tkSubscriptResolved: return Expr_throws(self->left);
    case tkVarAssign: return self->var->used && Expr_throws(self->var->init);
    case tkKeyword_for:
    case tkKeyword_if:
    case tkKeyword_while: return false; // actually the condition could throw.
    default:
        if (!self->prec) return false;
        return Expr_throws(self->left) || Expr_throws(self->right);
    }
}

static size_t Scope_calcSizeUsage(Scope* self) {
    size_t size = 0, sum = 0, subsize = 0, maxsubsize = 0;
    // all variables must be resolved before calling this
    foreach (Expr*, stmt, self->stmts) {
        switch (stmt->kind) {
        case tkKeyword_if:
        case tkKeyword_else:
        case tkKeyword_for:
        case tkKeyword_while:
            subsize = Scope_calcSizeUsage(stmt->body);
            if (subsize > maxsubsize) maxsubsize = subsize;
            break;
        default:;
        }
    }
    // some vars are not assigned, esp. temporaries _1 _2 etc.
    foreach (Var*, var, self->locals) {
        size = TypeType_size(var->spec->typeType);
        if (!size)
            eprintf("warning: cannot find size for '%s' at %d:%d\n", var->name,
                var->line, var->col);
        if (var->used) sum += size;
    }
    // add the largest size among the sizes of the sub-scopes
    sum += maxsubsize;
    return sum;
}

static Var* Scope_getVar(Scope* self, const char* name) {
    // stupid linear search, no dictionary yet
    foreach (Var*, local, self->locals) //
        if (!strcasecmp(name, local->name)) return local;
    if (self->parent) return Scope_getVar(self->parent, name);
    return NULL;
}

static Var* Type_getVar(Type* self, const char* name) {
    // stupid linear search, no dictionary yet
    foreach (Var*, var, self->body->locals) //
        if (!strcasecmp(name, var->name)) return var;

    if (self->super && self->super->typeType == TYObject)
        return Type_getVar(self->super->type, name);
    return NULL;
}

#pragma mark - Jet FUNC IMPL.

/// This creates a new Func marked as declare and having one
/// argument. The name of the function and the type of the argument can be
/// specified. This way you can create declared functions such as `print`,
/// `json`, etc. of each new type defined in source code.
static Func* Func_createDeclWithArg(char* name, char* retType, char* arg1Type) {
    Func* func = NEW(Func);
    func->name = name;
    func->isDeclare = true;
    if (retType) {
        func->spec = NEW(TypeSpec);
        func->spec->name = retType;
    }
    if (arg1Type) {
        Var* arg = NEW(Var);
        arg->name = "arg1";
        arg->spec = NEW(TypeSpec);
        arg->spec->name = arg1Type;
        PtrList_append(&func->args, arg);
        func->argCount = 1;
    }
    return func;
}

static size_t Func_calcSizeUsage(Func* self) {
    size_t size = 0, sum = 0;
    foreach (Var*, arg, self->args) {
        // all variables must be resolved before calling this
        size = TypeType_size(arg->spec->typeType);
        assert(size);
        // if (arg->used)
        sum += size;
    }
    if (self->body) sum += Scope_calcSizeUsage(self->body);
    return sum;
}

///////////////////////////////////////////////////////////////////////////
static bool isCmpOp(Expr* expr) {
    return expr->kind == tkOpLE //
        || expr->kind == tkOpLT //
        || expr->kind == tkOpGT //
        || expr->kind == tkOpGE //
        || expr->kind == tkOpEQ //
        || expr->kind == tkOpNE;
}

///////////////////////////////////////////////////////////////////////////
static bool isBoolOp(Expr* expr) {
    return expr->kind == tkKeyword_and //
        || expr->kind == tkKeyword_or //
        || expr->kind == tkKeyword_in //
        || expr->kind == tkKeyword_notin //
        || expr->kind == tkKeyword_not;
}

static size_t Type_calcSizeUsage(Type* self) {
    size_t size = 0, sum = 0;
    foreach (Var*, var, self->body->locals) {
        // all variables must be resolved before calling this
        size = TypeType_size(var->spec->typeType);
        assert(size);
        sum += size;
    }
    return sum;
}

#pragma mark - Jet EXPR IMPL.
static TypeSpec* Expr_getObjectTypeSpec(const Expr* const self) {
    if (!self) return NULL;

    // all that is left is object
    switch (self->kind) {
    case tkFunctionCallResolved:
        return self->func->spec;
        // case tkIdentifier:
        // case tkSubscript:
    case tkIdentifierResolved:
    case tkSubscriptResolved:
        return self->var->spec;
        //        }
        // TODO: tkOpColon should be handled separately in the semantic
        // pass, and should be assigned either TYObject or make a dedicated
        // TYRange
        //     case tkOpColon:
        //        return "Range";
        // TODO: what else???
    case tkPeriod:
    case tkOpComma: return Expr_getObjectTypeSpec(self->right);
    default: break;
    }
    return NULL;
}

static Type* Expr_getEnumType(const Expr* const self) {
    Type* ret = NULL;
    if (!self || self->typeType != TYObject) return ret;
    // all that is left is object
    switch (self->kind) {
    case tkFunctionCallResolved: ret = self->func->spec->type; break;
    case tkIdentifierResolved:
    case tkSubscriptResolved:
        ret = self->var->spec->type;
        break;
        //        }
        // TODO: tkOpColon should be handled separately in the semantic
        // pass, and should be assigned either TYObject or make a
        // dedicated TYRange
        //     case tkOpColon:
        //        return "Range";
        // TODO: what else???
    case tkPeriod:
    case tkOpComma: ret = Expr_getEnumType(self->left);
    default: break;
    }
    if (ret && !ret->isEnum) ret = NULL;
    return ret;
}

static Type* Expr_getObjectType(const Expr* const self) {
    if (!self || self->typeType != TYObject) return NULL;

    // all that is left is object
    switch (self->kind) {
    case tkFunctionCallResolved: return self->func->spec->type;
    case tkIdentifierResolved:
    case tkSubscriptResolved:
        return self->var->spec->type;
        //        }
        // TODO: tkOpColon should be handled separately in the semantic
        // pass, and should be assigned either TYObject or make a dedicated
        // TYRange
        //     case tkOpColon:
        //        return "Range";
        // TODO: what else???
    case tkPeriod:
    case tkOpComma: return Expr_getObjectType(self->right);
    default: break;
    }
    return NULL;
}

static Type* Expr_getTypeOrEnum(const Expr* const self) {
    if (!self || self->typeType != TYObject) return NULL;

    // all that is left is object
    switch (self->kind) {
    case tkFunctionCallResolved: return self->func->spec->type;
    case tkIdentifierResolved:
    case tkSubscriptResolved:
        return self->var->spec->type;
        //        }
        // TODO: tkOpColon should be handled separately in the semantic
        // pass, and should be assigned either TYObject or make a dedicated
        // TYRange
        //     case tkOpColon:
        //        return "Range";
        // TODO: what else???
    case tkPeriod: {
        Type* type = Expr_getObjectType(self->left);
        if (!type->isEnum) type = Expr_getObjectType(self->right);
        return type;
    }
    case tkOpComma: return Expr_getTypeOrEnum(self->left);
    default: break;
    }
    return NULL;
}
// static CString* Expr_getTypeOrEnumName(const Expr* const self) {
//     Type* type = Expr_getTypeOrEnum(self);
//     return type ? type->name : "";
// }

static const char* Expr_typeName(const Expr* const self) {
    if (!self) return "";
    const char* ret = TypeType_name(self->typeType);
    if (!ret) return "<unknown>"; // unresolved
    if (*ret) return ret; // primitive type

    // all that is left is object
    switch (self->kind) {
    case tkFunctionCallResolved: return self->func->spec->type->name;
    case tkIdentifierResolved:
    case tkSubscriptResolved:
        return self->var->spec->type->name;
        //        }
        // TODO: tkOpColon should be handled separately in the semantic
        // pass, and should be assigned either TYObject or make a dedicated
        // TYRange
        //     case tkOpColon:
        //        return "Range";
        // TODO: what else???
    // case tkPeriod:
    //     return Expr_typeName(self->right);
    case tkPeriod: {
        Type* type = Expr_getObjectType(self->left);
        return (type->isEnum) ? type->name : Expr_typeName(self->right);
    }
    case tkOpComma: return Expr_typeName(self->left);
    default: break;
    }
    return "<invalid>";
}

static void Expr_catarglabels(Expr* self) {
    switch (self->kind) {
    case tkOpComma:
        Expr_catarglabels(self->left);
        Expr_catarglabels(self->right);
        break;
    case tkOpAssign: printf("_%s", self->left->string); break;
    default: break;
    }
}

static int Expr_strarglabels(Expr* self, char* buf, int bufsize) {
    int ret = 0;
    switch (self->kind) {
    case tkOpComma:
        ret += Expr_strarglabels(self->left, buf, bufsize);
        ret += Expr_strarglabels(self->right, buf + ret, bufsize - ret);
        break;
    case tkOpAssign:
        ret += snprintf(buf, bufsize, "_%s", self->left->string);
        break;
    default: break;
    }
    return ret;
}

// TODO: see if this is still correct
static int Expr_countCommaList(Expr* expr) {
    int i = 0;
    if (expr)
        for (i = 1; expr->right && expr->kind == tkOpComma; i++)
            expr = expr->right;
    return i;
}

#pragma mark - Jet MODULE IMPL.

static Type* Module_getType(Module* module, const char* name) {
    // the type may be "mm.XYZType" in which case you should look in
    // module mm instead. actually the caller should have bothered about
    // that.
    foreach (Type*, type, module->types) //
        if (!strcasecmp(type->name, name)) return type;
    // type specs must be fully qualified, so there's no need to look in
    // other modules.
    foreach (Type*, enu, module->enums) //
        if (!strcasecmp(enu->name, name)) return enu;
    return NULL;
}

// i like this pattern, getType, getFunc, getVar, etc.
// even the module should have getVar.
// you don't need the actual Import object, so this one is just a
// bool. imports just turn into a #define for the alias and an #include
// for the actual file.
monostatic Import* Module_getImportByAlias(Module* module, const char* alias) {
    foreach (Import*, imp, module->imports) //
    {
        eprintf("import: %s %s\n", imp->name + imp->aliasOffset, alias);
        if (!strcmp(imp->name + imp->aliasOffset, alias)) return imp;
    }
    return NULL;
}

monostatic Func* Module_getFuncByName(Module* module, const char* name) {
    foreach (Func*, func, module->funcs) //
        if (!strcasecmp(func->name, name)) return func;
    // This returns the first matching func only
    //  no looking anywhere else. If the name is of the form
    // "mm.func" you should have bothered to look in mm instead.
    return NULL;
}
monostatic Func* Module_getFunc(Module* module, const char* selector) {
    foreach (Func*, func, module->funcs) //
        if (!strcasecmp(func->selector, selector)) return func;
    //  no looking anywhere else. If the name is of the form
    // "mm.func" you should have bothered to look in mm instead.
    return NULL;
}

// only call this func if you have failed to resolve a func by getFunc(...).
monostatic Func* Module_getFuncByTypeMatch(Module* module, Expr* funcCallExpr) {
    foreach (Func*, func, module->funcs) {
        if (strcasecmp(funcCallExpr->string, func->name)) continue;
        if (Expr_countCommaList(funcCallExpr->left) != func->argCount) continue;
        // check all argument types to see if they match.
        Expr* currArg = funcCallExpr->left;
        foreach (Var*, arg, func->args) {
            if (!currArg) break;
            Expr* cArg = (currArg->kind == tkOpComma) ? currArg->left : currArg;
            if (cArg->kind == tkOpAssign) cArg = cArg->right;
            // __ this is why you need typeType and typeSubType so that
            // compatible types can be checked by equality ignoring subType.
            // The way it is now, CString and String wont match because they
            // arent strictly equal, although they are perfectly compatible
            // for argument passing.
            if (ISIN(3, cArg->typeType, TYUnresolved, TYErrorType, TYNoType))
                return NULL;
            if (cArg->typeType == arg->spec->typeType
                && cArg->collectionType == arg->spec->collectionType) {

                if (cArg->typeType == TYObject
                    && Expr_getTypeOrEnum(cArg) != arg->spec->type)
                    goto nextfunc;

            } else {
                // an arg type has failed to match. Wait: if it is unresolved,
                // you can still consider this func. Otherwise you skip to next.
                if (cArg->typeType != TYUnresolved) goto nextfunc;
            }
            currArg = (currArg->kind == tkOpComma) ? currArg->right : NULL;
        }
        return func;
    nextfunc:;
    }
    return NULL;
}
monostatic Var* Module_getVar(Module* module, const char* name) {
    foreach (Var*, var, module->scope->locals) //
        if (!strcasecmp(var->name, name)) return var;
    //  no looking anywhere else. If the name is of the form
    // "mm.func" you should have bothered to look in mm instead.
    return NULL;
}

#include "write.h"
#include "emit.h"
#include "dumpc.h"

#pragma mark - PARSER

typedef enum CompilerMode {
    PMTokenize, // just tokenize (debug only)
    PMLint, // format on stdout, ALL errors on stderr
    PMEmitC, // parse, stop on first error, or emit C
    // PMBuildO, // generate the object file
    PMMake, // run make and build the target
    PMRun, // run the executable in debug mode (build it first)
    PMTest // generate test code and run it
} CompilerMode;

static const char* CompilerMode__str[] = { //
    [PMTokenize] = "PMTokenize",
    [PMLint] = "PMLint",
    [PMEmitC] = "PMEmitC",
    [PMMake] = "PMMake",
    [PMRun] = "PMRun",
    [PMTest] = "PMTest"
};

// typedef enum DiagnosticSeverity {
//     DiagError,
//     DiagWarning,
//     DiagHint
// } DiagnosticSeverity;

// typedef struct Diagnostic {
//     unsigned short line;
//     unsigned char col;
//     DiagnosticSeverity severity : 8;
//     // DiagnosticKind kind : 8;
//     // DiagnosticEntity entity : 8;
//     union {
//         Type* type;
//         Func* func;
//         Var* var;
//         Expr* expr;
//     };
// } Diagnostic;

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
    List(Module) * modules;

    CompilerMode mode;
    // JetOpts opts;

    bool generateCommentExprs; // set to false when compiling, set to
                               // true when linting

    // set these whenever use is detected (e.g. during resolveTypes or parsing
    // literals)
    struct {
        bool complex : 1, json : 1, yaml : 1, xml : 1, html : 1, http : 1,
            ftp : 1, imap : 1, pop3 : 1, smtp : 1, frpc : 1, fml : 1, fbin : 1,
            rational : 1, polynomial : 1, regex : 1, datetime : 1, colour : 1,
            range : 1, table : 1, ui : 1;
    } requires;
} Parser;

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

static void Parser_fini(Parser* parser) {
    // free(parser->data);
    // free(parser->orig.ref[0]);
    // free(parser->noext);
    // free(parser->moduleName);
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
            // while (*cptr) {
            //     while (*cptr != '\n')
            //        cptr++ ;
            //     *cptr = 0; // trample the newline (in the original buffer)}
            // }
            // if (parser->token.line - 1 != parser->orig.used)
            //     unreachable("mismatched line: adding %d (at pos %d)",
            //         parser->token.line, parser->orig.used);

            // add it anyway
            PtrArray_push(&parser->orig, c + 1);
        }
    }
    // for_to(i, parser->orig.used)
    //     printf("%4d: %s\n", i + 1, parser->orig.ref[i]);
    // abort();
    return lines;
}

static Parser* Parser_fromFile(char* filename, bool skipws, CompilerMode mode) {
    size_t flen = CString_length(filename);

    // Error: the file might not end in .ch
    if (!CString_endsWith(filename, flen, ".jet", 4)) {
        eprintf("jet: file '%s' invalid: name must end in '.jet'.\n", filename);
        return NULL;
    }

    struct stat sb;

    // Error: the file might not exist
    if (stat(filename, &sb) != 0) {
        eprintf("jet: file '%s' not found.\n", filename);
        return NULL;
    } else if (S_ISDIR(sb.st_mode)) {
        // Error: the "file" might really be a folder
        eprintf("jet: '%s' is a folder; only files are accepted.\n", filename);
        return NULL;
    } else if (access(filename, R_OK) == -1) {
        // Error: the user might not have read permissions for the file
        eprintf("jet: no permission to read file '%s'.\n", filename);
        return NULL;
    }
    FILE* file = fopen(filename, "r");
    assert(file);

    Parser* ret = NEW(Parser);

    ret->filename = filename;
    // ret->noext = CString_noext(CString_clone(filename));
    fseek(file, 0, SEEK_END);
    const size_t size = ftell(file) + 2;

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
        // ret->noext = CString_noext(CString_clone(filename));
        // ret->moduleName = CString_tr(CString_clone(ret->noext), '/', '.');
        ret->data = data;
        // ret->mangledName = CString_tr(CString_clone(ret->noext), '/', '_');
        // ret->capsMangledName =
        // CString_upper(CString_clone(ret->mangledName));
        ret->end = ret->data + size - 2;
        ret->orig = (PtrArray) {};
        PtrArray_push(&ret->orig, strndup(data, size));
        // memcpy(ret->orig.ref[0], ret->data, size);
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
        // ret->nlines =
        recordNewlines(ret);
        // ^ make that ret->orig = CString_splitlines(Cstring_clone(ret->data));

        if (ret->orig.used > 65535) {
            eprintf("%s: error: too many lines (%u); limit is 65000\n",
                filename, ret->orig.used);
            Parser_fini(ret);
            ret = NULL;
        }

    } else {
        eprintf("%s: error: file with %zu MB exceeds 16 MB\n", filename,
            (size - 2) / 1024 / 1024);
    }

    // fclose(file);
    return ret;
}
static bool Parser_matches(Parser* parser, TokenKind expected);

#include "errors.h"
#include "stats.h"

#pragma mark - PARSING BASICS

static Expr* exprFromCurrentToken(Parser* parser) {
    Expr* expr = Expr_fromToken(&parser->token);
    Token_advance(&parser->token);
    return expr;
}

static Expr* next_token_node(
    Parser* parser, TokenKind expected, const bool ignore_error) {
    if (parser->token.kind == expected) {
        return exprFromCurrentToken(parser);
    } else {
        if (!ignore_error) Parser_errorExpectedToken(parser, expected);
        return NULL;
    }
}
// these should all be part of Token_ when converted back to C
// in the match case, self->token should be advanced on error
static Expr* Parser_match(Parser* parser, TokenKind expected) {
    return next_token_node(parser, expected, false);
}

// this returns the match node or null
static Expr* Parser_trymatch(Parser* parser, TokenKind expected) {
    return next_token_node(parser, expected, true);
}

// just yes or no, simple
static bool Parser_matches(Parser* parser, TokenKind expected) {
    return (parser->token.kind == expected);
}

static bool Parser_ignore(Parser* parser, TokenKind expected) {
    bool ret;
    if ((ret = Parser_matches(parser, expected))) Token_advance(&parser->token);
    return ret;
}

// this is same as match without return
static void Parser_consume(Parser* parser, TokenKind expected) {
    if (!Parser_ignore(parser, expected))
        Parser_errorExpectedToken(parser, expected);
}

static char* parseIdent(Parser* parser) {
    if (parser->token.kind != tkIdentifier)
        Parser_errorExpectedToken(parser, tkIdentifier);
    char* p = parser->token.pos;
    Token_advance(&parser->token);
    return p;
}

static void getSelector(Func* func) {
    if (func->argCount) {
        size_t selLen = 0;
        int remain = 128, wrote = 0;
        char buf[128];
        buf[127] = 0;
        char* bufp = buf;

        Var* arg1 = (Var*)func->args->item;
        wrote = snprintf(bufp, remain, "%s_", TypeSpec_name(arg1->spec));
        selLen += wrote;
        bufp += wrote;
        remain -= wrote;

        wrote = snprintf(bufp, remain, "%s", func->name);
        selLen += wrote;
        bufp += wrote;
        remain -= wrote;

        foreach (Var*, arg, func->args->next) {
            wrote = snprintf(bufp, remain, "_%s", arg->name);
            selLen += wrote;
            bufp += wrote;
            remain -= wrote;
        }
        // TODO: why not use pstrndup here?
        func->selector = CString_pndup(buf, selLen + 1);
        // func->selector = PoolB_alloc(strPool, selLen + 1);
        // memcpy(func->selector, buf, selLen + 1);

        bufp = buf;

        wrote = snprintf(bufp, remain, "%s(", func->name);
        selLen += wrote;
        bufp += wrote;
        remain -= wrote;

        //  arg1 = (Var*)func->args->item;
        wrote = snprintf(bufp, remain, "%s", TypeSpec_name(arg1->spec));
        selLen += wrote;
        bufp += wrote;
        remain -= wrote;

        foreach (Var*, arg, func->args->next) {
            wrote = snprintf(bufp, remain, ", %s", arg->name);
            selLen += wrote;
            bufp += wrote;
            remain -= wrote;
        }
        selLen += snprintf(bufp, 2, ")");
        func->prettySelector = CString_pndup(buf, selLen + 1);

    } else {
        func->selector = func->name;
        char buf[128];
        buf[127] = 0;
        int n = snprintf(buf, 127, "%s()", func->name);
        func->prettySelector = CString_pndup(buf, n + 1);
        ;
    }
}

#include "resolve.h"

// this is a global astexpr representing 0. it will be used when parsing e.g.
// the colon op with nothing on either side. : -> 0:0 means the same as 1:end
static Expr lparen[] = { { .kind = tkParenOpen } };
static Expr rparen[] = { { .kind = tkParenClose } };
static Expr expr_const_0[] = { { .kind = tkNumber, .string = "0" } };
static Expr expr_const_yes[] = { { .kind = tkKeyword_yes } };
static Expr expr_const_no[] = { { .kind = tkKeyword_no } };
static Expr expr_const_nil[] = { { .kind = tkKeyword_nil } };
static Expr expr_const_empty[] = { { .kind = tkString, .string = "" } };

#include "analyse.h"
#include "parse.h"

// TODO: this should be in Module open/close
static void Parser_emit_open(Parser* parser) {
    // printf("#ifndef HAVE_%s\n#define HAVE_%s\n\n", parser->capsMangledName,
    //     parser->capsMangledName);
    // printf("#define THISMODULE %s\n", parser->mangledName);
    printf("#define THISFILE \"%s\"\n", parser->filename);
    printf("#define NUMLINES %d\n", parser->token.line);
}

static void Parser_emit_close(Parser* parser) {
    // printf("#undef THISMODULE\n");
    printf("#undef THISFILE\n");
    // printf("#endif // HAVE_%s\n", parser->capsMangledName);
}

static void alloc_stat() { }

// #include "ptr2off.h"

#pragma mark - main

int main(int argc, char* argv[]) {
    // fprintf(stderr, banner, "v0.2.2", "asd762345asd", 2020, 9, 21);
    if (argc == 1) {
        eputs("jet: no input files. What are you trying to do?\n");
        return 1;
    }
    // bool printDiagnostics = (argc > 2 && *argv[2] == 'd') || false;

    clock_Time t0 = clock_getTime();

    // JetOpts opts[1];
    // if (!getOpts(argc, argv, opts)) return 3;

    // todo: mode 'x' must be replaced with 'd', 'r', 'z' for build modes
    // TODO: these short circuits should not be here but in the mode switch
    // below. When you are in compile or build mode check these to save some
    // repeated generation. When linting this has no effect, unless of course
    // you have dumped the Jet into a binary format and want to check for that.
    // if (!(needsBuild(opts->srcfile, 'x') || opts->forceBuild)) return 0;
    // if (!(needsBuild(opts->srcfile, 'o') || opts->forceBuild)) return 0;
    // if (!(needsBuild(opts->srcfile, 'c') || opts->forceBuild)) return 0;

    // initStaticExprs();
    CompilerMode mode = PMEmitC;
    bool stats = false;
    // if (argc > 3 && *argv[3] == 'l') mode = PMLint;
    char* filename = argv[1];
    if (argc > 2)
        if (*argv[2] == 'c' || *argv[2] == 'C')
            mode = PMEmitC, stats = (*argv[2] == 'C');
    if (argc > 2)
        if (*argv[2] == 'l' || *argv[2] == 'L')
            mode = PMLint, stats = (*argv[2] == 'L');
    if (argc > 2)
        if (*argv[2] == 't' || *argv[2] == 'T')
            mode = PMTest, stats = (*argv[2] == 'T');
    // if (argc > 3 && *argv[3] == 't') mode = PMTest;
    // if (argc > 2 && *argv[2] == 't') mode = PMTest;

    Parser* parser = Parser_fromFile(filename, true, mode);
    if (!parser) return 2;

    parser->issues.warnUnusedArg = //
        parser->issues.warnUnusedFunc = //
        parser->issues.warnUnusedType = //
        parser->issues.warnUnusedVar = 1;

    List(Module) * modules;

    // com lives for the duration of main. it appears in parseModule
    Module* root = parseModule(parser, &modules, NULL);

    if (parser->mode == PMLint) {
        if (parser->issues.hasParseErrors) {
            /* TODO: fallback to token-based linter (formatter)*/
        } else {
            foreach (Module*, mod, modules)
                Module_write(mod);
            foreach (Module*, mod, modules)
                Module_dumpc(mod);
        }
    } else if (!(parser->issues.errCount)) {
        switch (parser->mode) {

        case PMEmitC: {
            // TODO: if (monolithic) printf("#define function static\n");
            Parser_emit_open(parser);
            // ^ This is called before including the runtime, so that the
            // runtime can know THISFILE NUMLINES etc.
            printf("#include \"jet/runtime.h\"\n");
            foreach (Module*, mod, modules)
                Module_emit(mod);
            Parser_emit_close(parser);

        } break;

        case PMTest: {
            printf("#include \"jet/tester.h\"\n");
            // TODO : THISFILE must be defined since function callsites need
            // it, but the other stuff in Parser_emit_open isn't required.
            // Besides, THISFILE should be the actual module's file not the
            // test file

            foreach (Module*, mod, modules)
                Module_genTests(mod);
        } break;

        default: break;
        }
    }

    double elap = clock_clockSpanMicro(t0) / 1.0e3;
    if (stats) printstats(parser, elap);

    if (parser->issues.errCount) {
        eprintf("\n\e[31;1;4m THERE ARE %2d ERRORS.                        "
                "      "
                "                         \e[0m\n How about fixing them? "
                "Reading "
                "the code would be a good start.\n",
            parser->issues.errCount);
    }

    if (parser->issues.warnCount)
        eprintf("\n\e[33m*** %d warnings\e[0m\n", parser->issues.warnCount);

    // if (!printDiagnostics) eprintf("*** cgen in %.2f ms\n", elap);

    // returns the last error kind (from enum ParserErrors) so that test
    // scripts can check for specific errors raised.
    int ret = (parser->issues.errCount || _InternalErrs);
    // eprintf("ret: %d\n", ret);
    return ret; // or parser->warnCount);
    // TODO: what about last warning?
}
