
// #include <assert.h>
// #include <ctype.h>
// #include <limits.h>
// #include <stdio.h>
// #include <stdint.h>
// #include <stdlib.h>
// #include <string.h>
// #include <math.h>

// #include "cycle.h"
// #include "jet/base.h"
#include "List.hpp"
#include "jet/core/isin.h"
// #include "jet/os/clock.h"

#define STEP 4

#include "types.hpp"
#include "tokenKind.h"
#include "lexer.hpp"

#define JOIN(x, y) x##y

#define NAME_CLASS(T) const char* JOIN(T, _typeName) = #T;

static const char* const spaces = //
    "                                                                     ";

#pragma mark - AST TYPE DEFINITIONS

template <class T>
bool operator==(T& a, T& b) {
    // define specific == inside classes
    return &a == &b || memcmp(a, b, sizeof(T));
}

struct ASTLocation {
    uint32_t line : 24, col : 8;
};

struct Module;
struct ASTImport {
    char* name;
    struct Module* module;
    uint32_t aliasOffset, line : 16, col : 8, used : 1;
};

struct ASTUnits {
    uint8_t powers[7], something;
    double factor, factors[7];
    char* label;
};

struct Interval {
    double start, end;
};

struct ASTType;

struct ASTVar {
    char* name;
    union {
        ASTType* type;
        char* spec;
        Interval* intv;
        ASTUnits* units;
    };
    struct {
        uint32_t line : 24, col, : 8;
        uint16_t dims;
        CollectionTypes collectionType : 6;
        bool hasRange : 1, hasUnits : 1;
        TypeTypes typeType : 7;
        bool nullable : 1;
    };

    const char* typeName() {
        switch (typeType) {
        case TYUnresolved:
            return spec;
        case TYObject:
            return type->name;
        default:
            return ::name(typeType);
        }
    }
    const char* typeCName() {
        switch (typeType) {
        case TYUnresolved:
            return spec;
        case TYObject:
            return type->name;
        default:
            return ::name(typeType);
        }
    }
    const char* defaultValue() {
        // if (!type) return "";
        switch (typeType) {
        case TYUnresolved:
            unreachable("unresolved: '%s' at %d:%d", name, line, col);
            return "ERROR_ERROR_ERROR";
        case TYString:
            return "\"\"";
        case TYObject:
            return "NULL";
        default:
            return "0";
        }
    }
    ASTExpr* init;
    bool used : 1, changed : 1, isLet : 1, isVar : 1, isArg : 1, stackAlloc : 1,
        isTarget : 1, visited : 1, escapes : 1, canInplace : 1, isPromise : 1,
        hasRefs : 1, obtainedBySerialization : 1, usedAsIndex : 1,
        reassigned : 1, resized : 1, returned : 1;
};

struct ASTExpr {
    struct {
        union {
            struct {
                uint16_t typeType : 8, // typeType of this expression -> must
                                       // match for ->left and ->right
                    collectionType : 4, // collectionType of this expr -> the
                                        // higher dim-type of left's and right's
                                        // collectionType.
                    nullable : 1, // is this expr nullable (applies only when
                                  // typeType is object.) generally will be set
                                  // on idents and func calls etc. since
                                  // arithmetic ops are not relevant to objects.
                                  // the OR expr may unset nullable: e.g.
                                  // `someNullabeFunc(..) or MyType()` is NOT
                                  // nullable.
                    impure : 1, // is this expr impure, has side effects?
                                // propagates: true if if either left or right
                                // is impure.
                    elemental : 1, // whether this expr is elemental.
                                   // propagates: true if either left or right
                                   // is elemental.
                    throws : 1; // whether this expr may throw an error.
                                // propagates: true if either left or right
                                // throws.
            };
            uint16_t allTypeInfo; // set this to set everything about the type
        };
        uint16_t line;

        // blow this bool to bits to store more flags
        uint8_t dims : 5, // hack for now so you can set upto 32 dims. figure it
                          // out later how to have a common typeinfo struct
                          // between astexpr & astvar
            promote : 1, // should this expr be promoted, e.g.
                         // count(arr[arr<34]) or sum{arr[3:9]}. does not
                         // propagate.
            canEval : 1, // the value is known (computable) at compile time,
                         // either by jetc or by backend cc.
            didEval : 1; //
        uint8_t prec : 6, // operator precedence for this expr
            unary : 1, // for an operator, is it unary (negation, not,
                       // return, check, array literal, ...)
            rassoc : 1; // is this a right-associative operator e.g.
                        // exponentiation
        uint8_t col;
        TokenKind kind : 8;
    };
    union {
        ASTExpr* left;
        List<ASTVar&>* vars; // for tkString
        ASTType* elementType; // for tkListLiteral, tkDictLiteral only!!
    };
    union {
        // ASTEvalInfo eval;
        struct {
            uint32_t hash, slen;
        }; // str len for strings, idents, unresolved funcs/vars etc.
        // why do you need the hash in the expr? just add to the dict using the
        // computed hash! TO COMPUTE HASH OF EXPR TREES
    };
    union {
        char* string;
        double real;
        int64_t integer;
        uint64_t uinteger;
        // char* name; // for idents or unresolved call or subscript
        ASTFunc* func; // for functioncall
        ASTVar* var; // for array subscript, or a tkVarAssign
        ASTScope* body; // for if/for/while
        ASTExpr* right;
        ASTImport* import; // for imports tkKeyword_import
    };
    // TODO: the code motion routine should skip over exprs with
    // promote=false this is set for exprs with func calls or array
    // filtering etc...
    bool isCmpOp() {
        return ISIN(6, kind, tkOpLE, tkOpLT, tkOpGT, tkOpGE, tkOpEQ, tkOpNE);
    }

    ///////////////////////////////////////////////////////////////////////////
    bool isBoolOp() {
        return ISIN(5, kind, tkKeyword_and, tkKeyword_or, tkKeyword_in,
            tkKeyword_notin, tkKeyword_not);
    }
    bool isSelfMutOp() {
        return ISIN(7, kind, tkPlusEq, tkMinusEq, tkSlashEq, tkTimesEq,
            tkPowerEq, tkOpModEq, tkOpAssign);
    }

    bool isArithOp() {
        return ISIN(12, kind, tkPlusEq, tkMinusEq, tkSlashEq, tkTimesEq,
            tkPowerEq, tkOpModEq, tkPlus, tkMinus, tkSlash, tkTimes, tkPower,
            tkOpMod);
    }

    ASTType* getEnumType() {
        ASTType* r = NULL;
        if (typeType != TYObject) return r;
        // all that is left is object
        switch (kind) {
        case tkFunctionCallResolved:
            r = ret->type;
            break;
        case tkIdentifierResolved:
        case tkSubscriptResolved:
            r = var->type;
            break; //        }
                   // TODO: tkOpColon should be handled separately in the
                   // semantic pass, and should be assigned either TYObject or
                   // make a dedicated TYRange
                   //     case tkOpColon:
                   //        return "Range";
                   // TODO: what else???
        case tkPeriod:
        case tkOpComma:
            r = getEnumType(left);
        default:
            break;
        }
        if (r && !r->isEnum) r = NULL;
        return r;
    }

    ASTType* getObjectType() {
        if (typeType != TYObject) return NULL;

        // all that is left is object
        switch (kind) {
        case tkFunctionCallResolved:
            return func.returnSpec->type;
        case tkIdentifierResolved:
        case tkSubscriptResolved:
            return var->type;
            //        }
            // TODO: tkOpColon should be handled separately in the semantic
            // pass, and should be assigned either TYObject or make a dedicated
            // TYRange
            //     case tkOpColon:
            //        return "Range";
            // TODO: what else???
        case tkPeriod:
        case tkOpComma:
            return getObjectType(right);
        default:
            break;
        }
        return NULL;
    }

    ASTType* getTypeOrEnum() {
        if (typeType != TYObject) return NULL;

        // all that is left is object
        switch (kind) {
        case tkFunctionCallResolved:
            return func.returnSpec->type;
        case tkIdentifierResolved:
        case tkSubscriptResolved:
            return var->type;
            //        }
            // TODO: tkOpColon should be handled separately in the semantic
            // pass, and should be assigned either TYObject or make a dedicated
            // TYRange
            //     case tkOpColon:
            //        return "Range";
            // TODO: what else???
        case tkPeriod: {
            ASTType* type = getObjectType(left);
            if (!type->isEnum) type = getObjectType(right);
            return type;
        }
        case tkOpComma:
            return getTypeOrEnum(left);
        default:
            break;
        }
        return NULL;
    }

    const char* typeName() {
        // if (!self) return "";
        const char* ret = name(typeType);
        if (!ret) return "<unknown>"; // unresolved
        if (*ret) return ret; // primitive type

        // all that is left is object
        switch (kind) {
        case tkFunctionCallResolved:
            return ret->type->name;
        case tkIdentifierResolved:
        case tkSubscriptResolved:
            return var->typeName();
            //        }
            // TODO: tkOpColon should be handled separately in the semantic
            // pass, and should be assigned either TYObject or make a dedicated
            // TYRange
            //     case tkOpColon:
            //        return "Range";
            // TODO: what else???
        // case tkPeriod:
        //     return  typeName( right);
        case tkPeriod: {
            ASTType* type = getObjectType(left);
            return (type->isEnum) ? type->name : right.typeName();
        }
        case tkOpComma:
            return left.typeName();
        default:
            break;
        }
        return "<invalid>";
    }

    void catarglabels() {
        switch (kind) {
        case tkOpComma:
            catarglabels(left);
            catarglabels(right);
            break;
        case tkOpAssign:
            printf("_%s", left->string);
            break;
        default:
            break;
        }
    }

    int strarglabels(char* buf, int bufsize) {
        int ret = 0;
        switch (kind) {
        case tkOpComma:
            ret += strarglabels(left, buf, bufsize);
            ret += strarglabels(right, buf + ret, bufsize - ret);
            break;
        case tkOpAssign:
            ret += snprintf(buf, bufsize, "_%s", left->string);
            break;
        default:
            break;
        }
        return ret;
    }

    // TODO: see if this is still correct
    int countCommaList() {
        int i = 0;
        ASTExpr* expr = this;
        for (i = 1; expr->right && expr->kind == tkOpComma; i++)
            expr = expr->right;
        return i;
    }
};

struct ASTScope {
    List<ASTExpr&>* stmts;
    List<ASTVar&>* vars;
    ASTScope* parent;

    // still space left
    size_t size() {
        size_t size = 0, sum = 0, subsize = 0, maxsubsize = 0;
        // all variables must be resolved before calling this
        for (ASTExpr& stmt : *stmts) {
            switch (stmt.kind) {
            case tkKeyword_if:
            case tkKeyword_else:
            case tkKeyword_for:
            case tkKeyword_elif:
            case tkKeyword_case:
            case tkKeyword_while:
                subsize = stmt.body.size();
                if (subsize > maxsubsize) maxsubsize = subsize;
                break;
            default:;
            }
        }
        // some vars are not assigned, esp. temporaries _1 _2 etc.
        for (ASTVar& var : *vars) {
            size = size(var.typeType);
            if (!size)
                eprintf("warning: cannot find size for '%s' at %d:%d\n",
                    var.name, var.line, var.col);
            if (var.used) sum += size;
        }
        // add the largest size among the sizes of the sub-scopes
        sum += maxsubsize;
        return sum;
    }

    ASTVar* var(const char* name) {
        // stupid linear search, no dictionary yet
        for (ASTVar& var : *vars) //
            if (!strcasecmp(name, var.name)) return &var;
        if (parent) return parent->var(name);
        return NULL;
    }
};

struct ASTType {
    char* name;
    /// [unused] supertype. Jet does not have inheritance, perhaps for good.
    // ASTTypeSpec* super;
    /// The other types that are used in this type (i.e. types of member
    /// variables)
    List<ASTType&>* usedTypes;
    /// The other types that use this type.
    List<ASTType&>* usedByTypes;
    /// The body of the type, as a scope. In effect it can have any expressions,
    /// but most kinds are disallowed by the parsing routine. Variable
    /// declarations and invariant checks are what you should mostly expect to
    /// see inside type bodies, not much else.
    ASTScope& body;
    uint16_t line;
    uint8_t col;
    bool analysed : 1, needJSON : 1, needXML : 1, needYAML : 1, visited : 1,
        isValueType : 1, isEnum : 1,
        isDeclare : 1; // all vars of this type will be stack
                       // allocated and passed around by value.

    size_t size() {
        size_t size = 0, sum = 0;
        for (ASTVar& var : *body.vars) {
            // all variables must be resolved before calling this
            size = size(var.typeType);
            assert(size);
            sum += size;
        }
        return sum;
    }
    ASTVar* member(const char* name) {
        // stupid linear search, no dictionary yet
        for (ASTVar& var : *body.vars) //
            if (!strcasecmp(name, var.name)) return &var;

        // if (super && super->typeType == TYObject)
        //     return ASTType_getVar(super->type, name);
        return NULL;
    }
};

// typedef struct ASTEnum {
//     char* name;
//     ASTScope* body;
//     uint16_t line;
//     uint8_t col;
//     bool analysed : 1, visited : 1;
// } ASTEnum;

struct ASTFunc {
    char* name;
    ASTScope& body;
    List<ASTVar&>* args;
    List<ASTVar&>*callers, *callees;
    ASTVar* ret;

    char *selector, *prettySelector;
    struct {
        uint16_t line;
        struct {
            uint16_t throws : 1,
                isRecursive : 1, // usesNet : 1,usesIO : 1, usesGUI : 1,
                mutator : 1, // usesSerialisation : 1, //
                isExported : 1, //
                usesReflection : 1, //
                nodispatch : 1, //
                isStmt : 1, //
                isDeclare : 1, //
                isCalledFromWithinLoop : 1, //
                elemental : 1, //
                isDefCtor : 1, //
                intrinsic : 1, // intrinsic: print, describe, json, etc. not to
                               // be output by linter
                analysed : 1, // semantic pass has been done, don't repeat
                isCalledAsync : 1, // is this func called async at least once?
                returnsNewObjectSometimes : 1,
                returnsNewObjectAlways : 1; // what this func returns is an
                                            // object that was obtained by a
                                            // constructor. Useful for checking
                                            // cycles in types.
            // Constructors ALWAYS return a new object. This means if you call a
            // constructor of a type from within the default constructor of
            // another type, and this chain has a cycle, you need to report
            // error. If this happens indirectly via intermediate funcs, check
            // the returnsNewObject flag of the func in question to see if it
            // internally calls the constructor. The function may have multiple
            // return paths and not all of them may call the constructor; in
            // this case set returnsNewObjectAlways accordingly.
        };
        uint8_t argCount, nameLen;
    };

    static ASTFunc* createDeclWithArg(
        char* name, char* retType, char* arg1Type) {
        ASTFunc* func = NEW(ASTFunc);
        func.name = name;
        func.isDeclare = true;
        if (retType) {
            func.returnSpec = NEW(ASTTypeSpec);
            func.returnSpec->name = retType;
        }
        if (arg1Type) {
            ASTVar* arg = NEW(ASTVar);
            arg->name = "arg1";
            arg->typeSpec = NEW(ASTTypeSpec);
            arg->name = arg1Type;
            PtrList_append(&func.args, arg);
            func.argCount = 1;
        }
        return func;
    }

    void makeSelector() {
        if (argCount) {
            size_t selLen = 0;
            int remain = 128, wrote = 0;
            char buf[128];
            buf[127] = 0;
            char* bufp = buf;

            wrote = snprintf(bufp, remain, "%s", name);
            selLen += wrote;
            bufp += wrote;
            remain -= wrote;

            if (args->count()) {
                ASTVar& arg1 = *args->begin();
                wrote = snprintf(bufp, remain, "_%s", arg1.typeName());
                selLen += wrote;
                bufp += wrote;
                remain -= wrote;
            }

            // if (args->count() > 1) {
            for (auto& arg : *args) {
                if (arg == args->first()) continue;
                wrote = snprintf(bufp, remain, "_%s", arg.name);
                selLen += wrote;
                bufp += wrote;
                remain -= wrote;
            }
            // }
            // TODO: why not use pstrndup here?
            selector = CString_pndup(buf, selLen + 1);
            // func.selector = PoolB_alloc(strPool, selLen + 1);
            // memcpy(func.selector, buf, selLen + 1);

            bufp = buf;

            wrote = snprintf(bufp, remain, "%s(", name);
            selLen += wrote;
            bufp += wrote;
            remain -= wrote;

            arg1 = (ASTVar*)func.args->item;
            wrote = snprintf(bufp, remain, "%s", arg1.typeName());
            selLen += wrote;
            bufp += wrote;
            remain -= wrote;

            for (ASTVar& arg : *args) {
                if (arg == args->first()) continue;
                wrote = snprintf(bufp, remain, ", %s", arg.name);
                selLen += wrote;
                bufp += wrote;
                remain -= wrote;
            }
            selLen += snprintf(bufp, 2, ")");
            prettySelector = CString_pndup(buf, selLen + 1);

        } else {
            selector = name;
            char buf[128];
            buf[127] = 0;
            int n = snprintf(buf, 127, "%s()", name);
            prettySelector = CString_pndup(buf, n + 1);
            ;
        }
    }
    size_t sizeUsage() {
        size_t size = 0, sum = 0;
        for (ASTVar& arg : args) {
            // all variables must be resolved before calling this
            size = size(arg->typeType);
            assert(size);
            // if (arg->used)
            sum += size;
        }
        sum += body.sizeUsage();
        return sum;
    }
};

struct ASTTest {
    char* name;
    ASTScope& body;
    char* selector;
    struct {
        uint16_t line;
        struct {
            uint16_t analysed : 1;
        } flags;
    };
};

struct Module {

    ASTScope scope[1];
    List<ASTFunc&>* funcs;
    List<ASTTest&>* tests;
    List<ASTType&>*types, *enums;
    List<ASTImport&>* imports;
    List<Module&>* importedBy;
    char *name, *fqname, *filename;

    DiagnosticReporter reporter;

    struct {
        bool complex : 1, json : 1, yaml : 1, xml : 1, html : 1, http : 1,
            ftp : 1, imap : 1, pop3 : 1, smtp : 1, frpc : 1, fml : 1, fbin : 1,
            rational : 1, polynomial : 1, regex : 1, datetime : 1, colour : 1,
            range : 1, table : 1, gui : 1;
    } requires;

    ASTType* type(const char* name) {
        for (ASTType& type : *types)
            if (!strcasecmp(type.name, name)) return &type;
        for (ASTType& enu : *enums)
            if (!strcasecmp(enu.name, name)) return &enu;
        return NULL;
    }

    ASTImport* import(const char* alias) {
        for (ASTImport& imp : *imports)
            if (!strcmp(imp.name + imp.aliasOffset, alias)) return &imp;
    }

    ASTFunc* funcNamed(const char* name) {
        for (ASTFunc& func : *funcs)
            if (!strcasecmp(func.name, name)) return &func;
        return NULL;
    }
    ASTFunc* func(const char* selector) {
        for (ASTFunc& func : *funcs)
            if (!strcasecmp(func.selector, selector)) return &func;
        return NULL;
    }
};

// better keep a set or map instead and add as you encounter in code
// or best do nothing and let user write 'import formats' etc
// typedef struct {
//     int need_BitVector : 1, need_Colour : 1, need_Currency : 1,
//         need_DateTime : 1, need_DiskItem : 1, need_Duration : 1,
//         need_Number : 1, need_Range : 1, need_Rational : 1, need_Regex : 1,
//         need_Size : 1, need_String : 1, need_YesOrNo : 1, need_Array : 1,
//         need_ArrayND : 1, need_Dict : 1, need_Filter : 1, need_List : 1,
//         need_Selection : 1, need_Sequence : 1, need_SequenceND : 1,
//         need_Slice : 1, need_SliceND : 1, need_FML : 1, need_HTML : 1,
//         need_JSON : 1, need_XML : 1, need_YAML : 1, need_FTP : 1, need_HTTP :
//         1, need_IMAP : 1, need_POP3 : 1, need_SMTP : 1, need_SSH : 1,
//         need_Pool : 1;
// } FPNeedBuiltins;

// #pragma mark - AST IMPORT IMPL.

// #pragma mark - AST UNITS IMPL.

// struct ASTTypeSpec;
// struct ASTType;
// struct ASTFunc;
// struct ASTScope;
// struct ASTExpr;
// struct ASTVar;

// #define List_ASTExpr PtrList
// #define List_ASTVar PtrList
// #define List_Module PtrList
// #define List_ASTFunc PtrList
// #define List_ASTEnum PtrList
// #define List_ASTTest PtrList
// #define List_ASTType PtrList
// #define List_ASTImport PtrList
// #define List_ASTScope PtrList

MKSTAT(ASTExpr)
MKSTAT(ASTFunc)
MKSTAT(ASTTest)
MKSTAT(ASTEnum)
MKSTAT(ASTTypeSpec)
MKSTAT(ASTType)
MKSTAT(Module)
MKSTAT(ASTScope)
MKSTAT(ASTImport)
MKSTAT(ASTVar)
MKSTAT(Parser)
MKSTAT(List_ASTExpr)
MKSTAT(List_ASTFunc)
MKSTAT(List_ASTEnum)
MKSTAT(List_ASTTest)
MKSTAT(List_ASTType)
MKSTAT(List_Module)
MKSTAT(List_ASTScope)
MKSTAT(List_ASTImport)
MKSTAT(List_ASTVar)
static uint32_t exprsAllocHistogram[128];

// static ASTTypeSpec* ASTTypeSpec_new(TypeTypes tt, CollectionTypes ct) {
//     ASTTypeSpec* ret = NEW(ASTTypeSpec);
//     ret->typeType = tt;
//     ret->collectionType = ct;
//     return ret;
// }

#pragma mark - AST FUNC IMPL.

/// This creates a new ASTFunc marked as declare and having one
/// argument. The name of the function and the type of the argument can be
/// specified. This way you can create declared functions such as `print`,
/// `json`, etc. of each new type defined in source code.

///////////////////////////////////////////////////////////////////////////

#pragma mark - AST EXPR IMPL.

#pragma mark - AST MODULE IMPL.

#include "lint.hpp"
#include "emit.hpp"

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

typedef enum DiagnosticSeverity {
    DiagError,
    DiagWarning,
    DiagHint
} DiagnosticSeverity;

typedef struct Diagnostic {
    unsigned short line;
    unsigned char col;
    DiagnosticSeverity severity : 8;
    DiagnosticKind kind : 8;
    DiagnosticEntity entity : 8;
    union {
        ASTType* type;
        ASTFunc* func;
        ASTVar* var;
        ASTExpr* expr;
    };
} Diagnostic;

typedef struct DiagnosticReporter {
    uint16_t errCount, warnCount, errLimit;
    uint8_t lastError /*enum type*/, warnUnusedVar : 1, warnUnusedFunc : 1,
        warnUnusedType : 1, warnUnusedArg : 1, hasParseErrors : 1;
    Array<const char*>* lines; // holds lines of original source for error
                               // reports // can be smallarray
    Array<Diagnostic> issues; // holds diagnostics can be smallarray
} DiagnosticReporter;

typedef struct Compiler {

    moveto module char *data, *end; //, *noext;

    Token token; // current

    List(Module) * modules;

    JetOpts opts;

    bool generateCommentExprs; // set to false when compiling, set to
                               // true when linting
} Compiler;

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

static void fini(Compiler& com) {
    // free(com->data);
    // free(com->orig.ref[0]);
    // free(com->noext);
    // free(com->moduleName);
    // free(com->mangledName);
    // free(com->capsMangledName);
}
#define FILE_SIZE_MAX 1 << 24

long recordNewlines(Compiler& com) {
    // push a new entry to get hold of the current source line later
    // this is the pointer in the original (unmodified) buffer
    char* cptr = com->orig.ref[0];
    char* cend = cptr + (com->end - com->data);
    // long lines = 1;
    for (char* c = cptr; c < cend; c++) {
        if (*c == '\n') {
            *c = 0;
            // lines++;
            // while (*cptr) {
            //     while (*cptr != '\n')
            //        cptr++ ;
            //     *cptr = 0; // trample the newline (in the original buffer)}
            // }
            // if (com->token.line - 1 != com->orig.used)
            //     unreachable("mismatched line: adding %d (at pos %d)",
            //         com->token.line, com->orig.used);

            // add it anyway
            PtrArray_push(&com->orig, c + 1);
        }
    }
    // for_to(i, com->orig.used)
    //     printf("%4d: %s\n", i + 1, com->orig.ref[i]);
    // abort();
    return lines;
}

static Parser* fromFile(const char* filename, bool skipws, CompilerMode mode) {

    FILE* file = fopen(filename, "r");
    if (!file) return NULL;

    Parser* ret = NULL;
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
        ret->data = data;
        // ret->moduleName = CString_tr(CString_clone(ret->noext), '/', '.');
        // ret->mangledName = CString_tr(CString_clone(ret->noext), '/', '_');
        // ret->capsMangledName =
        // CString_upper(CString_clone(ret->mangledName));
        ret->end = ret->data + size - 2;
        ret->orig = (PtrArray) {};
        PtrArray_push(&ret->orig, strndup(data, size));
        // memcpy(ret->orig.ref[0], ret->data, size);
        ret->token = (Lexer) { //
            .pos = ret->data,
            .skipWhiteSpace = skipws,
            .mergeArrayDims = false,
            .kind = tkUnknown,
            .line = 1,
            .col = 1
        };
        ret->issues = (DiagnosticReporter) { .errLimit = 50000 };
        ret->mode = mode;
        ret->generateCommentExprs = (ret->mode == PMLint);

        // If you ar not linting, even a single error is enough to stop and tell
        // the user to LINT THE DAMN FILE FIRST.
        if (ret->mode != PMLint) ret->issues.errLimit = 1;
        // ret->nlines =
        recordNewlines(ret);
        // ^ make that ret->orig = CString_splitlines(Cstring_clone(ret->data));

        if (ret->orig.used > 65535) {
            eprintf("%s: error: too many lines (%lu); limit is 65000\n",
                filename, ret->orig.used);
            fini(ret);
            ret = NULL;
        }

    } else {
        eprintf("%s: error: file with %zu MB exceeds 16 MB\n", filename,
            (size - 2) / 1024 / 1024);
    }

    // fclose(file);
    return ret;
}
static bool matches(Compiler& com, TokenKind expected);

#include "errors.hpp"
#include "stats.hpp"

#pragma mark - PARSING BASICS

static char* parseIdent(Compiler& com) {
    if (com->token.kind != tkIdentifier)
        errorExpectedToken(parser, tkIdentifier);
    char* p = com->token.pos;
    Token_advance(&com->token);
    return p;
}

#include "resolve.hpp"

// this is a global astexpr representing 0. it will be used when parsing e.g.
// the colon op with nothing on either side. : -> 0:0 means the same as 1:end
static ASTExpr lparen[] = { { .kind = tkParenOpen } };
static ASTExpr rparen[] = { { .kind = tkParenClose } };
static ASTExpr expr_const_0[] = { { .kind = tkNumber, .string = "0" } };
static ASTExpr expr_const_yes[] = { { .kind = tkKeyword_yes } };
static ASTExpr expr_const_no[] = { { .kind = tkKeyword_no } };
static ASTExpr expr_const_nil[] = { { .kind = tkKeyword_nil } };
static ASTExpr expr_const_empty[] = { { .kind = tkString, .string = "" } };

#include "analyse.hpp"
#include "parse.hpp"

static void alloc_stat() { }

// #include "ptr2off.hpp"

#pragma mark - main

typedef struct {
    CompilerMode mode;
    bool stats, coverage, profiling, buildobj, inplace, strictSpacing,
        disableGC, forceBuild;
    const char *srcfile, *makeMode;
    long nprocs, nthreads;
} JetOpts;

static void getDefaultOpts(JetOpts* out) {
    *out = (JetOpts) { .mode = PMRun, .makeMode = "debug", .buildobj = true };
}
void printOpts(JetOpts* opts) {
    static const char* _fp_yn[] = { "no", "yes" };
    printf("mode = %s\n", CompilerMode__str[opts->mode]);
    printf("stats = %s\n", _fp_yn[opts->stats]);
    printf("coverage = %s\n", _fp_yn[opts->coverage]);
    printf("profiling = %s\n", _fp_yn[opts->profiling]);
    printf("buildobj = %s\n", _fp_yn[opts->buildobj]);
    printf("inplace = %s\n", _fp_yn[opts->inplace]);
    printf("strictSpacing = %s\n", _fp_yn[opts->strictSpacing]);
    printf("disableGC = %s\n", _fp_yn[opts->disableGC]);
    printf("forceBuild = %s\n", _fp_yn[opts->forceBuild]);
    printf("srcfile = %s\n", opts->srcfile);
    printf("makeMode = %s\n", opts->makeMode);
    printf("nprocs = %ld\n", opts->nprocs);
    printf("nthreads = %ld\n", opts->nthreads);
}

static bool getOpts(int argc, char* argv[], JetOpts* out) {
    getDefaultOpts(out);
    bool ok = true;
    for (int i = 1; i < argc; i++) {
        if (!argv[i]) continue; // may have been clobbered by -o
        char* arg = argv[i];
        char lookahead = 1, *value;
        if (*arg == '-' && *(arg + 1)) {
            while (lookahead && lookahead != ' ') {
                // you can do e.g. -abcde to activate all
                if (!*++arg) break;
                lookahead = *(arg + 1);
                value = arg + 2;
                switch (*arg) {
                case 'c': // only generate .c file, stop at first error
                    out->mode = PMEmitC;
                    break;
                case 'w':
                    out->buildobj = false;
                    break;
                case 'v':
                    out->coverage = true;
                    break;
                case 'p':
                    out->profiling = true;
                    break;
                case 's':
                    out->stats = true;
                    break;
                case 'l':
                    out->mode = PMLint;
                    break;
                case 'k': // just print tokens (for debugging only).
                    out->mode = PMTokenize;
                    break;
                case 't':
                    out->mode = PMTest;
                    break;
                case 'b': // make specified target (or everything)
                    out->mode = PMMake;
                    break;
                case 'r':
                    out->makeMode = "release";
                    break;
                case 'z':
                    out->makeMode = "fast";
                    break;
                case 'f':
                    out->forceBuild = true;
                    break;
                case 'i': // in-place update linted file(s)
                    out->inplace = true;
                    break;
                case 'x':
                    out->disableGC = false;
                    break;
                case 'n':
                    if (i + 1 < argc && '-' != *argv[i + 1]) {
                        out->nprocs = atoi(argv[i + 1]);
                        i++;
                    } else {
                        eprintf(
                            "jetc: missing value for argument #%d (-n)\n", i);
                    }
                    break;
                case 'm':
                    if (i + 1 < argc && '-' != *argv[i + 1]) {
                        out->nthreads = atoi(argv[i + 1]);
                        i++;
                    } else {
                        eprintf(
                            "jetc: missing value for argument #%d (-m)\n", i);
                    }
                    break;
                default:
                defaultCase:
                    eprintf("jetc: unrecognized option #%d: %s [%s]\n", i,
                        argv[i], arg);
                    ok = false;
                }
            }
        } else {
            // jetc supports only ONE file at a time?
            out->srcfile = arg;
        }
    }

    if (!out->srcfile) {
        eputs("jet: error: no input file specified\n");
        ok = false;
    } else if (!strcmp(out->srcfile, "-")) {
        // this is STDIN...
    } else {
        size_t flen = CString_length(out->srcfile);
        if (!CString_endsWith(out->srcfile, flen, ".jet", 4)) {
            eprintf("%s: error: filename invalid, must end in '.jet'\n",
                out->srcfile);
            ok = false;
        }

        struct stat sb;
        if (stat(out->srcfile, &sb) != 0) {
            eprintf("%s: error: file not found\n", out->srcfile), ok = false;
        } else if (S_ISDIR(sb.st_mode)) {
            eprintf("%s: error: that's a folder\n", out->srcfile), ok = false;
        } else if (access(out->srcfile, R_OK) == -1) {
            eprintf("%s: error: permission denied\n", out->srcfile), ok = false;
        }
    }
    printOpts(out);

    for (int i = 0; i < argc; i++) printf("%s ", argv[i]);
    puts("");

    return ok;
}

static const unsigned long ONE_NANO = 1000000000;

// Check if the corresponding .c, .o, or .x file exists for a given source file
// and whether it is older than the source file. `file` is any Jet source file,
// `newext` is either 'c', 'o', or 'x'.
bool needsBuild(const char* file, char newext) {
    struct stat sb, sbc;

    printf("checking %s\n", file);

    int ret = stat(file, &sb);
    if (ret) return false; // unreachable
    size_t filetime
        = sb.st_mtimespec.tv_sec * ONE_NANO + sb.st_mtimespec.tv_nsec;

    static char cfile[4096] = {}; //, *cend = cfile + 4095;
    size_t n = strlen(file);
    assert(n < 4095);
    strncpy(cfile, file, 4094); // one for NULL, one for a dot to hide the file

    // char* lastSlash = cfile + n;
    // while (lastSlash > cfile && lastSlash[-1] != '/') lastSlash--;

    char* mover = cfile + n;
    while (--mover > cfile && mover[-1] != '/') mover[0] = mover[-1];
    mover[0] = '.';

    char* lastDot = cfile + n;
    while (lastDot > cfile && lastDot[-1] != '.') lastDot--;
    cfile[n - 3] = '.';
    cfile[n - 2] = newext; // change file.jet to .file.c
    cfile[n - 1] = 0;

    // size_t alen = strlen(altext);
    // assert(alen < cend - lastDot);
    // strncpy(lastDot, altext, alen);

    // printf("checking %s\n", cfile);

    if (stat(cfile, &sbc))
        return true; // yes it needs a build if it doesn't exist

    size_t cfiletime
        = sbc.st_mtimespec.tv_sec * ONE_NANO + sbc.st_mtimespec.tv_nsec;

    return cfiletime < filetime;
}

const int sz = sizeof(ASTExpr) + sizeof(ASTType) + sizeof(ASTFunc)
    + sizeof(Module) + sizeof(ASTVar) + sizeof(ASTImport);

int main(int argc, char* argv[]) {
    clock_Time t0 = clock_getTime();

    JetOpts opts[1];
    if (!getOpts(argc, argv, opts)) return 3;

    // todo: mode 'x' must be replaced with 'd', 'r', 'z' for build modes
    // TODO: these short circuits should not be here but in the mode switch
    // below. When you are in compile or build mode check these to save some
    // repeated generation. When linting this has no effect, unless of course
    // you have dumped the AST into a binary format and want to check for that.
    if (!(needsBuild(opts->srcfile, 'x') || opts->forceBuild)) return 0;
    if (!(needsBuild(opts->srcfile, 'o') || opts->forceBuild)) return 0;
    if (!(needsBuild(opts->srcfile, 'c') || opts->forceBuild)) return 0;

    Compiler& com = Compiler_withOptions(opts);

    = fromFile(opts->srcfile, !opts->strictSpacing, opts->mode);
    if (!parser) {
        eprintf("%s: error: could not read file\n", opts->srcfile);
        return 2;
    }

    // com lives for the duration of main. it appears in parseModule
    Module* root = parseModule(com, com->opts.srcfile);

    if (com->opts.mode == PMLint) {
        if (com->issues.hasParseErrors) {
            /* TODO: fallback to token-based linter (formatter)*/
        } else {
            for (Module& mod : modules) lint(mod);
        }
    } else if (!(com->issues.errCount)) {
        switch (com->mode) {
            // case PMLint: {
            //     foreach(Module*, mod, modules) Module_lint(mod, 0);
            // } break;

        case PMEmitC: {
            // TODO: if (monolithic) printf("#define function static\n");
            emit_open(parser);
            // ^ This is called before including the runtime, so that the
            // runtime can know THISFILE NUMLINES etc.
            printf("#include \"jet/runtime.h\"\n");
            for (Module& mod : modules) emit(mod);
            emit_close(parser);

        } break;

        case PMTest: {
            printf("#include \"jet/tester.h\"\n");
            // TODO : THISFILE must be defined since function callsites need
            // it, but the other stuff in  emit_open isn't required.
            // Besides, THISFILE should be the actual module's file not the
            // test file

            for (Module& mod : modules) genTests(mod);
        } break;

        default:
            break;
        }
    }

    double elap = clock_clockSpanMicro(t0) / 1.0e3;
    if (opts->stats) printstats(parser, elap);

    if (com->issues.errCount) {
        eprintf("\n\e[31;1;4m THERE ARE %2d ERRORS.                        "
                "      "
                "                         \e[0m\n How about fixing them? "
                "Reading "
                "the code would be a good start.\n",
            com->issues.errCount);
    }

    if (com->issues.warnCount)
        eprintf("\n\e[33m*** %d warnings\e[0m\n", com->issues.warnCount);

    // if (!printDiagnostics) eprintf("*** cgen in %.2f ms\n", elap);

    // returns the last error kind (from enum ParserErrors) so that test
    // scripts can check for specific errors raised.
    int ret = (com->issues.errCount || _InternalErrs);
    // eprintf("ret: %d\n", ret);
    return ret; // or com->warnCount);
    // TODO: what about last warning?
}
