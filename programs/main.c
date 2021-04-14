
// #include <assert.h>
// #include <ctype.h>
// #include <limits.h>
// #include <stdio.h>
// #include <stdint.h>
// #include <stdlib.h>
// #include <string.h>
// #include <math.h>

// #include "cycle.h"
#include "jet/base.h"
// #include "jet/os/clock.h"

#define STEP 4

#include "types.h"
#include "tokenKind.h"
#include "token.h"

#define JOIN(x, y) x##y

#define NAME_CLASS(T) const char* JOIN(T, _typeName) = #T;

static const char* const spaces = //
    "                                                                     ";

#pragma mark - AST TYPE DEFINITIONS

typedef struct ASTLocation {
    uint32_t line : 24, col : 8;
} ASTLocation;

struct ASTModule;
typedef struct ASTImport {
    char* name; //, *alias;
    struct ASTModule* module;
    uint32_t aliasOffset, line : 16, col : 8, used : 1;
    // bool isPackage, hasAlias;
} ASTImport;

typedef struct ASTUnits {
    uint8_t powers[7], something;
    double factor, factors[7];
    char* label;
} ASTUnits;

typedef struct {
    double start, end;
} Interval;
// TODO: replace this with the generic RealRange

// TODO: somewhere in the typespec or in the ASTVar need a flag refCounted
typedef struct ASTTypeSpec {
    union {
        struct ASTType* type;
        char* name;
        Interval* intv;
        // ^ e.g. var x in [1:250]
        // this does not make x integer. it only provides a constraint, you need
        // other clues to decide if x should really be integer.
        // e.g. var x in [1:1:256] -> now its an integer
        ASTUnits* units;
        // not keeping units on vars; move them to exprs instead. that way | is
        // simply a check/multiply op. in tthe AST | is a tkDimensionedExpr, or
        // simply every expr has a ASTUnits* member.
    };
    struct { // todo: this must be named TypeInfo & reused in ASTExpr not copy
             // pasted
        uint16_t dims; // more than 65535 dims will not be handled at compile
                       // time (size check, shape check etc) but at runtime. if
                       // collectionType is tensor but dims is 0, it means too
                       // many dims or ct-unknown dims, in any case it is then
                       // the generic ArrayND.
        CollectionTypes collectionType : 6;
        bool hasRange : 1, //
            hasUnits : 1;
        TypeTypes typeType : 7;
        bool nullable : 1;
    };
    ASTLocation loc[0];
    uint32_t line : 24, col, : 8;
} ASTTypeSpec;

typedef struct ASTVar {
    char* name;
    ASTTypeSpec* typeSpec;
    union {
        struct ASTExpr* init;
        // when you move to having return var, there wont be a returnSpec on
        // funcs anymore, only on vars. then you can get rid of asttypespec and
        // move its stuff here. When there is an init, you take typeinfo from
        // the expr, else directly here. check lower dword of the init ptr to be
        // null to know that init is null. playing with fire, but its safe play
        // imho. well its all for later.
        /* struct {
            unsigned _init_lower32;
            struct {
                uint16_t dims;
                CollectionTypes collectionType : 6;
                bool hasRange : 1, hasUnits : 1;
                TypeTypes typeType : 7;
                bool nullable : 1;
            };
        }; */
    };
    // List(ASTVar*) deps; // TODO: keep deps of each var so you can tell when a
    // dependency of an async var is changed before the async is awaited. First
    // you will have to figure out how to analyze array members and type members
    // (e.g. init an instance for each compound type and array for each array
    // and then set the member of that as dep...)
    // struct ASTExpr* lastUsed; //
    // last expr in owning scope that refers to this var. Note that you should
    // dive to search for the last expr, it may be within an inner scope. The
    // drop call should go at the end of such subscope, NOT within the subscope
    // itself after the actual expr. (so set the if/for/while as the lastref,
    // not the actual lastref) WHY NOT JUST SAVE THE LINE NUMBER OF THE LAST
    // USE?
    ASTLocation loc[0];
    uint32_t line : 24, col : 8; //
    uint16_t lastUsage, used, changed;
    // ^ YOu canot use the last used line no to decide drops etc. because code
    // motion can rearrange statements and leave the line numbers stale.
    // --- YES YOU CAN if you use == to compare when dropping and not >=. Also
    // for multiline exprs you should save the toplevel expr line and not the
    // line of the actual tkIdentifierResolved.
    struct {
        bool //
            isLet : 1, //
            isVar : 1, //
            isArg : 1, // a function arg, not a local var
            stackAlloc : 1, // this var is of a ref type, but it will be
                            // stack allocated. it will be passed around by
                            // reference as usual.
            isTarget : 1, // x = f(x,y)
            visited : 1, // for generating checks, used to avoid printing this
                         // var more than once.
            escapes : 1, // does it escape the owning SCOPE?
            canInplace : 1,
            isPromise : 1, // is it an async var (transparently a Promise<T>)?
            hasRefs : 1, // there are other vars/lets that reference this var or
                         // overlap with its storage. e.g. simply pointers that
                         // refer to this var, or slices or filters taken if
                         // this is an array/dataframe etc, or StringRefs, etc.
                         // useful for understanding aliasing patterns
                         // I think you rather need refCount which can be kept
                         // at compile time because inplacing decisions etc.
                         // need surety of var having no other refs outside the
                         // scope
            obtainedBySerialization : 1, // this is a JSON/XML/YAML obj obtained
                                         // by serializing something. If it is
                                         // passed to functions, warn and
                                         // recommend passing the object instead
                                         // and calling JSON/XML/YAML in that
                                         // func. This is so that calls like
                                         // print(YAML(xyz)) can be optim to
                                         // print_YAML(xyz) (i.e. not generating
                                         // an actual YAML tree just for print)
            usedAsIndex : 1, // if ys, should be converted to  a pointer rather
                             // than offset, and arr[b] should be converted to
                             // *b to avoid a + op.
                             // someone has to pay the price somewhere: if you
                             // e.g. print the loop variable user expects to see
                             // the offset, so there you have to -.
                             // generated loops can be done w/ ptrs
                             // e.g. a[:,:] = random()
            reassigned : 1, // this var was reassigned after init. If it is a
                            // non-primitive type, it generally implies this
                            // should be generated as a pointer.
            resized : 1, // this collection var was resized after init (due to
                         // resize(), push() etc.) and means that it cannot be
                         // generated as a fixed size array (static if size is
                         // known at compile time).
            returned : 1; // is a return variable ie. b in
        // function asd(x as Anc) returns (b as Whatever)
        // all args (in/out) are in the same list in func -> args.
    };
    // uint8_t col;
} ASTVar;

// when does something escape a scope?
// -- if it is assigned to a variable outside the scope
//    -- for func toplevels, one such var is the return var: anything
//    assigned to it escapes
// -- if it is passed to a func as an arg and the arg `escapes`
//    analyseExpr sets `escapes` on each arg as it traverses the func

typedef struct ASTExpr {
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
        struct ASTExpr* left;
        List(ASTVar*) * vars; // for tkString
        struct ASTType* elementType; // for tkListLiteral, tkDictLiteral only!!
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
        struct ASTFunc* func; // for functioncall
        struct ASTVar* var; // for array subscript, or a tkVarAssign
        struct ASTScope* body; // for if/for/while
        struct ASTExpr* right;
        struct ASTImport* import; // for imports tkKeyword_import
    };
    // TODO: the code motion routine should skip over exprs with
    // promote=false this is set for exprs with func calls or array
    // filtering etc...
} ASTExpr;

typedef struct ASTScope {
    List(ASTExpr) * stmts;
    List(ASTVar) * locals;
    struct ASTScope* parent;
    bool isLoop; // this affects drops: loop scopes cannot drop parent vars.
    // still space left
} ASTScope;

typedef struct ASTType {
    char* name;
    /// [unused] supertype. Jet does not have inheritance, perhaps for good.
    ASTTypeSpec* super;
    /// The other types that are used in this type (i.e. types of member
    /// variables)
    List(ASTType) * usedTypes;
    /// The other types that use this type.
    List(ASTType) * usedByTypes;
    /// The body of the type, as a scope. In effect it can have any expressions,
    /// but most kinds are disallowed by the parsing routine. Variable
    /// declarations and invariant checks are what you should mostly expect to
    /// see inside type bodies, not much else.
    ASTScope* body;
    uint16_t line;
    uint8_t col;
    bool analysed : 1, needJSON : 1, needXML : 1, needYAML : 1, visited : 1,
        isValueType : 1, isEnum : 1,
        isDeclare : 1; // all vars of this type will be stack
                       // allocated and passed around by value.
} ASTType;

// typedef struct ASTEnum {
//     char* name;
//     ASTScope* body;
//     uint16_t line;
//     uint8_t col;
//     bool analysed : 1, visited : 1;
// } ASTEnum;

typedef struct ASTFunc {
    char* name;
    ASTScope* body;
    List(ASTVar) * args;
    List(ASTFunc) * callers, *callees;
    ASTTypeSpec* returnSpec;
    char *selector, *prettySelector;
    struct {
        uint16_t line, used;
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
} ASTFunc;

typedef struct ASTTest {
    char* name;
    ASTScope* body;
    char* selector;
    struct {
        uint16_t line;
        struct {
            uint16_t analysed : 1;
        } flags;
    };
} ASTTest;

typedef struct ASTModule {

    ASTScope scope[1]; // global scope contains vars + exprs
    List(ASTFunc) * funcs;
    List(ASTTest) * tests;
    // List(ASTExpr) * exprs; // global exprs
    List(ASTType) * types, *enums;
    // List(ASTVar) * vars; // global vars
    List(ASTImport) * imports;
    // List(ASTType) * enums;
    List(ASTModule) * importedBy; // for dependency graph. also use
                                  // imports[i]->module over i
    char *name, *fqname, *filename;

    // DiagnosticReporter reporter;

    struct {
        bool complex : 1, json : 1, yaml : 1, xml : 1, html : 1, http : 1,
            ftp : 1, imap : 1, pop3 : 1, smtp : 1, frpc : 1, fml : 1, fbin : 1,
            rational : 1, polynomial : 1, regex : 1, datetime : 1, colour : 1,
            range : 1, table : 1, gui : 1;
    } requires;
} ASTModule;

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

#pragma mark - AST IMPORT IMPL.

#pragma mark - AST UNITS IMPL.

struct ASTTypeSpec;
struct ASTType;
struct ASTFunc;
struct ASTScope;
struct ASTExpr;
struct ASTVar;

#define List_ASTExpr PtrList
#define List_ASTVar PtrList
#define List_ASTModule PtrList
#define List_ASTFunc PtrList
#define List_ASTEnum PtrList
#define List_ASTTest PtrList
#define List_ASTType PtrList
#define List_ASTImport PtrList
#define List_ASTScope PtrList

MKSTAT(ASTExpr)
MKSTAT(ASTFunc)
MKSTAT(ASTTest)
MKSTAT(ASTEnum)
MKSTAT(ASTTypeSpec)
MKSTAT(ASTType)
MKSTAT(ASTModule)
MKSTAT(ASTScope)
MKSTAT(ASTImport)
MKSTAT(ASTVar)
MKSTAT(Parser)
MKSTAT(List_ASTExpr)
MKSTAT(List_ASTFunc)
MKSTAT(List_ASTEnum)
MKSTAT(List_ASTTest)
MKSTAT(List_ASTType)
MKSTAT(List_ASTModule)
MKSTAT(List_ASTScope)
MKSTAT(List_ASTImport)
MKSTAT(List_ASTVar)
static uint32_t exprsAllocHistogram[128];

static ASTTypeSpec* ASTTypeSpec_new(TypeTypes tt, CollectionTypes ct) {
    ASTTypeSpec* ret = NEW(ASTTypeSpec);
    ret->typeType = tt;
    ret->collectionType = ct;
    return ret;
}

static const char* ASTTypeSpec_name(ASTTypeSpec* self) {
    switch (self->typeType) {
    case TYUnresolved: return self->name;
    case TYObject: return self->type->name;
    default: return TypeType_name(self->typeType);
    }
    // what about collectiontype???
}

// The name of this type spec as it will appear in the generated C code.
static const char* ASTTypeSpec_cname(ASTTypeSpec* self) {
    switch (self->typeType) {
    case TYUnresolved: return self->name;
    case TYObject: return self->type->name;
    default: return TypeType_name(self->typeType);
    }
    // what about collectiontype???
}

static const char* getDefaultValueForType(ASTTypeSpec* type) {
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

static ASTExpr* ASTExpr_fromToken(const Token* self) {
    ASTExpr* ret = NEW(ASTExpr);
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
    case tkLineComment: // Comments go in the AST like regular stmts
        ret->string = self->pos;
        break;
    default:;
    }
    // the '!' will be trampled
    if (ret->kind == tkLineComment) ret->string++;
    // turn all 1.0234[DdE]+01 into 1.0234e+01.
    if (ret->kind == tkNumber) {
        CString_tr_ip(ret->string, 'd', 'e', self->matchlen);
        CString_tr_ip(ret->string, 'D', 'e', self->matchlen);
        CString_tr_ip(ret->string, 'E', 'e', self->matchlen);
    }
    return ret;
}

static bool ASTExpr_throws(ASTExpr* self) { // NOOO REMOVE This func and set the
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
    case tkSubscriptResolved: return ASTExpr_throws(self->left);
    case tkVarAssign: return self->var->used && ASTExpr_throws(self->var->init);
    case tkKeyword_for:
    case tkKeyword_if:
    case tkKeyword_while: return false; // actually the condition could throw.
    default:
        if (!self->prec) return false;
        return ASTExpr_throws(self->left) || ASTExpr_throws(self->right);
    }
}

static size_t ASTScope_calcSizeUsage(ASTScope* self) {
    size_t size = 0, sum = 0, subsize = 0, maxsubsize = 0;
    // all variables must be resolved before calling this
    foreach (ASTExpr*, stmt, self->stmts) {
        switch (stmt->kind) {
        case tkKeyword_if:
        case tkKeyword_else:
        case tkKeyword_for:
        case tkKeyword_while:
            subsize = ASTScope_calcSizeUsage(stmt->body);
            if (subsize > maxsubsize) maxsubsize = subsize;
            break;
        default:;
        }
    }
    // some vars are not assigned, esp. temporaries _1 _2 etc.
    foreach (ASTVar*, var, self->locals) {
        size = TypeType_size(var->typeSpec->typeType);
        if (!size)
            eprintf("warning: cannot find size for '%s' at %d:%d\n", var->name,
                var->line, var->col);
        if (var->used) sum += size;
    }
    // add the largest size among the sizes of the sub-scopes
    sum += maxsubsize;
    return sum;
}

static ASTVar* ASTScope_getVar(ASTScope* self, const char* name) {
    // stupid linear search, no dictionary yet
    foreach (ASTVar*, local, self->locals) //
        if (!strcasecmp(name, local->name)) return local;
    if (self->parent) return ASTScope_getVar(self->parent, name);
    return NULL;
}

static ASTVar* ASTType_getVar(ASTType* self, const char* name) {
    // stupid linear search, no dictionary yet
    foreach (ASTVar*, var, self->body->locals) //
        if (!strcasecmp(name, var->name)) return var;

    if (self->super && self->super->typeType == TYObject)
        return ASTType_getVar(self->super->type, name);
    return NULL;
}

#pragma mark - AST FUNC IMPL.

/// This creates a new ASTFunc marked as declare and having one
/// argument. The name of the function and the type of the argument can be
/// specified. This way you can create declared functions such as `print`,
/// `json`, etc. of each new type defined in source code.
static ASTFunc* ASTFunc_createDeclWithArg(
    char* name, char* retType, char* arg1Type) {
    ASTFunc* func = NEW(ASTFunc);
    func->name = name;
    func->isDeclare = true;
    if (retType) {
        func->returnSpec = NEW(ASTTypeSpec);
        func->returnSpec->name = retType;
    }
    if (arg1Type) {
        ASTVar* arg = NEW(ASTVar);
        arg->name = "arg1";
        arg->typeSpec = NEW(ASTTypeSpec);
        arg->typeSpec->name = arg1Type;
        PtrList_append(&func->args, arg);
        func->argCount = 1;
    }
    return func;
}

static size_t ASTFunc_calcSizeUsage(ASTFunc* self) {
    size_t size = 0, sum = 0;
    foreach (ASTVar*, arg, self->args) {
        // all variables must be resolved before calling this
        size = TypeType_size(arg->typeSpec->typeType);
        assert(size);
        // if (arg->used)
        sum += size;
    }
    if (self->body) sum += ASTScope_calcSizeUsage(self->body);
    return sum;
}

///////////////////////////////////////////////////////////////////////////
static bool isCmpOp(ASTExpr* expr) {
    return expr->kind == tkOpLE //
        || expr->kind == tkOpLT //
        || expr->kind == tkOpGT //
        || expr->kind == tkOpGE //
        || expr->kind == tkOpEQ //
        || expr->kind == tkOpNE;
}

///////////////////////////////////////////////////////////////////////////
static bool isBoolOp(ASTExpr* expr) {
    return expr->kind == tkKeyword_and //
        || expr->kind == tkKeyword_or //
        || expr->kind == tkKeyword_in //
        || expr->kind == tkKeyword_notin //
        || expr->kind == tkKeyword_not;
}

static size_t ASTType_calcSizeUsage(ASTType* self) {
    size_t size = 0, sum = 0;
    foreach (ASTVar*, var, self->body->locals) {
        // all variables must be resolved before calling this
        size = TypeType_size(var->typeSpec->typeType);
        assert(size);
        sum += size;
    }
    return sum;
}

#pragma mark - AST EXPR IMPL.
static ASTTypeSpec* ASTExpr_getObjectTypeSpec(const ASTExpr* const self) {
    if (!self) return NULL;

    // all that is left is object
    switch (self->kind) {
    case tkFunctionCallResolved:
        // case tkFunctionCall:
        return self->func->returnSpec;
    case tkIdentifierResolved:
    case tkSubscriptResolved:
        // case tkIdentifier:
        // case tkSubscript:
        return self->var->typeSpec;
        //        }
        // TODO: tkOpColon should be handled separately in the semantic
        // pass, and should be assigned either TYObject or make a dedicated
        // TYRange
        //     case tkOpColon:
        //        return "Range";
        // TODO: what else???
    case tkPeriod:
    case tkOpComma: return ASTExpr_getObjectTypeSpec(self->right);
    default: break;
    }
    return NULL;
}

static ASTType* ASTExpr_getEnumType(const ASTExpr* const self) {
    ASTType* ret = NULL;
    if (!self || self->typeType != TYObject) return ret;
    // all that is left is object
    switch (self->kind) {
    case tkFunctionCallResolved: ret = self->func->returnSpec->type; break;
    case tkIdentifierResolved:
    case tkSubscriptResolved:
        ret = self->var->typeSpec->type;
        break; //        }
               // TODO: tkOpColon should be handled separately in the semantic
               // pass, and should be assigned either TYObject or make a
               // dedicated TYRange
               //     case tkOpColon:
               //        return "Range";
               // TODO: what else???
    case tkPeriod:
    case tkOpComma: ret = ASTExpr_getEnumType(self->left);
    default: break;
    }
    if (ret && !ret->isEnum) ret = NULL;
    return ret;
}

static ASTType* ASTExpr_getObjectType(const ASTExpr* const self) {
    if (!self || self->typeType != TYObject) return NULL;

    // all that is left is object
    switch (self->kind) {
    case tkFunctionCallResolved: return self->func->returnSpec->type;
    case tkIdentifierResolved:
    case tkSubscriptResolved:
        return self->var->typeSpec->type;
        //        }
        // TODO: tkOpColon should be handled separately in the semantic
        // pass, and should be assigned either TYObject or make a dedicated
        // TYRange
        //     case tkOpColon:
        //        return "Range";
        // TODO: what else???
    case tkPeriod:
    case tkOpComma: return ASTExpr_getObjectType(self->right);
    default: break;
    }
    return NULL;
}

static ASTType* ASTExpr_getTypeOrEnum(const ASTExpr* const self) {
    if (!self || self->typeType != TYObject) return NULL;

    // all that is left is object
    switch (self->kind) {
    case tkFunctionCallResolved: return self->func->returnSpec->type;
    case tkIdentifierResolved:
    case tkSubscriptResolved:
        return self->var->typeSpec->type;
        //        }
        // TODO: tkOpColon should be handled separately in the semantic
        // pass, and should be assigned either TYObject or make a dedicated
        // TYRange
        //     case tkOpColon:
        //        return "Range";
        // TODO: what else???
    case tkPeriod: {
        ASTType* type = ASTExpr_getObjectType(self->left);
        if (!type->isEnum) type = ASTExpr_getObjectType(self->right);
        return type;
    }
    case tkOpComma: return ASTExpr_getTypeOrEnum(self->left);
    default: break;
    }
    return NULL;
}
// static CString* ASTExpr_getTypeOrEnumName(const ASTExpr* const self) {
//     ASTType* type = ASTExpr_getTypeOrEnum(self);
//     return type ? type->name : "";
// }

static const char* ASTExpr_typeName(const ASTExpr* const self) {
    if (!self) return "";
    const char* ret = TypeType_name(self->typeType);
    if (!ret) return "<unknown>"; // unresolved
    if (*ret) return ret; // primitive type

    // all that is left is object
    switch (self->kind) {
    case tkFunctionCallResolved: return self->func->returnSpec->type->name;
    case tkIdentifierResolved:
    case tkSubscriptResolved:
        return self->var->typeSpec->type->name;
        //        }
        // TODO: tkOpColon should be handled separately in the semantic
        // pass, and should be assigned either TYObject or make a dedicated
        // TYRange
        //     case tkOpColon:
        //        return "Range";
        // TODO: what else???
    // case tkPeriod:
    //     return ASTExpr_typeName(self->right);
    case tkPeriod: {
        ASTType* type = ASTExpr_getObjectType(self->left);
        return (type->isEnum) ? type->name : ASTExpr_typeName(self->right);
    }
    case tkOpComma: return ASTExpr_typeName(self->left);
    default: break;
    }
    return "<invalid>";
}

static void ASTExpr_catarglabels(ASTExpr* self) {
    switch (self->kind) {
    case tkOpComma:
        ASTExpr_catarglabels(self->left);
        ASTExpr_catarglabels(self->right);
        break;
    case tkOpAssign: printf("_%s", self->left->string); break;
    default: break;
    }
}

static int ASTExpr_strarglabels(ASTExpr* self, char* buf, int bufsize) {
    int ret = 0;
    switch (self->kind) {
    case tkOpComma:
        ret += ASTExpr_strarglabels(self->left, buf, bufsize);
        ret += ASTExpr_strarglabels(self->right, buf + ret, bufsize - ret);
        break;
    case tkOpAssign:
        ret += snprintf(buf, bufsize, "_%s", self->left->string);
        break;
    default: break;
    }
    return ret;
}

// TODO: see if this is still correct
static int ASTExpr_countCommaList(ASTExpr* expr) {
    int i = 0;
    if (expr)
        for (i = 1; expr->right && expr->kind == tkOpComma; i++)
            expr = expr->right;
    return i;
}

#pragma mark - AST MODULE IMPL.

static ASTType* ASTModule_getType(ASTModule* module, const char* name) {
    // the type may be "mm.XYZType" in which case you should look in
    // module mm instead. actually the caller should have bothered about
    // that.
    foreach (ASTType*, type, module->types) //
        if (!strcasecmp(type->name, name)) return type;
    // type specs must be fully qualified, so there's no need to look in
    // other modules.
    foreach (ASTType*, enu, module->enums) //
        if (!strcasecmp(enu->name, name)) return enu;
    return NULL;
}

// i like this pattern, getType, getFunc, getVar, etc.
// even the module should have getVar.
// you don't need the actual ASTImport object, so this one is just a
// bool. imports just turn into a #define for the alias and an #include
// for the actual file.
monostatic ASTImport* ASTModule_getImportByAlias(
    ASTModule* module, const char* alias) {
    foreach (ASTImport*, imp, module->imports) //
    {
        eprintf("import: %s %s\n", imp->name + imp->aliasOffset, alias);
        if (!strcmp(imp->name + imp->aliasOffset, alias)) return imp;
    }
    return NULL;
}

monostatic ASTFunc* ASTModule_getFuncByName(
    ASTModule* module, const char* name) {
    foreach (ASTFunc*, func, module->funcs) //
        if (!strcasecmp(func->name, name)) return func;
    // This returns the first matching func only
    //  no looking anywhere else. If the name is of the form
    // "mm.func" you should have bothered to look in mm instead.
    return NULL;
}
monostatic ASTFunc* ASTModule_getFunc(ASTModule* module, const char* selector) {
    foreach (ASTFunc*, func, module->funcs) //
        if (!strcasecmp(func->selector, selector)) return func;
    //  no looking anywhere else. If the name is of the form
    // "mm.func" you should have bothered to look in mm instead.
    return NULL;
}
monostatic ASTVar* ASTModule_getVar(ASTModule* module, const char* name) {
    foreach (ASTVar*, var, module->scope->locals) //
        if (!strcasecmp(var->name, name)) return var;
    //  no looking anywhere else. If the name is of the form
    // "mm.func" you should have bothered to look in mm instead.
    return NULL;
}

#include "lint.h"
#include "emit.h"

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
//         ASTType* type;
//         ASTFunc* func;
//         ASTVar* var;
//         ASTExpr* expr;
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
    char* moduleName; // mod.submod.xyz.mycode
    char* mangledName; // mod_submod_xyz_mycode
    char* capsMangledName; // MOD_SUBMOD_XYZ_MYCODE
    char *data, *end;
    char* noext;
    PtrArray orig; // holds lines of original source for error reports

    Token token; // current
    IssueMgr issues;
    List(ASTModule) * modules;

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
    ret->noext = CString_noext(CString_clone(filename));
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

static ASTExpr* exprFromCurrentToken(Parser* parser) {
    ASTExpr* expr = ASTExpr_fromToken(&parser->token);
    Token_advance(&parser->token);
    return expr;
}

static ASTExpr* next_token_node(
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
static ASTExpr* Parser_match(Parser* parser, TokenKind expected) {
    return next_token_node(parser, expected, false);
}

// this returns the match node or null
static ASTExpr* Parser_trymatch(Parser* parser, TokenKind expected) {
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

static void getSelector(ASTFunc* func) {
    if (func->argCount) {
        size_t selLen = 0;
        int remain = 128, wrote = 0;
        char buf[128];
        buf[127] = 0;
        char* bufp = buf;

        ASTVar* arg1 = (ASTVar*)func->args->item;
        wrote = snprintf(bufp, remain, "%s_", ASTTypeSpec_name(arg1->typeSpec));
        selLen += wrote;
        bufp += wrote;
        remain -= wrote;

        wrote = snprintf(bufp, remain, "%s", func->name);
        selLen += wrote;
        bufp += wrote;
        remain -= wrote;

        foreach (ASTVar*, arg, func->args->next) {
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

        //  arg1 = (ASTVar*)func->args->item;
        wrote = snprintf(bufp, remain, "%s", ASTTypeSpec_name(arg1->typeSpec));
        selLen += wrote;
        bufp += wrote;
        remain -= wrote;

        foreach (ASTVar*, arg, func->args->next) {
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
static ASTExpr lparen[] = { { .kind = tkParenOpen } };
static ASTExpr rparen[] = { { .kind = tkParenClose } };
static ASTExpr expr_const_0[] = { { .kind = tkNumber, .string = "0" } };
static ASTExpr expr_const_yes[] = { { .kind = tkKeyword_yes } };
static ASTExpr expr_const_no[] = { { .kind = tkKeyword_no } };
static ASTExpr expr_const_nil[] = { { .kind = tkKeyword_nil } };
static ASTExpr expr_const_empty[] = { { .kind = tkString, .string = "" } };

#include "analyse.h"
#include "parse.h"

// TODO: this should be in ASTModule open/close
static void Parser_emit_open(Parser* parser) {
    printf("#ifndef HAVE_%s\n#define HAVE_%s\n\n", parser->capsMangledName,
        parser->capsMangledName);
    printf("#define THISMODULE %s\n", parser->mangledName);
    printf("#define THISFILE \"%s\"\n", parser->filename);
    printf("#define NUMLINES %d\n", parser->token.line);
}

static void Parser_emit_close(Parser* parser) {
    printf("#undef THISMODULE\n");
    printf("#undef THISFILE\n");
    printf("#endif // HAVE_%s\n", parser->capsMangledName);
}

static void alloc_stat() { }

#include "ptr2off.h"

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
    // you have dumped the AST into a binary format and want to check for that.
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

    List(ASTModule) * modules;

    // com lives for the duration of main. it appears in parseModule
    ASTModule* root = parseModule(parser, &modules, NULL);

    if (parser->mode == PMLint) {
        if (parser->issues.hasParseErrors) {
            /* TODO: fallback to token-based linter (formatter)*/
        } else {
            foreach (ASTModule*, mod, modules)
                ASTModule_lint(mod);
        }
    } else if (!(parser->issues.errCount)) {
        switch (parser->mode) {

        case PMEmitC: {
            // TODO: if (monolithic) printf("#define function static\n");
            Parser_emit_open(parser);
            // ^ This is called before including the runtime, so that the
            // runtime can know THISFILE NUMLINES etc.
            printf("#include \"jet/runtime.h\"\n");
            foreach (ASTModule*, mod, modules)
                ASTModule_emit(mod);
            Parser_emit_close(parser);

        } break;

        case PMTest: {
            printf("#include \"jet/tester.h\"\n");
            // TODO : THISFILE must be defined since function callsites need
            // it, but the other stuff in Parser_emit_open isn't required.
            // Besides, THISFILE should be the actual module's file not the
            // test file

            foreach (ASTModule*, mod, modules)
                ASTModule_genTests(mod);
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
