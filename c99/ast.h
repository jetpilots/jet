#pragma mark - Jet TYPE DEFINITIONS

typedef struct JetLocation {
    uint32_t line : 24, col : 8;
} JetLocation;

typedef struct Module Module;

typedef struct TypeSpec TypeSpec;
typedef struct Type Type;
typedef struct Func Func;
typedef struct Scope Scope;
typedef struct Expr Expr;
typedef struct Var Var;

typedef struct Import {
    char* name; //, *alias;
    Module* module;
    uint32_t aliasOffset, line : 16, col : 8, used : 1;
    // bool isPackage, hasAlias;
} Import;

typedef struct JetUnits {
    uint8_t powers[7], something;
    double factor, factors[7];
    char* label;
} JetUnits;

typedef struct {
    double start, end;
} Interval;
// TODO: replace this with the generic RealRange

// TODO: somewhere in the typespec or in the Var need a flag refCounted
struct TypeSpec {
    union {
        Type* type;
        char* name;
        Interval* intv;
        // ^ e.g. var x in [1:250]
        // this does not make x integer. it only provides a constraint, you need
        // other clues to decide if x should really be integer.
        // e.g. var x in [1:1:256] -> now its an integer
        JetUnits* units;
        // not keeping units on vars; move them to exprs instead. that way | is
        // simply a check/multiply op. in tthe Jet | is a tkDimensionedExpr, or
        // simply every expr has a JetUnits* member.
    };
    struct { // todo: this must be named TypeInfo & reused in Expr not copy
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
    JetLocation loc[0];
    uint32_t line : 24, col, : 8;
};

struct Var {
    char* name;
    TypeSpec* spec;
    union {
        Expr* init;
        // when you move to having return var, there wont be a spec on
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
    // List(Var*) deps; // TODO: keep deps of each var so you can tell when a
    // dependency of an async var is changed before the async is awaited. First
    // you will have to figure out how to analyze array members and type members
    // (e.g. init an instance for each compound type and array for each array
    // and then set the member of that as dep...)
    // struct Expr* lastUsed; //
    // last expr in owning scope that refers to this var. Note that you should
    // dive to search for the last expr, it may be within an inner scope. The
    // drop call should go at the end of such subscope, NOT within the subscope
    // itself after the actual expr. (so set the if/for/while as the lastref,
    // not the actual lastref) WHY NOT JUST SAVE THE LINE NUMBER OF THE LJet
    // USE?
    JetLocation loc[0];
    uint32_t line : 24, col : 8; //
    uint16_t lastUsage, used, changed;
    // ^ YOu canot use the last used line no to decide drops etc. because code
    // motion can rearrange statements and leave the line numbers stale.
    // --- YES YOU CAN if you use == to compare when dropping and not >=. Also
    // for multiline exprs you should save the toplevel expr line and not the
    // line of the actual tkIdentifierResolved.

    // mixed storage is for strings/arrays etc which start out with a stack
    // buffer and move to a heap buffer if grown. var x T starts out with a
    // buffer T x__buf[N*sizeof(T)] in that case where N is an initial guess.
    // you need a separate drop function for each kind of storage.
    struct {
        char //
            isLet : 1, //
            isVar : 1, //
            storage : 2, // 0,1,2,3: refc/heap/stack/mixed
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
};

static const char* const StorageClassNames[]
    = { "refcounted", "heap", "stack", "mixed" };
// when does something escape a scope?
// -- if it is assigned to a variable outside the scope
//    -- for func toplevels, one such var is the return var: anything
//    assigned to it escapes
// -- if it is passed to a func as an arg and the arg `escapes`
//    analyseExpr sets `escapes` on each arg as it traverses the func

struct Expr {
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
            extract : 1, // should this expr be extracted to a var, e.g.
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
        Expr* left;
        List(Expr) * vars; // for tkString
        Type* elementType; // for tkListLiteral, tkDictLiteral only!!
    };
    union {
        // JetEvalInfo eval;
        struct {
            uint32_t hash, slen;
        }; // str len for strings, idents, unresolved funcs/vars etc.
        // why do you need the hash in the expr? just add to the dict using the
        // computed hash! TO COMPUTE HASH OF EXPR TREES
    };
    union {
        Expr* right;
        char* string;
        double real;
        int64_t integer;
        uint64_t uinteger;
        // char* name; // for idents or unresolved call or subscript
        Func* func; // for functioncall
        Var* var; // for array subscript, or a tkVarAssign
        Scope* body; // for if/for/while
        Import* import; // for imports tkKeyword_import
    };
    // TODO: the code motion routine should skip over exprs with
    // promote=false this is set for exprs with func calls or array
    // filtering etc...
};

struct Scope {
    List(Expr) * stmts;
    List(Var) * locals;
    Scope* parent;
    bool isLoop; // this affects drops: loop scopes cannot drop parent vars.
    // still space left
};

struct Type {
    char* name;
    /// [unused] supertype. Jet does not have inheritance, perhaps for good.
    TypeSpec* super;
    /// The other types that are used in this type (i.e. types of member
    /// variables)
    List(Type) * usedTypes, *usedByTypes;
    /// The other types that use this type.
    /// The body of the type, as a scope. In effect it can have any expressions,
    /// but most kinds are disallowed by the parsing routine. Variable
    /// declarations and invariant checks are what you should mostly expect to
    /// see inside type bodies, not much else.
    Scope* body;
    uint16_t line, used;
    uint8_t col;
    bool analysed : 1, needJSON : 1, needXML : 1, needYAML : 1, visited : 1,
        isValueType : 1, isEnum : 1,
        isDeclare : 1; // all vars of this type will be stack
                       // allocated and passed around by value.
};

// typedef struct JetEnum {
//     char* name;
//     Scope* body;
//     uint16_t line;
//     uint8_t col;
//     bool analysed : 1, visited : 1;
// } JetEnum;

struct Func {
    char* name;
    Scope* body;
    List(Var) * args;
    List(Func) * callers, *callees;
    TypeSpec* spec;
    char *selector, *prettySelector;
    struct {
        uint16_t line, used, col;
        struct {
            uint16_t throws : 1,
                recursivity : 2, // 0:unchecked,1:no,2:direct,3:indirect
                visited : 1, // used while checking cycles

                // usesNet : 1,usesIO : 1, usesGUI : 1,
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
};

typedef struct JetTest {
    char* name;
    Scope* body;
    char* selector;
    struct {
        uint16_t line;
        struct {
            uint16_t analysed : 1;
        } flags;
    };
} JetTest;

struct Module {

    Scope scope[1]; // global scope contains vars + exprs
    List(Func) * funcs;
    List(JetTest) * tests;
    List(Type) * types, *enums;
    List(Import) * imports;
    List(Module) * importedBy; // for dependency graph. also use
                               // imports[i]->module over i
    char *name, *fqname, *filename;

    // DiagnosticReporter reporter;

    struct {
        bool complex : 1, json : 1, yaml : 1, xml : 1, html : 1, http : 1,
            ftp : 1, imap : 1, pop3 : 1, smtp : 1, frpc : 1, fml : 1, fbin : 1,
            rational : 1, polynomial : 1, regex : 1, datetime : 1, colour : 1,
            range : 1, table : 1, gui : 1;
    } requires;
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

#pragma mark - Jet IMPORT IMPL.

#pragma mark - Jet UNITS IMPL.

#define List_Expr PtrList
#define List_Var PtrList
#define List_Module PtrList
#define List_Func PtrList
#define List_JetEnum PtrList
#define List_JetTest PtrList
#define List_Type PtrList
#define List_Import PtrList
#define List_Scope PtrList

MKSTAT(Expr)
MKSTAT(Func)
MKSTAT(JetTest)
MKSTAT(JetEnum)
MKSTAT(TypeSpec)
MKSTAT(Type)
MKSTAT(Module)
MKSTAT(Scope)
MKSTAT(Import)
MKSTAT(Var)
MKSTAT(Parser)
MKSTAT(List_Expr)
MKSTAT(List_Func)
MKSTAT(List_JetEnum)
MKSTAT(List_JetTest)
MKSTAT(List_Type)
MKSTAT(List_Module)
MKSTAT(List_Scope)
MKSTAT(List_Import)
MKSTAT(List_Var)
static uint32_t exprsAllocHistogram[128];