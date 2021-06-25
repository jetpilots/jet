typedef struct PArray {
  void** ref;
  int used, cap;
} PArray;
#define PARRAY_INITN 8 // 2 * sizeof(Expr) / sizeof(void*)
// that way when you need to grow beyond this, you can have a fixed number
// of Expr slots returned to the freelist.

PArray PArray_new() {
  return (PArray) { .ref = Pool_alloc(gPool, PARRAY_INITN * sizeof(void*)),
    .cap = PARRAY_INITN };
}

void PArray_grow2x(PArray* arr) {
  arr->cap *= 2;

  if (arr->cap == 2 * PARRAY_INITN) {
    // TODO: do jet_mem_dealloc
    arr->ref = NULL;
  }
  arr->ref = realloc(arr->ref, sizeof(void*) * arr->cap);
}

void PArray_push(PArray* arr, void* p) {
  if (unlikely(arr->used == arr->cap)) PArray_grow2x(arr);
  arr->ref[arr->used++] = p;
}

// #pragma mark - Jet TYPE DEFINITIONS

// typedef struct JetLocation {
//   uint32_t line : 24, col : 8;
// } JetLocation;

typedef struct Module Module;

typedef struct TypeSpec TypeSpec;
typedef struct Type Type;
typedef struct Func Func;
typedef struct Scope Scope;
typedef struct Expr Expr;
typedef struct Var Var;

typedef struct Import {
  // TODO: put name and mod in union
  char* name; //, *alias;
  Module* mod;
  uint32_t aliasOffset, line : 16, col : 8, used : 1;
  // bool isPackage, hasAlias;
} Import;

static const size_t szImp = sizeof(Import);

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
    Func* proto; // for func ptrs
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
  List(TypeSpec) * params;
  struct { // todo: this must be named TypeInfo & reused in Expr not copy
           // pasted
    uint16_t dims;
    CollectionTypes collType : 6;
    bool hasRange : 1, //
        hasUnits : 1;
    TypeTypes typeType : 7;
    bool nullable : 1;
  };
  // JetLocation loc[0];
  uint32_t line : 24, col : 8;
};
static const size_t szSpec = sizeof(TypeSpec);

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
            CollectionTypes collType : 6;
            bool hasRange : 1, hasUnits : 1;
            TypeTypes typeType : 7;
            bool nullable : 1;
        };
    }; */
  };
  // List(Var*) deps; // TODO: keep deps of each var so you can tell when a
  // dependency of an async var is changed before the async is awaited.
  // First you will have to figure out how to analyze array members and type
  // members (e.g. init an instance for each compound type and array for
  // each array and then set the member of that as dep...) struct Expr*
  // lastUsed; // last expr in owning scope that refers to this var. Note
  // that you should dive to search for the last expr, it may be within an
  // inner scope. The drop call should go at the end of such subscope, NOT
  // within the subscope itself after the actual expr. (so set the
  // if/for/while as the lastref, not the actual lastref) WHY NOT JUST SAVE
  // THE LINE NUMBER OF THE LJet USE?
  // JetLocation loc[0];
  uint32_t line : 24, col : 8; //
  uint16_t lastUsage,
      // firstUsage,
      used, changed;
  // ^ YOu canot use the last used line no to decide drops etc. because code
  // motion can rearrange statements and leave the line numbers stale.
  // --- YES YOU CAN if you use == to compare when dropping and not >=. Also
  // for multiline exprs you should save the toplevel expr line and not the
  // line of the actual tkIdentR.

  // mixed storage is for strings/arrays etc which start out with a stack
  // buffer and move to a heap buffer if grown. var x T starts out with a
  // buffer T x__buf[N*sizeof(T)] in that case where N is an initial guess.
  // you need a separate drop function for each kind of storage.
  struct {
    char //
        isLet : 1, //
        isVar : 1, //
        private : 1, //
        isMutableArg : 1, // func arg only: passed by ref (needs extra *)
        storage : 2, // 0,1,2,3: refc/heap/stack/mixed
        isArg : 1, // a function arg, not a local var
        // stackAlloc : 1, // this var is of a ref type, but it will be
        // stack allocated. it will be passed around by
        // reference as usual.
        usedInSameScope : 1,
        usedInChildScope : 1, // used to mark whether the var could be put
                              // into an inner scope instead.
                              // usedInChildScope means it is used in an
                              // IMMEDIATE child scope (not any level deep).
        isTarget : 1, // x = f(x,y)
        visited : 1, // for generating checks, used to avoid printing this
                     // var more than once.
        promote : 6, // does it escape the owning SCOPE? then just move
                     // it up to the required scope
        escapes : 1, // escapes func? well then heap allocate
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
                     // todo: this will  probably become a count and you
                     // will at compile time incr/decr it
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
        resized : 1; // this collection var was resized after init (due to
                     // resize(), push() etc.) and means that it cannot be
                     // generated as a fixed size array (static if size is
                     // known at compile time).
    // returned : 1; // is a return variable ie. b in
    // function asd(x as Anc) returns (b as Whatever)
    // all args (in/out) are in the same list in func -> args.
    // no need to mark returned; ans is the var returned
  };
  // uint8_t col;
};
static const size_t szVar = sizeof(Var);

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
            collType : 4, // collType of this expr -> the
                          // higher dim-type of left's and right's
                          // collType.
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
    struct {
      uint32_t hash, slen;
    };
  };
  union {
    Expr* right;
    char* str;
    Func* func; // for functioncall
    Var* var; // for array subscript, or a tkVarDefn
    Scope* body; // for if/for/while
    Import* imp; // for imports tkImport
  };
};
// this is a global astexpr representing 0. it will be used when parsing
// e.g. the colon op with nothing on either side. : -> 0:0 means the same as
// 1:end
static Expr lparen[] = { { .kind = tkParenOpen } };
static Expr rparen[] = { { .kind = tkParenClose } };
static Expr expr_const_0[] = { { .kind = tkNumber, .str = "0" } };
static Expr expr_const_yes[] = { { .kind = tkYes } };
static Expr expr_const_no[] = { { .kind = tkNo } };
static Expr expr_const_nil[] = { { .kind = tkNil } };
static Expr expr_const_empty[] = { { .kind = tkString, .str = "" } };

struct Scope {
  List(Expr) * stmts;
  List(Var) * locals;
  Scope* parent;
  bool isLoop; // this affects drops: loop scopes cannot drop parent vars.
  // still space left
};

struct Type {
  char* name;
  List(TypeSpec) * params;

  TypeSpec* super;

  /// The other types that are used in this type (i.e. types of member
  /// variables)
  List(Type) * derivedTypes, *usedTypes, *usedByTypes;
  /// The other types that use this type.
  /// The body of the type, as a scope. In effect it can have any
  /// expressions, but most kinds are disallowed by the parsing routine.
  /// Variable declarations and invariant checks are what you should mostly
  /// expect to see inside type bodies, not much else.
  Scope* body;
  uint16_t line, endline, used, id;
  uint8_t col;
  bool analysed : 1, needJSON : 1, needXML : 1, needYAML : 1, visited : 1,
      isValueType : 1, isEnum : 1, isMultiEnum : 1, private : 1,
      isDeclare : 1;
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
  List(TypeSpec) * params;
  List(Func) * callers, *callees;
  TypeSpec* spec;
  char *sel, *psel;
  struct {
    uint16_t line, endline, used, col;
    struct {
      uint16_t throws : 1,
          recursivity : 2, // 0:unchecked,1:no,2:direct,3:indirect
          visited : 1, // used while checking cycles

          // usesNet : 1,usesIO : 1, usesGUI : 1,
          mutator : 1, // usesSerialisation : 1, //
          private : 1, //
          usesReflection : 1, //
          dispatch : 1, // generate dispatcher for this vfunc
          yields : 1, // iterator, not normal func
          isStmt : 1, //
          isDeclare : 1, //
          isCalledFromWithinLoop : 1, //
          elemental : 1, //
          canEval : 1, // constexpr-like, etc
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
static const size_t szFunc = sizeof(Func);

typedef struct Test {
  char* name;
  Scope* body;
  char* selector;
  struct {
    uint16_t line;
    struct {
      uint16_t analysed : 1;
    } flags;
  };
} Test;

struct Module {

  Scope scope[1]; // global scope contains vars + exprs
  List(Func) * funcs, *tfuncs;
  List(Test) * tests;
  List(Type) * types, *ttypes, *enums;
  List(Import) * imports;
  List(Module) * importedBy; // for dependency graph. also use
                             // imports[i]->mod over i
  char *name, *cname, *Cname, *filename;
  char *out_x, *out_xc, *out_c, *out_o, *out_h, *out_hh, *out_jet;
  int nlines;
  bool modified,
      hmodified; // hmodified means a public func/type has been modified,so
                 // regenerate the header & recompile deps. not relevant in
                 // monolithic mode

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
//         need_Number : 1, need_Range : 1, need_Rational : 1, need_Regex :
//         1, need_Size : 1, need_String : 1, need_YesOrNo : 1, need_Array :
//         1, need_ArrayND : 1, need_Dict : 1, need_Filter : 1, need_List :
//         1, need_Selection : 1, need_Sequence : 1, need_SequenceND : 1,
//         need_Slice : 1, need_SliceND : 1, need_FML : 1, need_HTML : 1,
//         need_JSON : 1, need_XML : 1, need_YAML : 1, need_FTP : 1,
//         need_HTTP : 1, need_IMAP : 1, need_POP3 : 1, need_SMTP : 1,
//         need_SSH : 1, need_Pool : 1;
// } FPNeedBuiltins;

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
MKSTAT(Test)
MKSTAT(JetEnum)
MKSTAT(TypeSpec)
MKSTAT(Type)
MKSTAT(Module)
MKSTAT(Scope)
MKSTAT(Import)
MKSTAT(Var)
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

Expr* Expr_new_() {
  IFDEBUG(_allocTotal_Expr++);
  return Pool_alloc(gPool, sizeof(struct Expr));
}
Var* Var_new_() {
  IFDEBUG(_allocTotal_Var++);
  return Pool_alloc(gPool, sizeof(struct Var));
}
TypeSpec* TypeSpec_new_() {
  IFDEBUG(_allocTotal_TypeSpec++);
  return Pool_alloc(gPool, sizeof(struct TypeSpec));
}
Import* Import_new_() {
  IFDEBUG(_allocTotal_Import++);
  return Pool_alloc(gPool, sizeof(struct Import));
}
Scope* Scope_new_() {
  IFDEBUG(_allocTotal_Scope++);
  return Pool_alloc(gPool, sizeof(struct Scope));
}
Module* Module_new_() {
  IFDEBUG(_allocTotal_Module++);
  return Pool_alloc(gPool, sizeof(struct Module));
}
Type* Type_new_() {
  IFDEBUG(_allocTotal_Type++);
  return Pool_alloc(gPool, sizeof(struct Type));
}
Test* Test_new_() {
  IFDEBUG(_allocTotal_Test++);
  return Pool_alloc(gPool, sizeof(struct Test));
}
Func* Func_new_() {
  IFDEBUG(_allocTotal_Func++);
  return Pool_alloc(gPool, sizeof(struct Func));
}

static TypeSpec* spec_new(TypeTypes tt, CollectionTypes ct) {
  TypeSpec* ret = NEW(TypeSpec);
  ret->typeType = tt;
  ret->collType = ct;
  return ret;
}

static const char* spec_name(TypeSpec* self) {
  switch (self->typeType) {
  case TYUnknown: return self->name;
  case TYObject: return self->type->name;
  default: return Typetype_name(self->typeType);
  }
  // what about collectiontype???
}

// The name of this type spec as it will appear in the generated C code.
static const char* spec_cname(TypeSpec* self) {
  switch (self->typeType) {
  case TYUnknown: return self->name;
  case TYObject: return self->type->name;
  default: return Typetype_name(self->typeType);
  }
  // what about collectiontype???
}

static const char* getDefaultValueForType(TypeSpec* type) {
  if (!type) return "";
  switch (type->typeType) {
  case TYVoid: return "";
  case TYUnknown:
    unreachable(
        "unresolved: '%s' at %d:%d", type->name, type->line, type->col);
    return "ERROR_ERROR_ERROR";
  case TYString: return "\"\"";
  default: return "0";
  }
}

static Expr* expr_fromToken(const Token* self) {
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
  // case tkCheater:
  case tkFor:
  case tkWhile:
  case tkIf:
  case tkEnd:
  case tkEnum:
  case tkMatch:
  case tkCase:
  case tkFunc:
  case tkDecl:
  case tkTest:
  case tkCheck:
  case tkNot:
  case tkNotin:
  case tkAnd:
  case tkYes:
  case tkNo:
  case tkNil:
  case tkOr:
  case tkIn:
  case tkDo:
  case tkThen:
  case tkAs:
  case tkElse:
  case tkElif:
  case tkType:
  case tkReturn:
  case tkYield:
  case tkExtends:
  case tkVar:
  case tkLet:
  case tkImport:
  case tkIdent:
  // case tkArgumentLabel:
  case tkFuncCall:
  case tkSubscript:
  case tkObjInit:
  case tkNumber:
  case tkString:
  case tkRawString:
  case tkRegexp:
  // case tkMultiDotNumber:
  case tkComment: // Comments go in the Jet like regular stmts
    ret->str = self->pos;
    break;
  default:;
  }
  // the '!' will be trampled
  if (ret->kind == tkComment) ret->str++;
  // turn all 1.0234[DdE]+01 into 1.0234e+01.
  if (ret->kind == tkNumber) {
    cstr_tr_ip_len(ret->str, 'd', 'e', self->matchlen);
    cstr_tr_ip_len(ret->str, 'D', 'e', self->matchlen);
    cstr_tr_ip_len(ret->str, 'E', 'e', self->matchlen);
  }
  return ret;
}

// static bool expr_throws(Expr* self) { // NOOO REMOVE This func and set
// the
//                                       // throws flag recursively like the
//     // other flags (during e.g. the type resolution dive)
//     if (!self) return false;
//     switch (self->kind) {
//     case tkNumber:
//     case tkMultiDotNumber:
//     case tkRawString:
//     case tkRegexp:
//     case tkIdent:
//     case tkIdentR:
//     case tkString:
//     case tkComment: return false;
//     case tkFuncCall:
//     case tkFuncCallR:
//         return true; // self->func->throws;
//         // actually  only if the func really throws
//     case tkSubscript:
//     case tkSubscriptR: return expr_throws(self->left);
//     case tkVarDefn: return self->var->used &&
//     expr_throws(self->var->init); case tkFor: case tkIf:
//     case tkWhile: return false; // actually the condition could
//     throw. default:
//         if (!self->prec) return false;
//         return expr_throws(self->left) || expr_throws(self->right);
//     }
// }

static size_t scope_calcSizeUsage(Scope* self) {
  size_t size = 0, sum = 0, subsize = 0, maxsubsize = 0;
  // all variables must be resolved before calling this
  foreach (Expr*, stmt, self->stmts) {
    switch (stmt->kind) {
    case tkIf:
    case tkElse:
    case tkFor:
    case tkWhile:
      subsize = scope_calcSizeUsage(stmt->body);
      if (subsize > maxsubsize) maxsubsize = subsize;
      break;
    default:;
    }
  }
  // some vars are not assigned, esp. temporaries _1 _2 etc.
  foreach (Var*, var, self->locals) {
    size = Typetype_size(var->spec->typeType);
    if (!size)
      eprintf("warning: cannot find size for '%s' at %d:%d\n", var->name,
          var->line, var->col);
    if (var->used) sum += size;
  }
  // add the largest size among the sizes of the sub-scopes
  sum += maxsubsize;
  return sum;
}

static bool scope_defines(Scope* self, Var* var) {
  foreach (Var*, local, self->locals)
    if (local == var) return true;
  return false;
}

// how many scopes up is the var defined?
// 0 -> same scope. -1 -> var not found.
static int scope_reachable(Scope* self, Var* var) {
  int i = 0;
  do {
    if (scope_defines(self, var)) return i;
    i++;
  } while ((self = self->parent));
  return -1;
}

static int scope_reachableNoLoops(Scope* self, Var* var) {
  int i = 0;
  do {
    if (self->isLoop) return 0;
    if (scope_defines(self, var)) return i;
    i++;
  } while ((self = self->parent));
  return -1;
}

static Var* scope_getVar(Scope* self, const char* name) {
  // stupid linear search, no dictionary yet
  foreach (Var*, local, self->locals) //
    if (!strcasecmp(name, local->name)) return local;
  if (self->parent) return scope_getVar(self->parent, name);
  return NULL;
}

static Var* type_getVar(Type* self, const char* name) {
  // stupid linear search, no dictionary yet
  foreach (Var*, var, self->body->locals) //
    if (!strcasecmp(name, var->name)) return var;

  if (self->super && self->super->typeType == TYObject)
    return type_getVar(self->super->type, name);
  return NULL;
}

#pragma mark - Jet FUNC IMPL.

/// This creates a new Func marked as declare and having one
/// argument. The name of the function and the type of the argument can be
/// specified. This way you can create declared functions such as `print`,
/// `json`, etc. of each new type defined in source code.
static Func* func_createDeclWithArg(
    char* name, char* retType, char* arg1Type) {
  Func* func = NEW(Func);
  func->name = name;
  func->nameLen = strlen(name);
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
    li_push(&func->args, arg);
    func->argCount = 1;
  }
  return func;
}

static size_t func_calcSizeUsage(Func* self) {
  size_t size = 0, sum = 0;
  foreach (Var*, arg, self->args) {
    // all variables must be resolved before calling this
    size = Typetype_size(arg->spec->typeType);
    assert(size);
    // if (arg->used)
    sum += size;
  }
  if (self->body) sum += scope_calcSizeUsage(self->body);
  return sum;
}

static void getSelector(Func* func) {
  if (func->argCount) {
    size_t selLen = 0;
    int remain = 128, wrote = 0;
    char buf[128];
    buf[127] = 0;
    char *bufp = buf, *collName = "";

    Var* arg1 = (Var*)func->args->item;
    if (arg1->spec->collType)
      collName = cstr_interp_s(
          256, "_%s", Collectiontype_name(arg1->spec->collType));
    wrote
        = snprintf(bufp, remain, "%s%s_", spec_name(arg1->spec), collName);
    selLen += wrote;
    bufp += wrote;
    remain -= wrote;

    int l = func->nameLen;
    if (!l) l = strlen(func->name);
    wrote = snprintf(bufp, remain, "%.*s", func->nameLen, func->name);
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
    func->sel = cstr_pndup(buf, selLen + 1);
    // func->sel = PoolB_alloc(strPool, selLen + 1);
    // memcpy(func->sel, buf, selLen + 1);

    bufp = buf;

    wrote = snprintf(bufp, remain, "%s(", func->name);
    selLen += wrote;
    bufp += wrote;
    remain -= wrote;

    wrote = snprintf(bufp, remain, "%s%s", spec_name(arg1->spec), collName);
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
    func->psel = cstr_pndup(buf, selLen + 1);

  } else {
    func->sel = func->name;
    char buf[128];
    buf[127] = 0;
    int n = snprintf(buf, 127, "%s()", func->name);
    func->psel = cstr_pndup(buf, n + 1);
    ;
  }
}

///////////////////////////////////////////////////////////////////////////
static bool isCmpOp(Expr* expr) {
  return expr->kind == tkLE //
      || expr->kind == tkLT //
      || expr->kind == tkGT //
      || expr->kind == tkGE //
      || expr->kind == tkEQ //
      || expr->kind == tkNE;
}

///////////////////////////////////////////////////////////////////////////
static bool isBoolOp(Expr* expr) {
  return expr->kind > __tk__logicals__begin
      && expr->kind < __tk__logicals__end;
  // return expr->kind == tkAnd //
  //     || expr->kind == tkOr //
  //     || expr->kind == tkIn //
  //     || expr->kind == tkNotin //
  //     || expr->kind == tkNot;
}

static bool isCtrlExpr(Expr* expr) {
  return expr->kind > __tk__ctrlflow__begin
      && expr->kind < __tk__ctrlflow__end;

  //  expr->kind == tkIf //
  //     || expr->kind == tkFor //
  //     || expr->kind == tkWhile //
  //     || expr->kind == tkElse || expr->kind == tkElif
  //     || expr->kind == tkCase || expr->kind == tkMatch;
}

static bool isSelfMutOp(Expr* expr) {
  return expr->kind > __tk__selfMutOps__begin
      && expr->kind < __tk__selfMutOps__end;
  //  expr->kind == tkPlusEq //
  //     || expr->kind == tkMinusEq //
  //     || expr->kind == tkSlashEq //
  //     || expr->kind == tkTimesEq //
  //     || expr->kind == tkPowerEq //
  //     || expr->kind == tkModEq //
  //     || expr->kind == tkAssign;
}

static bool isArithOp(Expr* expr) {
  return expr->kind > __tk__arithOps__begin
      && expr->kind < __tk__arithOps__end;
  // == tkPlusEq //
  //     || expr->kind == tkMinusEq //
  //     || expr->kind == tkSlashEq //
  //     || expr->kind == tkTimesEq //
  //     || expr->kind == tkPowerEq //
  //     || expr->kind == tkModEq //
  //     || expr->kind == tkPlus || expr->kind == tkMinus
  //     || expr->kind == tkSlash || expr->kind == tkTimes
  //     || expr->kind == tkPower || expr->kind == tkMod;
}

static bool isLiteralExpr(Expr* expr) { return false; }
static bool isComparatorExpr(Expr* expr) { return false; }

static size_t type_calcSizeUsage(Type* self) {
  size_t size = 0, sum = 0;
  foreach (Var*, var, self->body->locals) {
    // all variables must be resolved before calling this
    size = Typetype_size(var->spec->typeType);
    assert(size);
    sum += size;
  }
  return sum;
}

#pragma mark - Jet EXPR IMPL.
static TypeSpec* expr_getObjectTypeSpec(const Expr* const self) {
  if (!self) return NULL;

  // all that is left is object
  switch (self->kind) {
  case tkFuncCallR:
    return self->func->spec;
    // case tkIdent:
    // case tkSubscript:
  case tkIdentR:
  case tkSubscriptR:
    return self->var->spec;
    //        }
    // TODO: tkColon should be handled separately in the semantic
    // pass, and should be assigned either TYObject or make a dedicated
    // TYRange
    //     case tkColon:
    //        return "Range";
    // TODO: what else???
  case tkPeriod:
  case tkComma: return expr_getObjectTypeSpec(self->right);
  default: break;
  }
  return NULL;
}

static Type* expr_getEnumType(const Expr* const self) {
  Type* ret = NULL;
  if (!self || self->typeType != TYObject) return ret;
  // all that is left is object
  switch (self->kind) {
  case tkFuncCallR: ret = self->func->spec->type; break;
  case tkIdentR:
  case tkSubscriptR:
    ret = self->var->spec->type;
    break;
    //        }
    // TODO: tkColon should be handled separately in the semantic
    // pass, and should be assigned either TYObject or make a
    // dedicated TYRange
    //     case tkColon:
    //        return "Range";
    // TODO: what else???
  case tkPeriod:
  case tkComma: ret = expr_getEnumType(self->left);
  default: break;
  }
  if (ret && !ret->isEnum) ret = NULL;
  return ret;
}

static Type* expr_getObjectType(const Expr* const self) {
  if (!self || self->typeType != TYObject) return NULL;

  // all that is left is object
  switch (self->kind) {
  case tkFuncCallR: return self->func->spec->type;
  case tkIdentR:
  case tkSubscriptR:
    return self->var->spec->type;
    //        }
    // TODO: tkColon should be handled separately in the semantic
    // pass, and should be assigned either TYObject or make a dedicated
    // TYRange
    //     case tkColon:
    //        return "Range";
    // TODO: what else???
  case tkPeriod:
  case tkComma: return expr_getObjectType(self->right);
  default: break;
  }
  return NULL;
}

static Type* expr_getTypeOrEnum(const Expr* const self) {
  if (!self || self->typeType != TYObject) return NULL;

  // all that is left is object
  switch (self->kind) {
  case tkFuncCallR: return self->func->spec->type;
  case tkIdentR:
  case tkSubscriptR:
    return self->var->spec->type;
    //        }
    // TODO: tkColon should be handled separately in the semantic
    // pass, and should be assigned either TYObject or make a dedicated
    // TYRange
    //     case tkColon:
    //        return "Range";
    // TODO: what else???
  case tkPeriod: {
    Type* type = expr_getObjectType(self->left);
    if (!type->isEnum) type = expr_getObjectType(self->right);
    return type;
  }
  case tkComma: return expr_getTypeOrEnum(self->left);
  default: break;
  }
  return NULL;
}
// static CString* expr_getTypeOrEnumName(const Expr* const self) {
//     Type* type = expr_getTypeOrEnum(self);
//     return type ? type->name : "";
// }

static const char* expr_typeName(const Expr* const self) {
  if (!self) return "";
  const char* ret = Typetype_name(self->typeType);
  if (!ret) return "<unknown>"; // unresolved
  if (*ret) return ret; // primitive type

  // all that is left is object
  switch (self->kind) {
  case tkFuncCallR: return self->func->spec->type->name;
  case tkIdentR:
  case tkSubscriptR:
    return self->var->spec->type->name;
    //        }
    // TODO: tkColon should be handled separately in the semantic
    // pass, and should be assigned either TYObject or make a dedicated
    // TYRange
    //     case tkColon:
    //        return "Range";
    // TODO: what else???
  // case tkPeriod:
  //     return expr_typeName(self->right);
  case tkPeriod: {
    Type* type = expr_getObjectType(self->left);
    return (type && type->isEnum) ? type->name : expr_typeName(self->right);
  }
  case tkComma: return expr_typeName(self->left);
  default: break;
  }
  return "<invalid>";
}

static void expr_catarglabels(Expr* self) {
  switch (self->kind) {
  case tkComma:
    expr_catarglabels(self->left);
    expr_catarglabels(self->right);
    break;
  case tkAssign: printf("_%s", self->left->str); break;
  default: break;
  }
}

static int expr_strarglabels(Expr* self, char* buf, int bufsize) {
  int ret = 0;
  switch (self->kind) {
  case tkComma:
    ret += expr_strarglabels(self->left, buf, bufsize);
    ret += expr_strarglabels(self->right, buf + ret, bufsize - ret);
    break;
  case tkArgAssign:
    ret += snprintf(buf, bufsize, "_%s", self->left->str);
    break;
  default: break;
  }
  return ret;
}

// TODO: see if this is still correct
static int expr_countCommaList(Expr* expr) {
  int i = 0;
  if (expr)
    for (i = 1; expr->right && expr->kind == tkComma; i++)
      expr = expr->right;
  return i;
}

static Type* mod_getType(Module* module, const char* name) {
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
monostatic Import* mod_getImportByAlias(Module* module, const char* alias) {
  foreach (Import*, imp, module->imports) //
  {
    eprintf("import: %s %s\n", imp->name + imp->aliasOffset, alias);
    if (!strcmp(imp->name + imp->aliasOffset, alias)) return imp;
  }
  return NULL;
}

monostatic Func* mod_getFuncByName(Module* module, const char* name) {
  foreach (Func*, func, module->funcs) //
    if (!strcasecmp(func->name, name)) return func;
  // This returns the first matching func only
  //  no looking anywhere else. If the name is of the form
  // "mm.func" you should have bothered to look in mm instead.
  return NULL;
}
monostatic Func* mod_getFunc(Module* module, const char* selector) {
  foreach (Func*, func, module->funcs) //
    if (!strcasecmp(func->sel, selector)) return func;
  //  no looking anywhere else. If the name is of the form
  // "mm.func" you should have bothered to look in mm instead.
  return NULL;
}

monostatic Expr* expr_getExpr(Expr* expr, int col) {
  int l;
  Expr* ret;
  switch (expr->kind) {
  case tkIdent:
  case tkSubscript:
  case tkFuncCall:
  case tkNumber:
  case tkString:
  case tkRawString:
  case tkRegexp:
    l = strlen(expr->str);
    if (expr->col <= col && col <= expr->col + l) return expr;
    break;
  case tkIdentR:
  case tkSubscriptR:
  case tkFuncCallR:
    l = strlen(expr->var->name);
    if (expr->col <= col && col <= expr->col + l) return expr;
    break;
  case tkIf:
  case tkWhile:
  case tkCase:
  case tkMatch:
    ret = expr_getExpr(expr->left, col);
    return ret ? ret : expr;
  default:
    if (expr->prec && !expr->unary) {
      ret = expr_getExpr(expr->left, col);
      if (ret) return ret;
    }
    ret = expr_getExpr(expr->right, col);
    if (ret) return ret;
    if (expr->col <= col
        && col <= expr->col + strlen(TokenKind_repr[expr->kind]))
      return expr;
  }
  return NULL;
}

monostatic Expr* scope_getExpr(
    Scope* scope, int line, int col, Scope** owner) {
  Expr* ret;
  foreach (Expr*, stmt, scope->stmts) {
    switch (stmt->kind) {
    case tkIf:
    case tkFor:
    case tkCase:
    case tkWhile:
      if (stmt->line == line) {
        *owner = scope;
        ret = expr_getExpr(stmt->left, col);
        return ret ? ret : stmt->left;
      } else {
        ret = scope_getExpr(stmt->body, line, col, owner);
        if (ret) return ret;
      }
      break;

    default:
      if (stmt->line == line) {
        *owner = scope;
        ret = expr_getExpr(stmt, col);
        return ret ? ret : stmt;
      }
      break;
    }
  }
  return NULL;
}

monostatic Expr* mod_getExpr(Module* mod, int line, int col,
    Func** ownerFunc, Scope** ownerScope, Type** ownerType) {
  foreach (Func*, func, mod->funcs) {
    if (func->line <= line && line <= func->endline) {
      Expr* ret = scope_getExpr(func->body, line, col, ownerScope);
      if (ret) {
        *ownerFunc = func;
        *ownerType = NULL;
        return ret;
      }
    }
  }
  foreach (Type*, type, mod->types) {
    if (type->line <= line && line <= type->endline) {
      Expr* ret = scope_getExpr(type->body, line, col, ownerScope);
      if (ret) {
        *ownerFunc = NULL;
        *ownerType = type;
        return ret;
      }
    }
  }
  foreach (Type*, type, mod->enums) {
    if (type->line <= line && line <= type->endline) {
      Expr* ret = scope_getExpr(type->body, line, col, ownerScope);
      if (ret) {
        *ownerFunc = NULL;
        *ownerType = type;
        return ret;
      }
    }
  }
  return NULL;
}
monostatic bool upcastable(Type* type, Type* target) {
  if (!type || !target) return false;
  if (type->isEnum) return type == target;
  do {
    if (type == target) return true;
    type = type->super ? type->super->type : NULL;
  } while (type);
  return false;
}

// only for use with runtime dispatch, obv
monostatic bool downcastable(Type* type, Type* target) {
  return upcastable(target, type);
}

// #define List_expr_zip(list, expr, yield)                                       \
//     while (1) {                                                                \
//         if (!(list = List_next(list))) break;                                  \
//         if (!(expr = expr_next(expr))) break;                                  \
//         yield(List_get(list), expr_get(expr));                                 \
//     }

// zip is basically each over two parallel collections.
// use it as zip(a, Var*, b, Var*, mod1->vars, List*, mod2->vars, Array*)
// { ... code using a, b }

// (a!=b&&c!=d)
#define BOTHOK(a, b, c, d)                                                 \
  ~(~((uint64_t)a ^ (uint64_t)b) | ~((uint64_t)c ^ (uint64_t)d))
#define zipn(item1, nxt1, E1, item2, nxt2, E2, coll1, T1, coll2, T2)       \
  for (bool _ = 1; _;)                                                     \
    for (T1##_Iter _it1 = begin(T1, coll1), _en1 = end(T1, coll1); _;)     \
      for (T2##_Iter _it2 = begin(T2, coll2), _en2 = end(T2, coll2); _;)   \
        for (E1 item1, nxt1; _;)                                           \
          for (E2 item2, nxt2; _; _ = 0)                                   \
            for (; BOTHOK(_it1, _en1, _it2, _en2);                         \
                 movenext(T1, _it1), movenext(T2, _it2))                   \
              if (item1 = get(T1, _it1), nxt1 = getu(T1, next(T1, _it1)),  \
                  item2 = get(T2, _it2), nxt2 = getu(T2, next(T2, _it2)),  \
                  1)

#define zip(item1, E1, item2, E2, coll1, T1, coll2, T2)                    \
  zipn(item1, __nxt1__, E1, item2, __nxt2__, E2, coll1, T1, coll2, T2)

// each is actually a template that just wraps other actions
// use it as each(var, Var*, mod->vars, List*) { ... code using var }
// use it as each(var, Var*, mod->vars, Array*)  so on
// E is element type, T is collection type
#define eachn(item, nxt, E, coll, T)                                       \
  for (bool _ = 1; _;)                                                     \
    for (T##_Iter _it = begin(T, coll), _end = end(T, coll); _;)           \
      for (E item, nxt; _; _ = 0)                                          \
        for (; _it != _end; movenext(T, _it))                              \
          if (item = get(T, _it), nxt = getu(T, next(T, _it)), 1)

#define each(item, E, coll, T) eachn(item, __next__, E, coll, T)

#define next(T, it) T##_next(it)
#define begin(T, it) T##_begin(it)
#define end(T, it) T##_end(it)
#define get(T, it) T##_get(it)
#define getu(T, it) (it ? T##_get(it) : (T)0)

// begin:
#define ExprP_begin(expr) (expr)
#define PtrListP_begin(list) (list)

// end:
#define ExprP_end(expr) (NULL)
#define PtrListP_end(list) (NULL)

// get: get item T from iterator<T>. in C++ this is operator *
#define ExprP_get(expr) (expr->kind == tkComma ? expr->left : expr)
#define PtrListP_get(list) (list->item)

// next: iterator to next item. in C++ this is operator ++, (here not
// in-place)
#define PtrListP_next(list) (list->next)
#define ExprP_next(expr) (expr->right)
// #define Array_next(ptr) (ptr + 1)
// #define Slice_next(slice) (slice->ptr + slice->step)
#define movenext(T, l) l = T##_next(l)

// only call this func if you have failed to resolve a func by
// getFunc(...).
typedef Expr* ExprP;
typedef ExprP ExprP_Iter;
typedef PtrList* PtrListP;
typedef PtrListP PtrListP_Iter;
typedef Var* VarP;

monostatic Func* mod_getFuncByTypeMatch(
    Module* module, Expr* funcCallExpr) {
  foreach (Func*, func, module->funcs) {
    if (strcasecmp(funcCallExpr->str, func->name)) continue;
    if (expr_countCommaList(funcCallExpr->left) != func->argCount) continue;
    // check all argument types to see if they match.
    // Expr* currArg = funcCallExpr->left;
    // zip(cArg, ExprP, arg, VarP, funcCallExpr->left, ExprP, func->args,
    //     PtrListP) {
    //     printf("%s at %d:%d / %s at %d:%d\n", arg->name, arg->line,
    //         arg->col, TokenKind_names[cArg->kind], cArg->line,
    //         cArg->col);
    // }
    // each(arg, VarP, func->args, PtrListP) { printf("%s\n", arg->name);
    // }

    zip(cArg, ExprP, arg, VarP, funcCallExpr->left, ExprP, func->args,
        PtrListP) {
      if (cArg->kind == tkArgAssign) {
        if (strcasecmp(arg->name, cArg->left->str)) goto nextfunc;
        cArg = cArg->right;
      }
      if (ISIN(3, cArg->typeType, TYUnknown, TYError, TYVoid)) return NULL;
      if (cArg->typeType == arg->spec->typeType
          && cArg->collType == arg->spec->collType) {
        if (cArg->typeType == TYObject
            && !upcastable(expr_getTypeOrEnum(cArg), arg->spec->type))
          goto nextfunc;
      } else {
        if (cArg->typeType != TYUnknown) goto nextfunc;
      }
    }

    // foreach (Var*, arg, func->args) {
    //     if (!currArg) break;
    //     Expr* cArg = (currArg->kind == tkComma) ? currArg->left :
    //     currArg; if (cArg->kind == tkAssign) cArg = cArg->right;
    //     // __ this is why you need typeType and typeSubType so that
    //     // compatible types can be checked by equality ignoring
    //     subType.
    //     // The way it is now, CString and String wont match because
    //     they
    //     // arent strictly equal, although they are perfectly compatible
    //     // for argument passing.
    //     if (ISIN(3, cArg->typeType, TYUnknown, TYError, TYVoid))
    //         return NULL;
    //     if (cArg->typeType == arg->spec->typeType
    //         && cArg->collType == arg->spec->collType) {

    //         if (cArg->typeType == TYObject
    //             && !upcastable(expr_getTypeOrEnum(cArg),
    //             arg->spec->type)) goto nextfunc;

    //     } else {
    //         // an arg type has failed to match. Wait: if it is
    //         unresolved,
    //         // you can still consider this func. Otherwise you skip to
    //         next. if (cArg->typeType != TYUnknown) goto nextfunc;
    //     }
    //     currArg = (currArg->kind == tkComma) ? currArg->right : NULL;
    // }
    return func;
  nextfunc:;
  }
  return NULL;
}
monostatic Var* mod_getVar(Module* module, const char* name) {
  foreach (Var*, var, module->scope->locals) //
    if (!strcasecmp(var->name, name)) return var;
  //  no looking anywhere else. If the name is of the form
  // "mm.func" you should have bothered to look in mm instead.
  return NULL;
}