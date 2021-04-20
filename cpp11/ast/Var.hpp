enum CollectionType : char {
    CTYNone,
    CTYDict,
    CTYSet,
    CTYArray,
    CTYList,
    CTYDlList,
    CTYStackArray,
    CTYStaticArray,
    CTYSmallArray,
    CTYSmallFixedArray,
    CTYArrayND,
    CTYTensor // dynamic dims
};
enum TypeType : char {
    TYUnresolved,
    TYObject,
    TYRange,
    TYNoType,
    TYNilType,
    TYErrorType,
    TYString,
    TYCString,
    TYFixedString,
    TYBool,
    TYNumber
};
enum TypeSubTypeNumber : char {
    TYNumReal64,
    TYNumReal32,
    TYNumInt64,
    TYNumUint64,
    TYNumInt32,
    TYNumUint32,
    TYNumInt16,
    TYNumUint16,
    TYNumInt8,
    TYNumUint8,
    TYNumComplex64,
    TYNumComplex128,
    TYNumDual64,
    TYNumDual128
};
enum TypeSubTypeString : char { TYStrGrowable, TYStrFixed, TYStrCString };
struct Type;
struct Expr;
typedef unsigned long long u64;
union TypeInfo {
    struct {
        TypeType typeType;
        union {
            TypeSubTypeString str;
            TypeSubTypeNumber num;
        } typeSubType;
        CollectionType collectionType;
        unsigned char dims;
    };
    int32_t val;
    bool operator!=(TypeInfo& t2) { return not(*this == t2); }
    bool operator==(TypeInfo& t2) { return val == t2.val; }
    int size() {
        switch (typeType) { }
    }
};
static_assert(sizeof(TypeInfo) == 4, "");

struct Var {
    const char* name;
    union {
        const char* _typename;
        Type* type;
    };

    TypeInfo typeInfo;
    Expr* init;
    SourceLoc loc;
    unsigned char changed;
    bool used, isMutable, isArgument, reassigned, escapes, visited;

    Var() { }
    Var(const char* name, SourceLoc loc, Expr* init)
        : name(name)
        , loc(loc)
        , init(init) { }

    const char* typeName() {
        if (typeInfo.typeType == TYObject)
            return type->name;
        else if (typeInfo.typeType)
            return typeInfo.name();
        else
            return _typename;
    }

    const char* getDefaultValue() {
        switch (typeInfo.typeType) {
        case TYUnresolved:
            unreachable("var '%s' with unresolved type '%s' at %d:%d", name,
                _typename, loc.line, loc.col);
            return "ERROR_ERROR_ERROR";
        case TYString: return "\"\"";
        default: return "0";
        }
    }
    bool is(TypeType ty) { return ty == typeInfo.typeType; }
    bool is(CollectionType cty) { return cty == typeInfo.collectionType; }
    // bool isNumeric() { return typeInfo.typeType == TYNumber; }
    bool isEnum() { return is(TYObject) and type->isEnum; }
};