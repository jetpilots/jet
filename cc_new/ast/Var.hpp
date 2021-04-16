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
    TYNumInt8,
    TYNumUint8,
    TYNumInt16,
    TYNumUint16,
    TYNumInt32,
    TYNumUint32,
    TYNumInt64,
    TYNumUint64,
    TYNumReal32,
    TYNumReal64,
    TYNumComplex64,
    TYNumComplex128,
    TYNumDual64,
    TYNumDual128
};
struct Type;
typedef unsigned long long u64;
struct TypeInfo {
    // union {
    //     const char* _name;
    //     Type* _type;
    //     struct {
    //         char _ptr[6];
    TypeType typeType;
    // bool elemental : 1;
    union {
        TypeSubTypeString str;
        TypeSubTypeNumber num;
        // TypeSubTypeString str;
    } typeSubType;
    CollectionType collectionType;
    unsigned char dims;
    // };
    // };
    const char* name() { return (char*)((u64)_name | 0xffffffffffffUL); }
    Type* type() { return (Type*)((u64)_type | 0xffffffffffffUL); }
    bool is(TypeType ty) { return ty == typeType; }
    bool is(CollectionType cty) { return cty == collectionType; }
    bool isNumeric() { return typeType > TYInt8; }
    bool isEnum() { return is(TYObject) and type()->isEnum; }
    bool operator!=(TypeInfo& t2) { return not(*this == t2); }
    bool operator==(TypeInfo& t2) {
        return typeType != t2.typeType //
            or type() != t2.type() //
            or collectionType != t2.collectionType;
    }
};
static_assert(sizeof(TypeInfo) == 4, "");

struct Var {
    const char* name;
    TypeInfo typeInfo;
    Expr* init;
    unsigned char changed;
    bool used, isMutable, isArgument, reassigned, escapes; // flags
};