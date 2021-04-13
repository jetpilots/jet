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
    TYNone,
    TYError,
    TYString,
    TYCString,
    TYFixedString,
    TYBoolean,
    TYInt8,
    TYUint8,
    TYInt16,
    TYUint16,
    TYInt32,
    TYUint32,
    TYInt64,
    TYUint64,
    TYReal32,
    TYReal64
};
struct Type;
typedef unsigned long long u64;
struct TypeInfo {
    union {
        const char* _name;
        Type* _type;
        struct {
            char _ptr[6];
            TypeType typeType : 5;
            bool elemental : 1;
            CollectionType collectionType : 4;
            unsigned char dims : 4;
        };
    };
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
static_assert(sizeof(TypeInfo) == 8, "");

struct Var {
    SmallString name;
    TypeInfo typeInfo;
    unsigned char changes;
    bool used, isMutable, isArgument; // flags
};