/*
enum CollTypes {
    CYNone,
    CYSet,
    CYDict,
    CYArray,
    CYSlice,
    CYFilter,
    CYSelection,
    CYRealRange,
    CYIntRange
};

enum CollSubTypesArray {
    CYStackArray,
    CYStaticArray,
    CYStackPtrArray,
    CYStaticPtrArray,

    CYHeapArray,
    CYHeapPtrArray,
    CYHeapSet,
    CYHeapSortedSet,
    CYHeapOrderedSet,

    CYHeapKDTree



};

enum CollSubTypesDict {
    CYHeapDict,
    CYHeapKDDict,
    CYHeapSortedDict,
    CYHeapOrderedDict
};

enum TypeTypes_basic {
    TYUnresolved,
    TYVoid,
    TYError,
    TYObject,
    TYSizeInt,
    TYString,
    TYNumber,
    TYBool
};





enum TypeSubTypes_Number {
    TYNumberInt64,
    TYNumberInt32,
    TYNumberInt16,
    TYNumberInt8,
    TYNumberUInt64,
    TYNumberUInt32,
    TYNumberUInt16,
    TYNumberUInt8,
    TYNumberReal32,
    TYNumberReal64,
    TYNumberDualReal64,
    TYNumberDualInt64,
    TYNumberDualReal32,
    TYNumberDualInt32,
    TYNumberComplex,
    TYNumberBigInt
};
*/

typedef enum TypeTypes {

    TYUnresolved = 0,

    TYNoType,
    TYAnyType,
    TYObject,

    TYErrorType,

    TYSize,

    TYString,
    TYBool,

    TYInt8,
    TYUInt8,
    TYInt16,
    TYUInt16,
    TYInt32,
    TYUInt32,
    TYInt64,
    TYUInt64,

    TYReal32,
    TYReal64

} TypeTypes;

static bool TypeType_isnum(TypeTypes tyty) { return tyty >= TYInt8; }

static const char* TypeType_name(TypeTypes tyty) {
    switch (tyty) {
    case TYUnresolved:
        return NULL;
    case TYAnyType:
        return "Anything";
    case TYNoType:
        return "Void";
    case TYErrorType:
        return "<invalid>";
    case TYString:
        return "CString";
    case TYBool:
        return "Boolean";
    case TYObject:
        return "";
    case TYSize:
    case TYInt8:
    case TYUInt8:
    case TYInt16:
    case TYUInt16:
    case TYInt32:
    case TYUInt32:
    case TYInt64:
    case TYUInt64:
    case TYReal32:
    case TYReal64:
        return "Number";
    }
}

static const char* TypeType_c_name[] = {
    [TYUnresolved] = "<unresolved>",
    [TYNoType] = "void",
    [TYAnyType] = "(any)",
    [TYErrorType] = "<invalid>",
    [TYString] = "CString",
    [TYBool] = "bool",
    [TYObject] = "<object>",
    [TYSize] = "SizeT",
    [TYInt8] = "Int8",
    [TYUInt8] = "UInt8",
    [TYInt16] = "Int16",
    [TYUInt16] = "UInt16",
    [TYInt32] = "Int32",
    [TYUInt32] = "UInt32",
    [TYInt64] = "Int64",
    [TYUInt64] = "UInt64",
    [TYReal32] = "Float",
    [TYReal64] = "Double",
};

static const char* TypeType_format(TypeTypes tyty, bool quoted) {
    switch (tyty) {
    case TYUnresolved:
    case TYNoType:
    case TYAnyType:
    case TYErrorType:
        return NULL;
    case TYObject:
        return "%p";
    case TYSize:

        return "%lu";
    case TYString:
        return quoted ? "\\\"%s\\\"" : "%s";
    case TYBool:
        return "%d";

    case TYInt8:
        return "%d";
    case TYUInt8:
        return "%u";
    case TYInt16:
        return "%d";
    case TYUInt16:
        return "%u";
    case TYInt32:
        return "%d";
    case TYUInt32:
        return "%u";
    case TYInt64:
        return "%d";
    case TYUInt64:
        return "%u";
    case TYReal32:
        return "%g";
    case TYReal64:
        return "%g";
    }
}

static unsigned int TypeType_size(TypeTypes tyty) {
    switch (tyty) {
    case TYUnresolved:
    case TYNoType:
    case TYAnyType:
    case TYErrorType:
        return 0;
    case TYObject:
        return sizeof(void*);
    case TYSize:
        return sizeof(size_t);
    case TYString:
        return sizeof(char*);
    case TYBool:
        return sizeof(int);
    case TYInt8:
        return 1;
    case TYUInt8:
        return 1;
    case TYInt16:
        return 2;
    case TYUInt16:
        return 2;
    case TYInt32:
        return 4;
    case TYUInt32:
        return 4;
    case TYInt64:
        return 8;
    case TYUInt64:
        return 8;
    case TYReal32:
        return 4;
    case TYReal64:
        return 8;
    }
}

static TypeTypes TypeType_byName(const char* spec) {
    if (!spec) return TYUnresolved;
    if (!strcasecmp(spec, "Number")) return TYReal64;

    if (!strcasecmp(spec, "String")) return TYString;
    if (!strcasecmp(spec, "Boolean")) return TYBool;

    return TYUnresolved;
}

typedef enum CollectionTypes {
    CTYNone = 0,
    CTYArray,
    CTYList,
    CTYDList,
    CTYDictS,
    CTYDictU,
    CTYOrderedDictS,
    CTYSortedDictS,
    CTYOrderedDictU,
    CTYSortedDictU,
    CTYSet,
    CTYOrderedSet,
    CTYSortedSet,
    CTYTensor,
    CTYDataFrame,
    CTYStackArray,
    CTYStackArray8,

    CTYStackArray16,
    CTYStackArray32,
    CTYStackArray64,
    CTYStackArray128,
    CTYStackArray256,
    CTYStackArray512,
    CTYStackArray1024,
    CTYStackArray2048,
    CTYStackArray4096,
} CollectionTypes;

static const char* CollectionType_nativeName(CollectionTypes coty) {
    switch (coty) {
    case CTYNone:
        return "";
    case CTYArray:
        return "Array_";
    case CTYList:
        return "List_";
    case CTYDList:
        return "DList_";
    case CTYDictS:
        return "DictS_";
    case CTYDictU:
        return "DictU_";
    case CTYOrderedDictS:
        return "_o";
    case CTYSortedDictS:
        return "_s";
    case CTYOrderedDictU:
        return "_O";
    case CTYSortedDictU:
        return "_S";
    case CTYSet:
        return "Set_";
    case CTYOrderedSet:
        return "OSet_";
    case CTYSortedSet:
        return "SSet_";
    case CTYTensor:
        return "Tensor_";
    case CTYDataFrame:
        return "_F";
    case CTYStackArray:
        return "[]";
    case CTYStackArray8:
        return "[8]";
    case CTYStackArray16:
        return "[16]";
    case CTYStackArray32:
        return "[32]";
    case CTYStackArray64:
        return "[64]";
    case CTYStackArray128:
        return "[128]";
    case CTYStackArray256:
        return "[256]";
    case CTYStackArray512:
        return "[512]";
    case CTYStackArray1024:
        return "[1024]";
    case CTYStackArray2048:
        return "[2048]";
    case CTYStackArray4096:
        return "[4096]";
    }
}
