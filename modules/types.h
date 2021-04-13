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
   // ^ Stack/static arrays are non-resizable and of compile-time known size
   CYHeapArray,
   CYHeapPtrArray,
   CYHeapSet,
   CYHeapSortedSet,
   CYHeapOrderedSet,
   // ^ Set is grouped with Array and not Dict since "feels like" an array
   CYHeapKDTree
   // ^ independent KD-tree (1D). Otherwise arrays may have a ref to a KD-tree
   // when you want both structured array AND fast neighbour search. However
   // sometimes you just want search (e.g. point cloud), so you can use this
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

// enum TypeSubTypes_String {

// };

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
// enum TypeSubTypes_ {};

// TODO: ASTTypeSpecs will have a TypeTypes typeType; that can be used
// to determine quickly if it is a primitive. fits in 4bits btw
typedef enum TypeTypes {
    // FIXME: this should fit in 4bit max!!!! or not?
    TYUnresolved = 0, // unknown type
    // nonprimitives: means they should have their own
    // methods to print,
    // serialise, identify, reflect, etc.
    TYNoType, // void
    TYNilType, // passes type validation with anything. be careful!
    TYObject, // resolved to an ASTType
    // primitives that can be printed or represented with no fuss
    TYErrorType, // use this to poison an expr which has a type error
    // and the error has been reported, to avoid
    // reporting the same error for all parents, as the
    // error-checking routine unwinds.
    TYSize, // this is actually uintptr_t, since actual ptrs are
    // TYObjects. maybe rename it
    TYString, // need to distinguish String and char*?
    TYBool,
    // above this, ie. > 4 or >= TYInt8, all may have units |kg.m/s etc.
    TYInt8,
    TYUInt8, //
    TYInt16,
    TYUInt16,
    TYInt32,
    TYUInt32,
    TYInt64, // for loop indices start out as size_t by default
    TYUInt64,
    //  TYReal16,
    TYReal32,
    TYReal64 // Numbers start out with Real64 by default
    // conplex, duals, intervals,etc.??
} TypeTypes;

static bool TypeType_isnum(TypeTypes tyty) { return tyty >= TYInt8; }

static const char* TypeType_name(TypeTypes tyty) {
    switch (tyty) {
    case TYUnresolved: return NULL;
    case TYNilType: return "Anything";
    case TYNoType: return "Void";
    case TYErrorType: return "<invalid>";
    case TYString: return "CString";
    case TYBool: return "Boolean";
    case TYObject: return "";
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
    case TYReal64: return "Number";
    }
}

static const char* TypeType_c_name[] = {
    [TYUnresolved] = "<unresolved>",
    [TYNoType] = "void",
    [TYNilType] = "(any)",
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

// these are DEFAULTS
static const char* TypeType_format(TypeTypes tyty, bool quoted) {
    switch (tyty) {
    case TYUnresolved:
    case TYNoType:
    case TYNilType:
    case TYErrorType: return NULL;
    case TYObject: return "%p";
    case TYSize: // this is actually uintptr_t, since actual ptrs are
                 // TYObjects. maybe rename it
        return "%lu";
    case TYString: return quoted ? "\\\"%s\\\"" : "%s";
    case TYBool:
        return "%d";
        // above this, ie. > 4 or >= TYInt8, all may have units |kg.m/s etc.
        // so you can test tyty >= TYInt8 to see if *units must be
        // processed.
    case TYInt8: return "%d";
    case TYUInt8: return "%u";
    case TYInt16: return "%d";
    case TYUInt16: return "%u";
    case TYInt32: return "%d";
    case TYUInt32: return "%u";
    case TYInt64: // for loop indices start out as size_t by default
        return "%d";
    case TYUInt64: return "%u";
    case TYReal32: return "%g";
    case TYReal64: // Numbers start out with Real64 by default
        return "%g";
    }
}

// needed to compute stack usage
static unsigned int TypeType_size(TypeTypes tyty) {
    switch (tyty) {
    case TYUnresolved:
    case TYNoType:
    case TYNilType:
    case TYErrorType: return 0;
    case TYObject: return sizeof(void*);
    case TYSize: return sizeof(size_t);
    case TYString: return sizeof(char*); // what about length
    case TYBool: return sizeof(int);
    case TYInt8: return 1;
    case TYUInt8: return 1;
    case TYInt16: return 2;
    case TYUInt16: return 2;
    case TYInt32: return 4;
    case TYUInt32: return 4;
    case TYInt64: return 8;
    case TYUInt64: return 8;
    case TYReal32: return 4;
    case TYReal64: return 8;
    }
}

// If we get entirely rid of type annotation, the process for determining
// type as it is now will be very convoluted. First you pass strings around
// and compare them ("String", "Number", etc.) and then set TypeTypes
// according to that why not set the right TypeTypes directly in analysis?
// but this is needed as long as there is ANY type annotation somewhere.
// (e.g. func args)
static TypeTypes TypeType_byName(const char* spec) {
    if (!spec) return TYUnresolved;
    if (!strcasecmp(spec, "Number"))
        return TYReal64; // this is default, analysis might change it to
                         // more specific
    if (!strcasecmp(spec, "String")) return TYString;
    if (!strcasecmp(spec, "Boolean")) return TYBool;
    // note that Vector, Matrix, etc. are actually types, so they should
    // resolve to TYObject.
    return TYUnresolved;
}

// says what exactly a collection should generate to, since in check
// source code, collections' actual kind is abstracted away. 4 bits.
typedef enum CollectionTypes {
    CTYNone = 0, // Number value
    CTYArray,
    CTYList,
    CTYDList,
    CTYDictS,
    CTYDictU,
    CTYOrderedDictS, // String keys
    CTYSortedDictS, // UInt/Int/Ptr keys
    CTYOrderedDictU,
    CTYSortedDictU,
    CTYSet,
    CTYOrderedSet,
    CTYSortedSet,
    CTYTensor, // will need to store dims. includes vector/matrix/tensor
    CTYDataFrame,
    CTYStackArray, // computed size from init. can get later using countof()
    CTYStackArray8, // these are NOT in BYTES, but sizeof(whatever), so
                    // careful with double/int arrays
    CTYStackArray16,
    CTYStackArray32,
    CTYStackArray64,
    CTYStackArray128,
    CTYStackArray256,
    CTYStackArray512,
    CTYStackArray1024,
    CTYStackArray2048,
    CTYStackArray4096, // really? you need any larger?
} CollectionTypes;

static const char* CollectionType_nativeName(CollectionTypes coty) {
    switch (coty) {
    case CTYNone: return "";
    case CTYArray: return "Array_";
    case CTYList: return "List_";
    case CTYDList: return "DList_";
    case CTYDictS: return "DictS_";
    case CTYDictU: return "DictU_";
    case CTYOrderedDictS: return "_o";
    case CTYSortedDictS: return "_s";
    case CTYOrderedDictU: return "_O";
    case CTYSortedDictU: return "_S";
    case CTYSet: return "Set_";
    case CTYOrderedSet: return "OSet_";
    case CTYSortedSet: return "SSet_";
    case CTYTensor: return "Tensor_";
    case CTYDataFrame: return "_F";
    case CTYStackArray: return "[]";
    case CTYStackArray8: return "[8]";
    case CTYStackArray16: return "[16]";
    case CTYStackArray32: return "[32]";
    case CTYStackArray64: return "[64]";
    case CTYStackArray128: return "[128]";
    case CTYStackArray256: return "[256]";
    case CTYStackArray512: return "[512]";
    case CTYStackArray1024: return "[1024]";
    case CTYStackArray2048: return "[2048]";
    case CTYStackArray4096: return "[4096]";
    }
}
