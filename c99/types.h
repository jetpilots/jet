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
   ty_unresolved,
   ty_void,
   ty_error,
   ty_object,
   ty_sizeInt,
   ty_string,
   ty_number,
   ty_bool
};

// enum TypeSubTypes_String {

// };

enum TypeSubTypes_Number {
   ty_numberInt64,
   ty_numberInt32,
   ty_numberInt16,
   ty_numberInt8,
   ty_numberUInt64,
   ty_numberUInt32,
   ty_numberUInt16,
   ty_numberUInt8,
   ty_numberReal32,
   ty_numberReal64,
   ty_numberDualReal64,
   ty_numberDualInt64,
   ty_numberDualReal32,
   ty_numberDualInt32,
   ty_numberComplex,
   ty_numberBigInt
};
*/
// enum TypeSubTypes_ {};

// TODO: ast_typespecs_t will have a typetype_e typeType; that can be used
// to determine quickly if it is a primitive. fits in 4bits btw
typedef enum typetype_e {
    // FIXME: this should fit in 4bit max!!!! or not?
    ty_unresolved = 0, // unknown type
    // nonprimitives: means they should have their own
    // methods to print,
    // serialise, identify, reflect, etc.
    ty_noType, // void
    ty_nilType, // passes type validation with anything. be careful!
    ty_object, // resolved to an ast_type_t
    // primitives that can be printed or represented with no fuss
    ty_errorType, // use this to poison an expr which has a type error
    // and the error has been reported, to avoid
    // reporting the same error for all parents, as the
    // error-checking routine unwinds.
    ty_size, // this is actually uintptr_t, since actual ptrs are
    // ty_objects. maybe rename it
    ty_string, // need to distinguish String and char*?
    ty_bool,
    // above this, ie. > 4 or >= ty_int8, all may have units |kg.m/s etc.
    ty_int8,
    ty_uInt8, //
    ty_int16,
    ty_uInt16,
    ty_int32,
    ty_uInt32,
    ty_int64, // for loop indices start out as size_t by default
    ty_uInt64,
    //  ty_real16,
    ty_real32,
    ty_real64 // Numbers start out with Real64 by default
    // conplex, duals, intervals,etc.??
} typetype_e;

static bool typetype_e_isnum(typetype_e tyty) { return tyty >= ty_int8; }

static const char* typetype_e_name(typetype_e tyty) {
    switch (tyty) {
    case ty_unresolved: return NULL;
    case ty_nilType: return "Anything";
    case ty_noType: return "Void";
    case ty_errorType: return "<invalid>";
    case ty_string: return "CString";
    case ty_bool: return "Boolean";
    case ty_object: return "";
    case ty_size:
    case ty_int8:
    case ty_uInt8:
    case ty_int16:
    case ty_uInt16:
    case ty_int32:
    case ty_uInt32:
    case ty_int64:
    case ty_uInt64:
    case ty_real32:
    case ty_real64: return "Number";
    }
}

static const char* typetype_e_c_name[] = {
    [ty_unresolved] = "<unresolved>",
    [ty_noType] = "void",
    [ty_nilType] = "(any)",
    [ty_errorType] = "<invalid>",
    [ty_string] = "CString",
    [ty_bool] = "bool",
    [ty_object] = "<object>",
    [ty_size] = "SizeT",
    [ty_int8] = "Int8",
    [ty_uInt8] = "UInt8",
    [ty_int16] = "Int16",
    [ty_uInt16] = "UInt16",
    [ty_int32] = "Int32",
    [ty_uInt32] = "UInt32",
    [ty_int64] = "Int64",
    [ty_uInt64] = "UInt64",
    [ty_real32] = "Float",
    [ty_real64] = "Double",
};

// these are DEFAULTS
static const char* typetype_e_format(typetype_e tyty, bool quoted) {
    switch (tyty) {
    case ty_unresolved:
    case ty_noType:
    case ty_nilType:
    case ty_errorType: return NULL;
    case ty_object: return "%p";
    case ty_size: // this is actually uintptr_t, since actual ptrs are
                  // ty_objects. maybe rename it
        return "%lu";
    case ty_string: return quoted ? "\\\"%s\\\"" : "%s";
    case ty_bool:
        return "%d";
        // above this, ie. > 4 or >= ty_int8, all may have units |kg.m/s etc.
        // so you can test tyty >= ty_int8 to see if *units must be
        // processed.
    case ty_int8: return "%d";
    case ty_uInt8: return "%u";
    case ty_int16: return "%d";
    case ty_uInt16: return "%u";
    case ty_int32: return "%d";
    case ty_uInt32: return "%u";
    case ty_int64: // for loop indices start out as size_t by default
        return "%d";
    case ty_uInt64: return "%u";
    case ty_real32: return "%g";
    case ty_real64: // Numbers start out with Real64 by default
        return "%g";
    }
}

// needed to compute stack usage
static unsigned int typetype_e_size(typetype_e tyty) {
    switch (tyty) {
    case ty_unresolved:
    case ty_noType:
    case ty_nilType:
    case ty_errorType: return 0;
    case ty_object: return sizeof(void*);
    case ty_size: return sizeof(size_t);
    case ty_string: return sizeof(char*); // what about length
    case ty_bool: return sizeof(int);
    case ty_int8: return 1;
    case ty_uInt8: return 1;
    case ty_int16: return 2;
    case ty_uInt16: return 2;
    case ty_int32: return 4;
    case ty_uInt32: return 4;
    case ty_int64: return 8;
    case ty_uInt64: return 8;
    case ty_real32: return 4;
    case ty_real64: return 8;
    }
}

// If we get entirely rid of type annotation, the process for determining
// type as it is now will be very convoluted. First you pass strings around
// and compare them ("String", "Number", etc.) and then set typetype_e
// according to that why not set the right typetype_e directly in analysis?
// but this is needed as long as there is ANY type annotation somewhere.
// (e.g. func args)
static typetype_e typetype_e_byName(const char* spec) {
    if (!spec) return ty_unresolved;
    if (!strcasecmp(spec, "Number"))
        return ty_real64; // this is default, analysis might change it to
                          // more specific
    if (!strcasecmp(spec, "String")) return ty_string;
    if (!strcasecmp(spec, "Boolean")) return ty_bool;
    // note that Vector, Matrix, etc. are actually types, so they should
    // resolve to ty_object.
    return ty_unresolved;
}

// says what exactly a collection should generate to, since in check
// source code, collections' actual kind is abstracted away. 4 bits.
typedef enum collectiontype_e {
    cty_none = 0, // Number value
    cty_array,
    cty_list,
    cty_dList,
    cty_dictS,
    cty_dictU,
    cty_orderedDictS, // String keys
    cty_sortedDictS, // UInt/Int/Ptr keys
    cty_orderedDictU,
    cty_sortedDictU,
    cty_set,
    cty_orderedSet,
    cty_sortedSet,
    cty_tensor, // will need to store dims. includes vector/matrix/tensor
    cty_dataFrame,
    cty_stackArray, // computed size from init. can get later using countof()
    cty_stackArray8, // these are NOT in BYTES, but sizeof(whatever), so
                     // careful with double/int arrays
    cty_stackArray16,
    cty_stackArray32,
    cty_stackArray64,
    cty_stackArray128,
    cty_stackArray256,
    cty_stackArray512,
    cty_stackArray1024,
    cty_stackArray2048,
    cty_stackArray4096, // really? you need any larger?
} collectiontype_e;

static const char* collectiontype_e_nativeName(collectiontype_e coty) {
    switch (coty) {
    case cty_none: return "";
    case cty_array: return "Array_";
    case cty_list: return "List_";
    case cty_dList: return "DList_";
    case cty_dictS: return "DictS_";
    case cty_dictU: return "DictU_";
    case cty_orderedDictS: return "_o";
    case cty_sortedDictS: return "_s";
    case cty_orderedDictU: return "_O";
    case cty_sortedDictU: return "_S";
    case cty_set: return "Set_";
    case cty_orderedSet: return "OSet_";
    case cty_sortedSet: return "SSet_";
    case cty_tensor: return "Tensor_";
    case cty_dataFrame: return "_F";
    case cty_stackArray: return "[]";
    case cty_stackArray8: return "[8]";
    case cty_stackArray16: return "[16]";
    case cty_stackArray32: return "[32]";
    case cty_stackArray64: return "[64]";
    case cty_stackArray128: return "[128]";
    case cty_stackArray256: return "[256]";
    case cty_stackArray512: return "[512]";
    case cty_stackArray1024: return "[1024]";
    case cty_stackArray2048: return "[2048]";
    case cty_stackArray4096: return "[4096]";
    }
}
