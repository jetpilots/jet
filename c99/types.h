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
   // ^ independent KD-tree (1D). Otherwise arrays may have a ref to a
KD-tree
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
   TYUnknown,
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

// TODO: TypeSpecs will have a TypeTypes typeType; that can be used
// to determine quickly if it is a primitive. fits in 4bits btw
typedef enum TypeTypes {
  // FIXME: this should fit in 4bit max!!!! or not?
  TYUnknown = 0, // unknown type
  // nonprimitives: means they should have their own
  // methods to print,
  // serialise, identify, reflect, etc.
  // TYOpen, // open for inference -> tyunknown is this
  TYVoid,   // void
  TYNil,    // passes type validation with anything. be careful!
  TYObject, // resolved to an Type
  // primitives that can be printed or represented with no fuss
  TYFuncPtr,
  TYError, // use this to poison an expr which has a type error
  // and the error has been reported, to avoid
  // reporting the same error for all parents, as the
  // error-checking routine unwinds.
  // This is needed in addition to TYUnknown because that also indicates
  // something is open for non-local inference. If you instead poison
  // something with TYError it will not be considered for nonlocal
  // inference.
  TYDateTime,    // this should have subtypes TYDateTimeSeconds and
  TYDateTimeSec, // based on usage patterns the right one
  // is applied (for just > >= etc. seconds from epoch is enough)
  TYRange,  //
  TYPtrInt, // this is actually uintptr_t, since actual ptrs are
  TYSize2D, // this is actually uintptr_t, since actual ptrs are
  // TYObjects. maybe rename it
  TYPoint2D, // 200x200
  TYRect,    // 0x0:150x150. A range of 2 2D points is a... rect!
  TYRegex,
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
  TYReal64, // Numbers start out with Real64 by default
  TYDual32, // for forward mode AD
  TYDual64,
  TYActive32, // for reverse mode AD
  TYActive64,
  TYComplex
  // conplex, duals, intervals,etc.??
} TypeTypes;

static bool Typetype_isnum(TypeTypes tyty) { return tyty >= TYInt8; }

static const char* Typetype_name(TypeTypes tyty) {
  switch (tyty) {
  case TYUnknown: return NULL;
  case TYNil: return "Nil";
  case TYVoid: return "void";
  case TYError: return "<invalid>";
  case TYString: return "CString";
  case TYBool: return "Boolean";
  case TYFuncPtr: return "func";
  case TYRegex: return "Regex";
  case TYObject: return "";
  case TYPtrInt:
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
  case TYDual32:
  case TYDual64:
  case TYActive32:
  case TYActive64: return "Number";
  case TYDateTime:
  case TYDateTimeSec: return "DateTime";
  case TYSize2D: return "ui.Size";
  case TYPoint2D: return "ui.Point";
  case TYRect: return "ui.Rect";
  case TYRange: return "Range";
  case TYComplex: return "Complex";
  }
  return "<unknown>";
}

static const char* Typetype_c_name[] = { [TYUnknown] = "<unknown>",
  [TYVoid] = "void",
  [TYNil] = "(nil)",
  [TYError] = "<invalid>",
  [TYString] = "String",
  [TYBool] = "bool",
  [TYRegex] = "Regex",
  [TYObject] = "<object>",
  [TYFuncPtr] = "<func>",
  [TYPtrInt] = "SizeT",
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
  [TYDateTime] = "DateTime",
  [TYDateTimeSec] = "DateTimeSec",
  [TYRange] = "Range",
  [TYSize2D] = "Size2D",
  [TYPoint2D] = "Point2D",
  [TYRect] = "Rect" };

// these are DEFAULTS
static const char* Typetype_format(TypeTypes tyty, bool quoted) {
  switch (tyty) {
  case TYUnknown:
  case TYVoid:
  case TYNil:
  case TYError: return NULL;
  case TYFuncPtr:
  case TYObject: return "%p";
  case TYPtrInt: // this is actually uintptr_t, since actual ptrs are
                 // TYObjects. maybe rename it
    return "%lu";
  case TYString: return quoted ? "\\\"%s\\\"" : "%s";
  case TYRegex: return "%s";
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
  case TYReal32:
  case TYReal64:
    return "%g";
    // Numbers start out with Real64 by default
  case TYDateTime:
  case TYDateTimeSec:
  case TYRange:
  case TYSize2D:
  case TYPoint2D:
  case TYRect:
  case TYDual32:
  case TYDual64:
  case TYActive32:
  case TYActive64:
  case TYComplex: return "%s";
  }
  return "%p";
}

// needed to compute stack usage or sort type members
static unsigned int Typetype_size(TypeTypes tyty) {
  switch (tyty) {
  case TYUnknown:
  case TYVoid:
  case TYNil:
  case TYError: return 0;
  case TYFuncPtr:
  case TYObject: return sizeof(void*);
  case TYPtrInt: return sizeof(size_t);
  case TYRegex: return sizeof(char*);
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
  case TYDateTime:
  case TYDateTimeSec:
  case TYRange:
  case TYSize2D:
  case TYPoint2D:
  case TYRect: return 8;
  case TYDual32:
  case TYDual64:
  case TYActive32:
  case TYActive64:
  case TYComplex: return 16;
  }
  return 0;
}

// If we get entirely rid of type annotation, the process for determining
// type as it is now will be very convoluted. First you pass strings around
// and compare them ("String", "Number", etc.) and then set TypeTypes
// according to that why not set the right TypeTypes directly in analysis?
// but this is needed as long as there is ANY type annotation somewhere.
// (e.g. func args)
static TypeTypes Typetype_byName(const char* spec) {
  if (!spec) return TYUnknown;
  if (!strcasecmp(spec, "Number")) return TYReal64;
  // this is default, analysis might change it to
  // more specific
  if (!strcasecmp(spec, "String")) return TYString;
  if (!strcasecmp(spec, "Regex")) return TYRegex;
  if (!strcasecmp(spec, "DateTime")) return TYDateTime;
  if (!strcasecmp(spec, "Boolean")) return TYBool;
  // note that Vector, Matrix, etc. are actually types, so they should
  // resolve to TYObject.
  return TYUnknown;
}

// says what exactly a collection should generate to, since in check
// source code, collections' actual kind is abstracted away. 4 bits.
typedef enum CollectionTypes {
  CTYNone = 0, // Number value
  CTYRange,
  CTYArray,
  CTYList,
  CTYDList,
  CTYDict,
  CTYDictU,
  CTYOrderedDictS, // String keys
  CTYSortedDictS,  // UInt/Int/VPtr keys
  CTYOrderedDictU,
  CTYSortedDictU,
  CTYSet,
  CTYOrderedSet,
  CTYSortedSet,
  CTYTensor,   // will need to store dims. includes vector/matrix/tensor
  CTYIterator, // funcs that yield
  CTYDataFrame,
  CTYStackArray,  // computed size from init. can get later using countof()
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

#define TRYMATCH(s)                                                        \
  if (!strncasecmp(#s, name, cstr_len(#s))) return CTY##s;

CollectionTypes Collectiontype_byName(char* name) {
  TRYMATCH(None) // N
  TRYMATCH(Array)
  TRYMATCH(List)
  TRYMATCH(DList)
  TRYMATCH(Dict)
  TRYMATCH(DictU)
  TRYMATCH(OrderedDictS)
  TRYMATCH(SortedDictS)
  TRYMATCH(OrderedDictU)
  TRYMATCH(SortedDictU)
  TRYMATCH(Set)
  TRYMATCH(OrderedSet)
  TRYMATCH(SortedSet)
  TRYMATCH(Tensor) // wil
  TRYMATCH(DataFrame)
  TRYMATCH(Iterator)
  TRYMATCH(StackArray) //
  TRYMATCH(StackArray8)
  TRYMATCH(StackArray16)
  TRYMATCH(StackArray32)
  TRYMATCH(StackArray64)
  TRYMATCH(StackArray128)
  TRYMATCH(StackArray256)
  TRYMATCH(StackArray512)
  TRYMATCH(StackArray1024)
  TRYMATCH(StackArray2048)
  TRYMATCH(StackArray4096)
  return CTYNone;
}

static const char* Collectiontype_name(CollectionTypes coty) {
  switch (coty) {
  case CTYNone: return "";
  case CTYRange: return "Range";
  case CTYArray: return "Array";
  case CTYList: return "List";
  case CTYDList: return "DList";
  case CTYDict: return "Dict";
  case CTYDictU: return "DictU";
  case CTYOrderedDictS: return "OrderedDict";
  case CTYSortedDictS: return "_s";
  case CTYOrderedDictU: return "_O";
  case CTYSortedDictU: return "_S";
  case CTYSet: return "Set";
  case CTYOrderedSet: return "OSet";
  case CTYSortedSet: return "SSet";
  case CTYTensor: return "Tensor";
  case CTYIterator: return "Iterator";
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
  return "CTY";
}

// static int Collectiontype_dims(CollectionTypes coty) {
//   switch (coty) {
//   case CTYNone: return 0;
//   case CTYArray: return 1;
//   case CTYList: return 1;
//   case CTYDList: return 1;
//   case CTYDictS: return 1;
//   case CTYDictU: return 1;
//   case CTYOrderedDictS: return 1;
//   case CTYSortedDictS: return 1;
//   case CTYOrderedDictU: return 1;
//   case CTYSortedDictU: return 1;
//   case CTYSet: return 1;
//   case CTYOrderedSet: return 1;
//   case CTYSortedSet: return 1;
//   case CTYTensor: return "Tensor_";
//   case CTYDataFrame: return "_F";
//   case CTYStackArray: return "[]";
//   case CTYStackArray8: return "[8]";
//   case CTYStackArray16: return "[16]";
//   case CTYStackArray32: return "[32]";
//   case CTYStackArray64: return "[64]";
//   case CTYStackArray128: return "[128]";
//   case CTYStackArray256: return "[256]";
//   case CTYStackArray512: return "[512]";
//   case CTYStackArray1024: return "[1024]";
//   case CTYStackArray2048: return "[2048]";
//   case CTYStackArray4096: return "[4096]";
//   }
//   return "CTY";
// }
