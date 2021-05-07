#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>

typedef unsigned long long u64;

// A string.
struct _String {
    char* buf;
    unsigned len, cap;
};

struct _Object;
struct _Array;

// A value: string, number, true, false, object, array, or null.
// null is indicated by the pointer to a  var being nullptr.
// if you have a  var, then all-bits-zero means false.
// struct var {
//     _Object* obj() { return _obj; }
//     _Array* arr() { return _arr; }
//     _String* str() { return _str; }
//     const char* cstr() { return _cstr; }
//     double num() { return _num; }
//     union {
//         _Object* _obj;
//         _Array* _arr;
//         _String* _str;
//         const char* _cstr;
//         double _num;
//         u64 _bits;
// #ifdef FP_ENDIAN_BIG
//         struct {
//             u64 //
//                 _kind==_kinds_F : 1, // bit indicates false when double is
//                 nan
//                 //            _signbit : 1,
//                 _nanbits : 11,
//                 _kind==_kinds_O : 1, // bit indicates object when double is
//                 nan _kind==_kinds_A : 1, // bit indicates array when double
//                 is nan _kind==_kinds_S : 1, // bit indicates string when
//                 double is nan _kind==_kinds_T : 1, // bit indicates true
//                 when double is nan _ptr : 48; // unused (this is the ptr
//                 value btw)
//         };
// #else
//         struct {
//             u64 //
//                 _ptr : 48, // unused (this is the ptr value btw)
//                 _kind==_kinds_F : 1, // bit indicates false when double is
//                 nan _kind==_kinds_O : 1, // bit indicates object when double
//                 is nan _kind==_kinds_A : 1, // bit indicates array when
//                 double is nan _kind==_kinds_S : 1, // bit indicates string
//                 when double is nan _nanbits : 11, _kind==_kinds_T : 1; //
//                 bit indicates true when double is nan
//             //            _signbit : 1; // don't need the top 11 bits
//         };
// #endif
//     };
// };

// typedef  var var;

// TODO: I guess it can be made faster. instead of setting individual bits
// after setting nanbits, you can define masks for B_F, B_O, B_A, etc. which
// includes the nanbits. Then constructing doubles is a simple double copy
// (possibly optimized away) and for pointers it is one additional bitwise or.
// and stop malloc'ing everything!!! var is 8B, pass it around by value.

// o a t f s n -
// ALL BITS 0 IS DOUBLE 0.0 IS IN BAND!!!
// null: all bits are 0, so any member compares true to 0.
// ^^^ no, the pointer itself is nullptr to indicate null. allbits zero is
// false. t: double is inf, bit B_T is set f:  double is inf, bit B_F is set n:
// double is not inf, or double is inf and bit B_T and B_F clear s: double is
// inf, bit B_S is set, unset it to get ptr to string s: double is inf, bit B_C
// is set, unset it to get ptr to cstring o: double is inf, bit B_O is set,
// unset it to get ptr to obj a: double is inf, bit B_A is set, unset it to get
// ptr to array need 5 bits.
struct var;
// A key-value pair.
struct _KeyValue {
    char* key;
    var value;
};

// A linked list of key-value pairs.
struct _Object {
    _KeyValue kv;
    struct _Object* next;

    var operator[](const char* str) { return get(str); }
    // TODO: the func should take a KeyValue pair and not allocate it. Now
    // you allocate using  newPair but it should be up to the caller whether
    // a new allocation is needed or an existing KeyValue pair is to be added
    // --- NO NO NO
    var* setv(char* key, var value) {
        // assert(objp);
        // assert(isObj());

        _Object* objItem = kv.value.obj(); // obj(); // objp. ptr();
        if (!objItem) {
            objItem = newObjectItem({ key, value }, nullptr);
            kv.value._ptr = (u64)objItem;
            return &kv.value._ptr;
        } else {
            do {
                if (!strcmp(objItem->kv.key, key)) {
                    objItem->kv.value = value;
                    return nullptr;
                } else if (!objItem->next) {
                    objItem->next = newObjectItem({ key, value }, nullptr);
                    kv.value._ptr = (u64)objItem->next;
                    return &kv.value._ptr;
                }
                objItem = objItem->next;
            } while (objItem);
        }
        return nullptr;
    }

    var get(const char* key) {
        const char* lkey = key;
        while (1) { // isObj()) {
            // assert(objp. isObj());
            _Object* objItem = this; // obj();
            var* objp = nullptr;
            int keylen = 0;
            while (lkey[keylen] && lkey[keylen] != '.') keylen++;
            // printf("--- objItem: %p\n", objItem);
            while (objItem) {
                // printf("--- %s %s %d\n", objItem->kv.key, lkey, keylen);
                if (!strncmp(objItem->kv.key, lkey, keylen)) {
                    // printf("--- found %s\n", objItem->kv.key);
                    objp = &objItem->kv.value;
                    break;
                }
                objItem = objItem->next;
            }
            lkey += keylen + 1;
            if (!*lkey) return objp;
        }
        return nullptr;
    }
};

// static const var null = var();

// A linked list of values.
struct _Array {
    var value;
    struct _Array* next;

    var operator[](size_t i) { return getAt(i); }
    var getAt(int index) {
        _Array* arri = this;
        while (arri) {
            if (index--) return arri->value;
            arri = arri->next;
        }
        return var();
    }

    // returns an array reference to the last item, so that you can use it for
    // repeated pushes. insert call should do this too
    var push(var newValue) {

        _Array* arrItem = this;
        if (!arrItem) {
            arrItem = newArrayItem(newValue, nullptr);
            value._ptr = (u64)arrItem;
            return value;
        } else {
            do {
                if (!arrItem->next) {
                    arrItem->next = newArrayItem(newValue, nullptr);
                    var ret = {};
                    ret._ptr = (u64)arrItem->next;
                    return ret;
                }
                arrItem = arrItem->next;
            } while (arrItem);
        }
        return var();
    }

    // insert at position 0
    void shift(var value) { value._ptr = (u64)newArrayItem(value, this); }
};

#define setMyFlag(flag)                                                        \
    {                                                                          \
        _nanbits = 0x7FF;                                                      \
        flag = 1;                                                              \
    }
#define setFlag(x, flag)                                                       \
    {                                                                          \
        x._nanbits = 0x7FF;                                                    \
        x.flag = 1;                                                            \
    }
// #define ptr(x) ((void*)(x->_ptr))
// ((void*)(x->bits & ((1ULL << 48) - 1 - 3)))

// A generic dispatch macro would create a dispatcher for any given function
// e.g. `print` here. You need to provide definitions of what to do for a
// string, number, bool, null. The defs for obj and array, as well as the
// dispatch boilerplate will be auto generated by the macro.

#define exists // NONNULL

// for a func e.g.  print
// following is generated:
// public func  print -> calls recursive __ print and does any work
// before and after this
// -> __ print recursive dispatcher.
// -> __ printString works on string values
// -> __ printNumber works on number values
// -> __ printBool works on bool values
// -> __ printNull works on null values
// -> __ printObject runs all member pairs through __ print
// -> __ printArray  runs all elements through __ print
// Except for __<name>Null, __<func> and <func> itself,
// the funcs take NOT  var* but the dereferenced, relevant underlying
// type. i.e. the dispatcher is responsible for ALL the "dispatching".

// Macro rules
// 1. Functions to be generated take any number of arguments, but there
//    is a separate macro to be used for each # of arguments.
// 2. General format is
//        genDispatcherN(funcName, arg1Name..argNName,
//            stringCode, numberCode, boolCode, nullCode)
// 3. Not suitable for specific functions that don't just want the object
//        and array invocations to simply walk their contents.

// BTW if you want to link to standard C structs (they should have a header
// with typeid etc.) you'll need 1 more bit to mark. And you'll need 1 more
// bit to mark a real nan anyway, since now everything is used up there is
// no legal way to actually indicate nan.

#define GEN_DISPATCHER_1(funcName, arg1Name, keyCode, stringCode, numberCode,  \
    boolCode, nullCode, preCode, postCode, objPreCode, objPostCode,            \
    pairPreCode, pairPostCode, arrPreCode, arrPostCode, arrElemPreCode,        \
    arrElemPostCode)                                                           \
                                                                               \
    static void __##funcName(var v, int level);                                \
    static void __##funcName##Pair(const _KeyValue& kv, int level) {           \
        keyCode;                                                               \
        __##funcName(kv.value, level);                                         \
    }                                                                          \
    static void __##funcName##Object(const _Object* obj, int level) {          \
        objPreCode;                                                            \
        while (obj) {                                                          \
            pairPreCode;                                                       \
            __##funcName##Pair(obj->kv, level + 1);                            \
            pairPostCode;                                                      \
            obj = obj->next;                                                   \
        }                                                                      \
        objPostCode;                                                           \
    }                                                                          \
    static void __##funcName##Array(const _Array* arr, int level) {            \
        arrPreCode;                                                            \
        while (arr) {                                                          \
            arrElemPreCode;                                                    \
            __##funcName(arr->value, level + 1);                               \
            arrElemPostCode;                                                   \
            arr = arr->next;                                                   \
        }                                                                      \
        arrPostCode;                                                           \
    }                                                                          \
    static void __##funcName##Str(const char* val) { stringCode; }             \
    static void __##funcName##Bool(bool val) { boolCode; }                     \
    static void __##funcName##Null() { nullCode; }                             \
    static void __##funcName##Number(double val) { numberCode; }               \
    static void __##funcName(var v, int level) {                               \
        if (!v)                                                                \
            __##funcName##Null();                                              \
        else if (!isnan(v.num()))                                              \
            __##funcName##Number(v.num());                                     \
        else if (v.isObj())                                                    \
            __##funcName##Object(v.obj(), level);                              \
        else if (v.isArr())                                                    \
            __##funcName##Array(v.arr(), level);                               \
        else if (v.isStr())                                                    \
            __##funcName##Str(v.cstr());                                       \
        else if (v.isTrue() || v.isFalse())                                    \
            __##funcName##Bool(v.isTrue());                                    \
    }                                                                          \
    static void funcName(var arg1Name) {                                       \
        preCode;                                                               \
        __##funcName(arg1Name, 1);                                             \
        postCode;                                                              \
    }

static const char* const spaces
    = "                                                                   "
      " ";

const int indStep = 4;

GEN_DISPATCHER_1( //
    identify, // name
    arg1, // arg name
    printf("%s: ", kv.key), // key code
    printf("string"), // string code
    printf("number"), // number code
    printf("bool"), // bool code
    printf("null"), // null code
    , // pre code
    puts(""), // post code
    puts("object {"), // obj pre code
    printf("%.*s}", (level - 1) * indStep, spaces), // obj post code
    printf("%.*s", level* indStep, spaces), // pair pre code
    printf("%s", obj->next ? ",\n" : "\n"), // pair post code
    puts("array ["), // array pre code
    printf("%.*s]", (level - 1) * indStep, spaces), // array post code
    printf("%.*s", level* indStep, spaces), // array elem pre code
    printf("%s", arr->next ? ",\n" : "\n") // array elem post code
)

GEN_DISPATCHER_1( //
    print, // name
    arg1, // arg name
    printf("%s: ", kv.key), // key code
    printf("\"%s\"", val), // string code
    printf("%g", val), // number code
    printf("%s", val ? "true" : "false"), // bool code
    printf("null"), // null code
    , // pre code
    puts(""), // post code
    puts("{"), // obj pre code
    printf("%.*s}", (level - 1) * indStep, spaces), // obj post code
    printf("%.*s", level* indStep, spaces), // pair pre code
    printf("%s", obj->next ? ",\n" : "\n"), // pair post code
    printf("["), // array pre code
    printf("]"), // array post code
    , // array elem pre code
    printf("%s", arr->next ? ", " : "") // array elem post code
)

GEN_DISPATCHER_1( //
    printJSON, // name
    arg1, // arg name
    printf("\"%s\": ", kv.key), // key code
    printf("\"%s\"", val), // string code
    printf("%g", val), // number code
    printf("%s", val ? "true" : "false"), // bool code
    printf("null"), // null code
    , // pre code
    puts(""), // post code
    puts("{"), // obj pre code
    printf("%.*s}", (level - 1) * indStep, spaces), // obj post code
    printf("%.*s", level* indStep, spaces), // pair pre code
    printf("%s", obj->next ? ",\n" : "\n"), // pair post code
    puts("["), // array pre code
    printf("%.*s]", (level - 1) * indStep, spaces), // array post code
    printf("%.*s", (level)*indStep, spaces), // array elem pre code
    printf("%s", arr->next ? ",\n" : "\n") // array elem post code
)

// how to expose this macro to   still keeping the struct opaque?
GEN_DISPATCHER_1( //
    printYAML, // name
    arg1, // arg name
    printf("%s: ", kv.key), // key code
    printf("\"%s\"\n", val), // string code
    printf("%g\n", val), // number code
    printf("%s\n", val ? "true" : "false"), // bool code
    printf("null\n"), // null code
    , // pre code
    , // post code
    puts(""), // obj pre code
    , // obj post code
    printf("%.*s", (level - 1) * indStep, spaces), // pair pre code
    , // pair post code
    puts(""), // array pre code
    , // array post code
    printf("%.*s- ", (level - 1) * indStep, spaces), // array elem pre code
                                                     // array elem post code
)

// static _KeyValue newPair(char* key, var value) {
//     return { key, value };
//     // return ret;
// }

static _Object* newObjectItem(_KeyValue kv, _Object* next) {
    _Object* ret = new _Object;
    ret->kv = kv;
    ret->next = next;
    return ret;
}

static _Array* newArrayItem(var value, _Array* next) {
    _Array* ret = new _Array;
    ret->value = value;
    ret->next = next;
    return ret;
}

// -------------------------- PUBLIC STUFF --------------------------------

#include <initializer_list>

struct var {

    _Object* obj() { return _obj; }
    _Array* arr() { return _arr; }
    _String* str() { return _str; }
    const char* cstr() { return _cstr; }
    double num() { return _num; }

    var(std::initializer_list<var> l) {
        _bits = _masks_A;
        pushn(l);
    }
    var(std::initializer_list<_KeyValue> l) { setn(l); }
    void pushn(std::initializer_list<var> l) {
        var lp = *this;
        for (auto& li : l) lp = lp.push(li);
    }
    void setn(std::initializer_list<_KeyValue> l) {
        var lp = *this;
        for (auto& li : l) lp = lp.setv(li.key, li.value);
    }

    bool isNum() { return !isnan(_num); }
    bool isObj() { return isnan(_num) && _kind == _kinds_O; }
    bool isNull() { return isnan(_num) && _kind == _kinds_X; }
    bool isArr() { return isnan(_num) && _kind == _kinds_A; }
    bool isStr() { return isnan(_num) && _kind == _kinds_S; }
    bool isNumStr() { return isnan(_num) && _kind == _kinds_NS; }
    bool isCStr() { return isnan(_num) && _kind == _kinds_C; }
    bool isTrue() { return isnan(_num) && _kind == _kinds_T; }
    bool isFalse() { return isnan(_num) && _kind == _kinds_F; }

    var() { _bits = _masks_X; }
    var(double d) { _num = d; }
    var(int i) { var((double)i); }
    var(bool b) { _bits = b ? _masks_T : _masks_F; }
    var(const char* str) { _bits = _masks_X | ((u64)str & 0xffffffffffff); }

    var operator[](const char* s) { return (*_obj)[s]; }
    var operator[](size_t i) { return (*_arr)[i]; }

    static var object() {
        var ret;
        ret._bits = _masks_O;
        return ret;
    }
    static var object(std::initializer_list<_KeyValue> l) {
        var o = object();
        o.setn(l);
        return o;
    }
    static var object(_KeyValue kv) {
        var o = object();
        o.setv(kv.key, kv.value);
        return o;
    }

    void print() { ::print(*this); }
    void identify() { ::identify(*this); }
    void printJSON() { ::printJSON(*this); }
    void printYAML() { ::printYAML(*this); }

    operator double() { return num(); }
    operator const char*() { return cstr(); }

private:
    enum { // 0 is never used, that will make it inf not nan.
        _kinds_O = 1, // obj
        _kinds_A, // array - NYI -> uses linked list
        _kinds_L, // linked list
        _kinds_T, // true
        _kinds_F, // false
        _kinds_N, // number
        _kinds_NS, // number as string
        _kinds_C, // cstring
        _kinds_S, // string
        _kinds_X, // null
        _kinds_D, // unix datetime
        _kinds_B // base64 data
    };
    enum { // 0 is never used, that will make it inf not nan.
        _masks_O = 0x7ffUL << 52 | ((u64)_kinds_O) << 48, // obj
        _masks_A = 0x7ffUL << 52 | ((u64)_kinds_A) << 48, // array
        _masks_T = 0x7ffUL << 52 | ((u64)_kinds_T) << 48, // true
        _masks_F = 0x7ffUL << 52 | ((u64)_kinds_F) << 48, // false
        _masks_N = 0x7ffUL << 52 | ((u64)_kinds_N) << 48, // number
        _masks_NS = 0x7ffUL << 52 | ((u64)_kinds_NS) << 48, // number as string
        _masks_C = 0x7ffUL << 52 | ((u64)_kinds_C) << 48, // cstring
        _masks_S = 0x7ffUL << 52 | ((u64)_kinds_S) << 48, // string
        _masks_X = 0x7ffUL << 52 | ((u64)_kinds_X) << 48 // null
    };
    union {
        _Object* _obj;
        _Array* _arr;
        _String* _str;
        const char* _cstr;
        double _num;
        u64 _bits;
        struct {
            u64 _ptr : 48, // ptr value
                _kind : 4, _nanbits : 11, _signbit : 1;
        };
    };
};
int main(int argc, char* argv[]) {

    // maisn(argc, argv);
    // user facing   only exposes 1 type  var (alias var)
    // and all user facing functions can only take and return DFValues
    // not internal-use types marked with __. Also internal use funcs marked
    // with __ cannot be exposed.
    // the  var structure should not be exposed: user facing   deals
    // only with opaque type  var* (aka var). That means everything
    // goes on heap.

    var v43 = 42.012;
    var v43s = "Brown fox";
    var vlist = { 9, 8, 7 };
    var slist = { "yui", "opi" };
    var vobj = var::object({
        { "oip", "rest" }, //
        { "uy", 76 } //
    }); // var::object();
    var varr = {}; // var::array();

    // printf("%g\n", v43->num);
    v43.print();
    // printf("%d\n", isnan(v43->num));

    // printf("%d\n", isnan(v43s->num));
    // printf("%s\n", v43s->cstr);
    v43s.print();

    vobj["mykey"] = v43s;
    set(vobj, "someval", v43);
    vobj.print();
    vobj.identify();

    varr.push(v43s);
    push(varr, v43);
    varr.print();
    varr.identify();

    var vc = v43 + v43;
    vc.print();

    var vlkp = get(vobj, "mykey");
    // printf("got: %p\n", vlkp);
    // printf("org: %p\n", v43s);
    // printf("%llu %llu\n", vlkp->bits, v43s->bits);
    vlkp.print();
    vlkp.identify();

    vlkp = get(vobj, "sdhu");
    // printf("got: %p\n", vlkp);
    vlkp.print();

    var vnewobj = var::object();
    set(vnewobj, "blake", vobj);
    set(vnewobj, "uighur", varr);
    set(vnewobj, "merut", v43s);
    vnewobj.identify();
    vnewobj.print();
    vnewobj.printJSON();
    vnewobj.printYAML();

    // array construction is simple
    var arr = { 36, 45, vobj, { 2, 65 }, 76 };

    var obj = var::object(
        { { "six", 6. }, { "five", 5. }, { "four", var::object({ "kuy", 4. }) },
            { "three", 3. }, { "two", 2. } });

    var o2 = var::object({ "obj", obj });
    push(arr, 99.);
    arr.printYAML();
    o2.printYAML();
    var vl = get(o2, "obj.four");
    vl.print();

    // have a root object that holds all vars.
    //  get should be able to handle keys like "m gh.ij" and "mv[7].ty"

    return 0;
}

// how about a dynamic lang that has the above variant like type,
// and is embeddable. Embedding is easy: you need a Context obj and there is
// a Context_load_from(char* sourceFile) or Context_load(char* code) func to
// load a source code and set it up (creates a DLL /.so and loads it)
// and a Context_eval(Context*, char* code) call.
// All calls return a  var, all args are DFValues.
// Context c = Context_load_from("mycode. ")
// Context_eval("rfunc(%s, %d, %o)", "basic str", 32, o)
