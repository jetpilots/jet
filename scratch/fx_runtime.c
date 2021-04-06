#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>

typedef unsigned long long u64;

// A string.
typedef struct fx__String {
    char* buf;
    size_t len, cap;
} fx__String;

typedef struct fx__Object fx__Object;
typedef struct fx__Array fx__Array;

// A value: string, number, true, false, object, array, or null.
// null is indicated by the pointer to a fx__Value being NULL.
// if you have a fx__Value, then all-bits-zero means false.
typedef union fx__Value {
    fx__Object* obj;
    fx__Array* arr;
    fx__String* str;
    const char* cstr;
    double num;
    u64 bits;
#ifdef FP_ENDIAN_BIG
    struct {
        u64 //
            _B_F : 1, // bit indicates false when double is nan
            //            _signbit : 1,
            _nanbits : 11,
            _B_O : 1, // bit indicates object when double is nan
            _B_A : 1, // bit indicates array when double is nan
            _B_S : 1, // bit indicates string when double is nan
            _B_T : 1, // bit indicates true when double is nan
            _ptr : 48; // unused (this is the ptr value btw)
    };
#else
    struct {
        u64 //
            _ptr : 48, // unused (this is the ptr value btw)
            _B_F : 1, // bit indicates false when double is nan
            _B_O : 1, // bit indicates object when double is nan
            _B_A : 1, // bit indicates array when double is nan
            _B_S : 1, // bit indicates string when double is nan
            _nanbits : 11,
            _B_T : 1; // bit indicates true when double is nan
        //            _signbit : 1; // don't need the top 11 bits
    };
#endif
} fx__Value;
typedef fx__Value* var;

// TODO: I guess it can be made faster. instead of setting individual bits
// after setting nanbits, you can define masks for B_F, B_O, B_A, etc. which
// includes the nanbits. Then constructing doubles is a simple double copy
// (possibly optimized away) and for pointers it is one additional bitwise or.
// and stop malloc'ing everything!!! var is 8B, pass it around by value.

// o a t f s n -
// ALL BITS 0 IS DOUBLE 0.0 IS IN BAND!!!
// null: all bits are 0, so any member compares true to 0.
// ^^^ no, the pointer itself is NULL to indicate null. allbits zero is false.
// t: double is inf, bit B_T is set
// f:  double is inf, bit B_F is set
// n: double is not inf, or double is inf and bit B_T and B_F clear
// s: double is inf, bit B_S is set, unset it to get ptr to string
// s: double is inf, bit B_C is set, unset it to get ptr to cstring
// o: double is inf, bit B_O is set, unset it to get ptr to obj
// a: double is inf, bit B_A is set, unset it to get ptr to array
// need 5 bits.

// A key-value pair.
typedef struct fx__KeyValue {
    char* key;
    fx__Value* value;
} fx__KeyValue;

// A linked list of key-value pairs.
struct fx__Object {
    fx__KeyValue* kv;
    struct fx__Object* next;
};

// A linked list of values.
struct fx__Array {
    fx__Value* value;
    struct fx__Array* next;
};

#define fx_setFlag(x, flag)                                                    \
    {                                                                          \
        x->_nanbits = 0x7FF;                                                   \
        x->flag = 1;                                                           \
    }

#define fx_ptr(x) ((void*)(x->_ptr))
// ((void*)(x->bits & ((1ULL << 48) - 1 - 3)))

// A generic dispatch macro would create a dispatcher for any given function
// e.g. `print` here. You need to provide definitions of what to do for a
// string, number, bool, null. The defs for obj and array, as well as the
// dispatch boilerplate will be auto generated by the macro.

#define exists // NONNULL

// for a func e.g. fx_print
// following is generated:
// public func fx_print -> calls recursive __fx_print and does any work before
// and after this
// -> __fx_print recursive dispatcher.
// -> __fx_printString works on string values
// -> __fx_printNumber works on number values
// -> __fx_printBool works on bool values
// -> __fx_printNull works on null values
// -> __fx_printObject runs all member pairs through __fx_print
// -> __fx_printArray  runs all elements through __fx_print
// Except for __<name>Null, __<func> and <func> itself,
// the funcs take NOT fx__Value* but the dereferenced, relevant underlying
// type. i.e. the dispatcher is responsible for ALL the "dispatching".

// Macro rules
// 1. Functions to be generated take any number of arguments, but there
//    is a separate macro to be used for each # of arguments.
// 2. General format is
//        genDispatcherN(funcName, arg1Name..argNName,
//            stringCode, numberCode, boolCode, nullCode)
// 3. Not suitable for specific functions that don't just want the object
//        and array invocations to simply walk their contents.

// BTW if you want to link to standard C structs (they should have a header with
// typeid etc.) you'll need 1 more bit to mark. And you'll need 1 more bit to
// mark a real nan anyway, since now everything is used up there is no legal way
// to actually indicate nan.

#define GEN_DISPATCHER_1(funcName, arg1Name, keyCode, stringCode, numberCode,  \
    boolCode, nullCode, preCode, postCode, objPreCode, objPostCode,            \
    pairPreCode, pairPostCode, arrPreCode, arrPostCode, arrElemPreCode,        \
    arrElemPostCode)                                                           \
                                                                               \
    static void __##funcName(fx__Value* v, int level);                         \
    static void __##funcName##Pair(const exists fx__KeyValue* kv, int level) { \
        keyCode;                                                               \
        __##funcName(kv->value, level);                                        \
    }                                                                          \
    static void __##funcName##Object(                                          \
        const exists fx__Object* obj, int level) {                             \
        objPreCode;                                                            \
        while (obj) {                                                          \
            pairPreCode;                                                       \
            __##funcName##Pair(obj->kv, level + 1);                            \
            pairPostCode;                                                      \
            obj = obj->next;                                                   \
        }                                                                      \
        objPostCode;                                                           \
    }                                                                          \
    static void __##funcName##Array(const exists fx__Array* arr, int level) {  \
        arrPreCode;                                                            \
        while (arr) {                                                          \
            arrElemPreCode;                                                    \
            __##funcName(arr->value, level + 1);                               \
            arrElemPostCode;                                                   \
            arr = arr->next;                                                   \
        }                                                                      \
        arrPostCode;                                                           \
    }                                                                          \
    static void __##funcName##Str(char* val) { stringCode; }                   \
    static void __##funcName##Bool(bool val) { boolCode; }                     \
    static void __##funcName##Null() { nullCode; }                             \
    static void __##funcName##Number(double val) { numberCode; }               \
    static void __##funcName(fx__Value* v, int level) {                        \
        if (!v)                                                                \
            __##funcName##Null();                                              \
        else if (!isnan(v->num))                                               \
            __##funcName##Number(v->num);                                      \
        else if (v->_B_O)                                                      \
            __##funcName##Object(fx_ptr(v), level);                            \
        else if (v->_B_A)                                                      \
            __##funcName##Array(fx_ptr(v), level);                             \
        else if (v->_B_S)                                                      \
            __##funcName##Str(fx_ptr(v));                                      \
        else if (v->_B_T || v->_B_F)                                           \
            __##funcName##Bool(v->_B_T);                                       \
    }                                                                          \
    static void funcName(fx__Value* arg1Name) {                                \
        preCode;                                                               \
        __##funcName(arg1Name, 1);                                             \
        postCode;                                                              \
    }

static const char* const spaces
    = "                                                                    ";
typedef char bool;
const int indStep = 4;

GEN_DISPATCHER_1( //
    fx_identify, // name
    arg1, // arg name
    printf("%s: ", kv->key), // key code
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
    fx_print, // name
    arg1, // arg name
    printf("%s: ", kv->key), // key code
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
    fx_printJSON, // name
    arg1, // arg name
    printf("\"%s\": ", kv->key), // key code
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

// how to expose this macro to API still keeping the struct opaque?
GEN_DISPATCHER_1( //
    fx_printYAML, // name
    arg1, // arg name
    printf("%s: ", kv->key), // key code
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

static fx__KeyValue* fx_newPair(char* key, fx__Value* value) {
    fx__KeyValue* ret = malloc(sizeof(fx__KeyValue));
    ret->key = key;
    ret->value = value;
    return ret;
}

static fx__Object* fx_newObjectItem(fx__KeyValue* kv, fx__Object* next) {
    fx__Object* ret = malloc(sizeof(fx__Object));
    ret->kv = kv;
    ret->next = next;
    return ret;
}

static fx__Array* fx_newArrayItem(fx__Value* value, fx__Array* next) {
    fx__Array* ret = malloc(sizeof(fx__Array));
    ret->value = value;
    ret->next = next;
    return ret;
}

// -------------------------- PUBLIC STUFF --------------------------------
#define API

API bool fx_isNum(var x) { return !isnan(x->num); }
API bool fx_isObj(var x) { return isnan(x->num) && x->_B_O; }
API bool fx_isArr(var x) { return isnan(x->num) && x->_B_A; }
API bool fx_isStr(var x) { return isnan(x->num) && x->_B_S; }
API bool fx_isTrue(var x) { return isnan(x->num) && x->_B_T; }
API bool fx_isFalse(var x) { return isnan(x->num) && x->_B_F; }

API var fx_emptyObject();

// TODO: the func should take a KeyValue pair and not allocate it. Now
// you allocate using fx_newPair but it should be up to the caller whether
// a new allocation is needed or an existing KeyValue pair is to be added
// --- NO NO NO
API var fx_set(var objp, char* key, var value) {
    assert(objp);
    assert(fx_isObj(objp));

    fx__Object* objItem = fx_ptr(objp);
    if (!objItem) {
        objItem = fx_newObjectItem(fx_newPair(key, value), NULL);
        objp->_ptr = (u64)objItem;
        return objp;
    } else {
        do {
            if (!strcmp(objItem->kv->key, key)) {
                objItem->kv->value = value;
                return NULL;
            } else if (!objItem->next) {
                objItem->next = fx_newObjectItem(fx_newPair(key, value), NULL);
                var ret = fx_emptyObject();
                ret->_ptr = (u64)objItem->next;
                return ret;
            }
            objItem = objItem->next;
        } while (objItem);
    }
    return NULL;
}

API var fx_get(var objp, char* key) {
    // assert(objp);

    char* lkey = key;
    while (objp && fx_isObj(objp)) {
        // assert(fx_isObj(objp));
        fx__Object* objItem = fx_ptr(objp);
        objp = NULL;
        int keylen = 0;
        while (lkey[keylen] && lkey[keylen] != '.') keylen++;
        while (objItem) {
            // printf("%s %s %d\n", objItem->kv->key, lkey, keylen);
            if (!strncmp(objItem->kv->key, lkey, keylen)) {
                // printf("found %s\n", objItem->kv->key);
                objp = objItem->kv->value;
                break;
            }
            objItem = objItem->next;
        }
        lkey += keylen + 1;
        if (!*lkey) return objp;
    }
    return NULL;
}

API var fx_getAt(var arrp, int index) {
    assert(arrp);
    assert(fx_isArr(arrp));

    fx__Array* arrItem = fx_ptr(arrp);

    for (int i = 0; arrItem; i++) {
        if (i == index) return arrItem->value;
        arrItem = arrItem->next;
    }
    return NULL;
}
API var fx_emptyArray();
// returns an array reference to the last item, so that you can use it for
// repeated pushes. insert call should do this too
API var fx_push(var arrp, var value) {
    assert(arrp);
    assert(fx_isArr(arrp));

    fx__Array* arrItem = fx_ptr(arrp);
    if (!arrItem) {
        arrItem = fx_newArrayItem(value, NULL);
        arrp->_ptr = (u64)arrItem;
        return arrp;
    } else {
        do {
            if (!arrItem->next) {
                arrItem->next = fx_newArrayItem(value, NULL);
                var ret = fx_emptyArray();
                ret->_ptr = (u64)arrItem->next;
                return ret;
            }
            arrItem = arrItem->next;
        } while (arrItem);
    }
    return NULL;
}

// insert at position 0
API void fx_shift(var arrp, var value) {
    assert(arrp);
    assert(fx_isArr(arrp));

    fx__Array* arrItem = fx_ptr(arrp);
    // printf("arrItem: %p\n", arrItem);
    // if (!arrItem) {
    //     arrItem = fx_newArrayItem(value, NULL);
    //     arrp->_ptr = (u64)arrItem;
    // } else {
    // arrItem =
    arrp->_ptr = (u64)fx_newArrayItem(value, arrItem);
    // arrItem = fx_ptr(arrp);
    // printf("arrItemn: %p %p %p\n", arrItem, arrItem->value, arrItem->next);
    // arrItem->next = arrItem;
    // fx__Array* oldNext = arrItem->next;
    // if (!arrItem->next) {
    //     arrItem->next = break;
    // }
    // arrItem = arrItem->next;
    // } while (arrItem);
    // }
}

API var fx_stringRef(const char* str) {
    var ret = malloc(sizeof(fx__Value));
    *ret = (fx__Value) { .cstr = str };
    fx_setFlag(ret, _B_S);
    return ret;
}
API var fx_string(const char* str) { return fx_stringRef(strdup(str)); }
API var fx_number(double d) {
    var ret = malloc(sizeof(fx__Value));
    *ret = (fx__Value) { .num = d };
    return ret;
}
API var fx_true() {
    var ret = calloc(1, sizeof(fx__Value));
    fx_setFlag(ret, _B_T);
    return ret;
}
API var fx_false() {
    var ret = calloc(1, sizeof(fx__Value));
    fx_setFlag(ret, _B_F);
    return ret;
}
// when you have B_O set but null ptr, it means empty object, not null
API var fx_emptyObject() {
    var ret = calloc(1, sizeof(fx__Value));
    fx_setFlag(ret, _B_O);
    return ret;
}
// when you have B_A set but null ptr, it means empty array, not null
API var fx_emptyArray() {
    var ret = calloc(1, sizeof(fx__Value));
    fx_setFlag(ret, _B_A);
    return ret;
}

API var fx_arrayWith(var items[], int size) {
    var arr = fx_emptyArray();
    // careful with >= 0
    // printf("arr new: %p\n", fx_ptr(arr));

    for (int s = size - 1; s >= 0; --s) fx_shift(arr, items[s]);
    return arr;
}
#define fx_array(n, ...) fx_arrayWith((var[]) { __VA_ARGS__ }, n)

API var fx_null() { return NULL; }

API var fx_add(var a, var b) {
    assert(fx_isNum(a) && fx_isNum(b));
    return fx_number(a->num + b->num);
}

API var fx_sub(var a, var b) {
    assert(fx_isNum(a) && fx_isNum(b));
    return fx_number(a->num - b->num);
}

API var fx_mul(var a, var b) {
    assert(fx_isNum(a) && fx_isNum(b));
    return fx_number(a->num * b->num);
}

API var fx_div(var a, var b) {
    assert(fx_isNum(a) && fx_isNum(b));
    return fx_number(a->num / b->num);
}

API var fx_pow(var a, var b) {
    assert(fx_isNum(a) && fx_isNum(b));
    return fx_number(pow(a->num, b->num));
}

API var fx_exp(var a) {
    assert(fx_isNum(a));
    return fx_number(exp(a->num));
}

API var fx_log(var a) {
    assert(fx_isNum(a));
    return fx_number(log10(a->num));
}

API var fx_ln(var a) {
    assert(fx_isNum(a));
    return fx_number(log(a->num));
}

#define va_parg(x) va_arg(x, void*)

var fx_object(int size, ...) {
    va_list args;
    va_start(args, size);
    var obj = fx_emptyObject();
    var last = obj;
    while (size-- > 0) {
        // TODO: may need strdup
        char* key = va_parg(args);
        var val = va_parg(args);
        last = fx_set(last, key, val);
    }
    va_end(args);
    return obj;
}

int main() {
    // user facing API only exposes 1 type fx__Value (alias var)
    // and all user facing functions can only take and return DFValues
    // not internal-use types marked with __. Also internal use funcs marked
    // with __ cannot be exposed.
    // the fx__Value structure should not be exposed: user facing API deals only
    // with opaque type fx__Value* (aka var). That means everything goes on
    // heap.
    var v43 = fx_number(42.012);
    var v43s = fx_string("Brown fox");
    var vobj = fx_emptyObject();
    var varr = fx_emptyArray();

    // printf("%g\n", v43->num);
    fx_print(v43);
    // printf("%d\n", isnan(v43->num));

    // printf("%d\n", isnan(v43s->num));
    // printf("%s\n", v43s->cstr);
    fx_print(v43s);

    fx_set(vobj, "mykey", v43s);
    fx_set(vobj, "someval", v43);
    fx_print(vobj);
    fx_identify(vobj);

    fx_push(varr, v43s);
    fx_push(varr, v43);
    fx_print(varr);
    fx_identify(varr);

    var vc = fx_add(v43, v43);
    fx_print(vc);

    var vlkp = fx_get(vobj, "mykey");
    // printf("got: %p\n", vlkp);
    // printf("org: %p\n", v43s);
    // printf("%llu %llu\n", vlkp->bits, v43s->bits);
    fx_print(vlkp);
    fx_identify(vlkp);

    vlkp = fx_get(vobj, "sdhu");
    // printf("got: %p\n", vlkp);
    fx_print(vlkp);

    var vnewobj = fx_emptyObject();
    fx_set(vnewobj, "blake", vobj);
    fx_set(vnewobj, "uighur", varr);
    fx_set(vnewobj, "merut", v43s);
    fx_identify(vnewobj);
    fx_print(vnewobj);
    fx_printJSON(vnewobj);
    fx_printYAML(vnewobj);

    // array construction is simple
    var arr = //
        fx_array(4, //
            fx_number(36), //
            fx_number(45), //
            vobj, //
            fx_array(2, fx_number(65), //
                fx_number(76)));

    var obj = //
        fx_object(5, //
            "six", fx_number(6), //
            "five", fx_number(5), //
            "four", fx_object(1, "kuy", fx_number(4)), //
            "three", fx_number(3), //
            "two", fx_number(2));

    var o2 = fx_object(1, "obj", obj);
    fx_push(arr, fx_number(99));
    fx_printYAML(arr);
    fx_printYAML(o2);
    var vl = fx_get(o2, "obj.four");
    fx_print(vl);

    // have a root object that holds all vars.
    // fx_get should be able to handle keys like "mv.gh.ij" and "mv[7].ty"

    return 0;
}

// how about a dynamic lang that has the above variant like type,
// and is embeddable. Embedding is easy: you need a Context obj and there is
// a Context_load_from(char* sourceFile) or Context_load(char* code) func to
// load a source code and set it up (creates a DLL /.so and loads it)
// and a Context_eval(Context*, char* code) call.
// All calls return a fx__Value, all args are DFValues.
// Context c = Context_load_from("mycode.fx_")
// Context_eval("rfunc(%s, %d, %o)", "basic str", 32, o)
