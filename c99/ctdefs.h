#include "../engine/jet/base.h"
#include "ast.h"

#define MK1OBJ(T, block)                                                       \
    (T[1]) { block }
#define CT_MODULE(block) MK1OBJ(JetModule, block)
#define CT_FUNC(block) MK1OBJ(JetFunc, block)
#define CT_TYPE(block) MK1OBJ(JetType, block)
#define CT_TSPEC(block) MK1OBJ(JetTypeSpec, block)
#define CT_VAR(block) MK1OBJ(JetVar, block)
#define CT_EXPR(block) MK1OBJ(JetExpr, block)
// #define CT_LIST(n, ...) CT_LIST##n(__VA_ARGS__)
#define CT_LIST1(it) MK1OBJ(PtrList, .item = it)
// #define CT_LIST2(it, ...) MK1OBJ(PtrList, .item = it, .next = __VA_ARGS__)
// #define CT_LIST3(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST2(__VA_ARGS__))
// #define CT_LIST4(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST3(__VA_ARGS__))
// #define CT_LIST5(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST4(__VA_ARGS__))
// #define CT_LIST6(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST5(__VA_ARGS__))
// #define CT_LIST7(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST6(__VA_ARGS__))
// #define CT_LIST8(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST7(__VA_ARGS__))
// #define CT_LIST9(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST8(__VA_ARGS__))
// #define CT_LIST10(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST9(__VA_ARGS__))
// #define CT_LIST11(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST10(__VA_ARGS__))
// #define CT_LIST12(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST11(__VA_ARGS__))
// #define CT_LIST13(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST12(__VA_ARGS__))
// #define CT_LIST14(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST13(__VA_ARGS__))
// #define CT_LIST15(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST14(__VA_ARGS__))
// #define CT_LIST16(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST15(__VA_ARGS__))
// #define CT_LIST17(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST16(__VA_ARGS__))
// #define CT_LIST18(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST17(__VA_ARGS__))
// #define CT_LIST19(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST18(__VA_ARGS__))
// #define CT_LIST20(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST19(__VA_ARGS__))
// #define CT_LIST21(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST20(__VA_ARGS__))
// #define CT_LIST22(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST21(__VA_ARGS__))
// #define CT_LIST23(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST22(__VA_ARGS__))
// #define CT_LIST24(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST23(__VA_ARGS__))
// #define CT_LIST25(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST24(__VA_ARGS__))
// #define CT_LIST26(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST25(__VA_ARGS__))
// #define CT_LIST27(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST26(__VA_ARGS__))
// #define CT_LIST28(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST27(__VA_ARGS__))
// #define CT_LIST29(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST28(__VA_ARGS__))
// #define CT_LIST30(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST29(__VA_ARGS__))
// #define CT_LIST31(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST30(__VA_ARGS__))
// #define CT_LIST32(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST31(__VA_ARGS__))
// #define CT_LIST33(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST32(__VA_ARGS__))
// #define CT_LIST34(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST33(__VA_ARGS__))
// #define CT_LIST35(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST34(__VA_ARGS__))
// #define CT_LIST36(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST35(__VA_ARGS__))
// #define CT_LIST37(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST36(__VA_ARGS__))
// #define CT_LIST38(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST37(__VA_ARGS__))
// #define CT_LIST39(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST38(__VA_ARGS__))
// #define CT_LIST40(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST39(__VA_ARGS__))
// #define CT_LIST41(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST40(__VA_ARGS__))
// #define CT_LIST42(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST41(__VA_ARGS__))
// #define CT_LIST43(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST42(__VA_ARGS__))
// #define CT_LIST44(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST43(__VA_ARGS__))
// #define CT_LIST45(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST44(__VA_ARGS__))
// #define CT_LIST46(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST45(__VA_ARGS__))
// #define CT_LIST47(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST46(__VA_ARGS__))
// #define CT_LIST48(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST47(__VA_ARGS__))
// #define CT_LIST49(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST48(__VA_ARGS__))
// #define CT_LIST50(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST49(__VA_ARGS__))
// #define CT_LIST51(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST50(__VA_ARGS__))
// #define CT_LIST52(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST51(__VA_ARGS__))
// #define CT_LIST53(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST52(__VA_ARGS__))
// #define CT_LIST54(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST53(__VA_ARGS__))
// #define CT_LIST55(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST54(__VA_ARGS__))
// #define CT_LIST56(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST55(__VA_ARGS__))
// #define CT_LIST57(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST56(__VA_ARGS__))
// #define CT_LIST58(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST57(__VA_ARGS__))
// #define CT_LIST59(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST58(__VA_ARGS__))
// #define CT_LIST60(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST59(__VA_ARGS__))
// #define CT_LIST61(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST60(__VA_ARGS__))
// #define CT_LIST62(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST61(__VA_ARGS__))
// #define CT_LIST63(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST62(__VA_ARGS__))
// #define CT_LIST64(it, ...) \
//     MK1OBJ(PtrList, .item = it, .next = CT_LIST63(__VA_ARGS__))

#define CT_LIST2(a1, a2) CT_LIST1(a1)
#define CT_LIST3(a1, a2, a3) CT_LIST2(a1, a2)
#define CT_LIST4(a1, a2, a3, a4) CT_LIST3(a1, a2, a3)
#define CT_LIST5(a1, a2, a3, a4, a5) CT_LIST4(a1, a2, a3, a4)

JetVar* v = CT_VAR({ .name = "arg1" });

JetModule* base = //
    CT_MODULE(({
        .name = "base",
        .filename = "<builtin>", //
        .funcs = CT_LIST(3, //
            CT_FUNC({ .name = "print",
                .args = CT_LIST(1, CT_VAR({ .name = "arg1" })) }), //
            CT_FUNC({ .name = "describe" }), //
            CT_FUNC({ .name = "json" }) //
            ),
        .types = CT_LIST(1, //
            CT_TYPE(.name = "XML") //
            ) //
    }));
