#include <stdio.h>
#define monostatic static
#include "c99/main.c"

#define AEXPR(...)                                                         \
  &(Expr) { __VA_ARGS__ }
extern Func func_Number_tszf;
static Var v = { .name = "c",
    .spec = &(TypeSpec) { .typeType = 20,
        .collectionType = 0,
        .name = "",
        .dims = 0,
        .nullable = 0 },
    .init = &(Expr) {
        .kind = tkOpPlus,
        .line = 11,
        .col = 13,
        .left = AEXPR(.kind = tkOpUnaryMinus, .line = 11, .col = 9,
            .right
            = AEXPR(.kind = tkNumber, .line = 11, .col = 11, .string = "4", ),
            .prec = 55, .unary = 1, ),
        .right = &(Expr){.kind = tkFunctionCallResolved, .line = 11, .col = 15,
            .func = &func_Number_tszf,
            .left
            = &(Expr){.kind = tkNumber, .line = 11, .col = 20, .string = "34"},
            .prec = 60, },
        .prec = 47,
    } };
Func func_Number_tszf = { .name = "tszf",
  .selector = "Number_tszf",
  .args = &(PtrList) { .item = &(Var) { .name = "n",
                         .spec = &(TypeSpec) { .typeType = 20,
                           .collectionType = 0,
                           .name = "Number",
                           .dims = 0,
                           .nullable = 0 } } },
  .intrinsic = 0 };

// typedef struct Type {

//     int x;
//     const char* name;
//     struct Type* ty;
//     bool isEnum;
//
// }

// Type; //

// // static  Type ty4 = {
// .x = 3
// }

// ; //
// // static  Type x = {
// .x = 3, .ty = &ty4
// }

// ; //

// int msain() { }

static Type type_TypeInfo = { .name = "TypeInfo" };                       //
static Type type_VarInfo = { .name = "VarInfo" };                         //
static Type enum_TypeTypes = { .name = "TypeTypes", .isEnum = true };     //
static Type enum_NumSubTypes = { .name = "NumSubTypes", .isEnum = true }; //
static Type enum_StrSubTypes = { .name = "StrSubTypes", .isEnum = true }; //
static Type enum_StorageKinds
  = { .name = "StorageKinds", .isEnum = true }; //
static Func func_TypeInfo
  = { .name = "TypeInfo", .selector = "TypeInfo", .intrinsic = 0 }; //
static Func func_TypeInfo_json = { .name = "json",
  .selector = "TypeInfo_json",
  .args = &(PtrList) { .item = &(Var) { .name = "arg1",
                         .spec = &(TypeSpec) { .typeType = 3,
                           .collectionType = 0,
                           .type = &type_TypeInfo,
                           .dims = 0,
                           .nullable = 0 } } }, //
  .intrinsic = 1 };                             //
static Func func_TypeInfo_print = { .name = "print",
  .selector = "TypeInfo_print",
  .args = &(PtrList) { .item = &(Var) { .name = "arg1",
                         .spec = &(TypeSpec) { .typeType = 3,
                           .collectionType = 0,
                           .type = &type_TypeInfo,
                           .dims = 0,
                           .nullable = 0 } } }, //
  .intrinsic = 1 };                             //
static Func func_TypeInfo_describe = { .name = "describe",
  .selector = "TypeInfo_describe",
  .args = &(PtrList) { .item = &(Var) { .name = "arg1",
                         .spec = &(TypeSpec) { .typeType = 3,
                           .collectionType = 0,
                           .type = &type_TypeInfo,
                           .dims = 0,
                           .nullable = 0 } } }, //
  .intrinsic = 1 };                             //
static Func func_VarInfo
  = { .name = "VarInfo", .selector = "VarInfo", .intrinsic = 0 }; //
static Func func_VarInfo_json = { .name = "json",
  .selector = "VarInfo_json",
  .args = &(PtrList) { .item = &(Var) { .name = "arg1",
                         .spec = &(TypeSpec) { .typeType = 3,
                           .collectionType = 0,
                           .type = &type_VarInfo,
                           .dims = 0,
                           .nullable = 0 } } }, //
  .intrinsic = 1 };                             //
static Func func_VarInfo_print = { .name = "print",
  .selector = "VarInfo_print",
  .args = &(PtrList) { .item = &(Var) { .name = "arg1",
                         .spec = &(TypeSpec) { .typeType = 3,
                           .collectionType = 0,
                           .type = &type_VarInfo,
                           .dims = 0,
                           .nullable = 0 } } }, //
  .intrinsic = 1 };                             //
static Func func_VarInfo_describe = { .name = "describe",
  .selector = "VarInfo_describe",
  .args = &(PtrList) { .item = &(Var) { .name = "arg1",
                         .spec = &(TypeSpec) { .typeType = 3,
                           .collectionType = 0,
                           .type = &type_VarInfo,
                           .dims = 0,
                           .nullable = 0 } } }, //
  .intrinsic = 1 };                             //
static Func func_CString_VarInfo = { .name = "VarInfo",
  .selector = "CString_VarInfo",
  .args = &(PtrList) { .item = &(Var) { .name = "s",
                         .spec = &(TypeSpec) { .typeType = 9,
                           .collectionType = 0,
                           .name = "String",
                           .dims = 0,
                           .nullable = 0 } } }, //
  .intrinsic = 0 };                             //
static Func func_Number_TypeInfo = { .name = "TypeInfo",
  .selector = "Number_TypeInfo",
  .args = &(PtrList) { .item = &(Var) { .name = "v",
                         .spec = &(TypeSpec) { .typeType = 20,
                           .collectionType = 0,
                           .name = "Number",
                           .dims = 0,
                           .nullable = 0 } } }, //
  .intrinsic = 0 };                             //
static Func func_CString_TypeInfo = { .name = "TypeInfo",
  .selector = "CString_TypeInfo",
  .args = &(PtrList) { .item = &(Var) { .name = "s",
                         .spec = &(TypeSpec) { .typeType = 9,
                           .collectionType = 0,
                           .name = "String",
                           .dims = 0,
                           .nullable = 0 } } }, //
  .intrinsic = 0 };                             //
static Func func_Number_tsz_en_mks = { .name = "tsz",
  .selector = "Number_tsz_en_mks",
  .args = &(PtrList) { .item = &(Var) { .name = "s",
                         .spec = &(TypeSpec) { .typeType = 20,
                           .collectionType = 0,
                           .name = "Number",
                           .dims = 0,
                           .nullable = 0 } }, //
    .next = &(PtrList) { .item = &(Var) { .name = "en",
                           .spec = &(TypeSpec) { .typeType = 3,
                             .collectionType = 0,
                             .type = &enum_TypeTypes,
                             .dims = 0,
                             .nullable = 0 } }, //
      .next = &(PtrList) { .item = &(Var) { .name = "mks",
                             .spec = &(TypeSpec) { .typeType = 9,
                               .collectionType = 0,
                               .name = "String",
                               .dims = 0,
                               .nullable = 0 } } } } }, //
  .intrinsic = 0 };                                     //
static Func func_Number_tsz_en_mk = { .name = "tsz",
  .selector = "Number_tsz_en_mk",
  .args = &(PtrList) { .item = &(Var) { .name = "s",
                         .spec = &(TypeSpec) { .typeType = 20,
                           .collectionType = 0,
                           .name = "Number",
                           .dims = 0,
                           .nullable = 0 } }, //
    .next = &(PtrList) { .item = &(Var) { .name = "en",
                           .spec = &(TypeSpec) { .typeType = 3,
                             .collectionType = 0,
                             .type = &enum_TypeTypes,
                             .dims = 0,
                             .nullable = 0 } }, //
      .next = &(PtrList) { .item = &(Var) { .name = "mk",
                             .spec = &(TypeSpec) { .typeType = 20,
                               .collectionType = 0,
                               .name = "Number",
                               .dims = 0,
                               .nullable = 0 } } } } }, //
  .intrinsic = 0 };                                     //
static Func func_TypeTypes_tsza = { .name = "tsza",
  .selector = "TypeTypes_tsza",
  .args = &(PtrList) { .item = &(Var) { .name = "en",
                         .spec = &(TypeSpec) { .typeType = 3,
                           .collectionType = 0,
                           .type = &enum_TypeTypes,
                           .dims = 0,
                           .nullable = 0 } } }, //
  .intrinsic = 0 };                             //
static Func func_CString_json = { .name = "json",
  .selector = "CString_json",
  .args = &(PtrList) { .item = &(Var) { .name = "arg1",
                         .spec = &(TypeSpec) { .typeType = 9,
                           .collectionType = 0,
                           .name = "String",
                           .dims = 0,
                           .nullable = 0 } } }, //
  .intrinsic = 1 };                             //
static Func func_CString_print = { .name = "print",
  .selector = "CString_print",
  .args = &(PtrList) { .item = &(Var) { .name = "arg1",
                         .spec = &(TypeSpec) { .typeType = 9,
                           .collectionType = 0,
                           .name = "String",
                           .dims = 0,
                           .nullable = 0 } } }, //
  .intrinsic = 1 };                             //
static Func func_CString_describe = { .name = "describe",
  .selector = "CString_describe",
  .args = &(PtrList) { .item = &(Var) { .name = "arg1",
                         .spec = &(TypeSpec) { .typeType = 9,
                           .collectionType = 0,
                           .name = "String",
                           .dims = 0,
                           .nullable = 0 } } }, //
  .intrinsic = 1 };                             //
static Func func_Number_json = { .name = "json",
  .selector = "Number_json",
  .args = &(PtrList) { .item = &(Var) { .name = "arg1",
                         .spec = &(TypeSpec) { .typeType = 20,
                           .collectionType = 0,
                           .name = "Number",
                           .dims = 0,
                           .nullable = 0 } } }, //
  .intrinsic = 1 };                             //
static Func func_Number_print = { .name = "print",
  .selector = "Number_print",
  .args = &(PtrList) { .item = &(Var) { .name = "arg1",
                         .spec = &(TypeSpec) { .typeType = 20,
                           .collectionType = 0,
                           .name = "Number",
                           .dims = 0,
                           .nullable = 0 } } }, //
  .intrinsic = 1 };                             //
static Func func_Number_describe = { .name = "describe",
  .selector = "Number_describe",
  .args = &(PtrList) { .item = &(Var) { .name = "arg1",
                         .spec = &(TypeSpec) { .typeType = 20,
                           .collectionType = 0,
                           .name = "Number",
                           .dims = 0,
                           .nullable = 0 } } }, //
  .intrinsic = 1 };                             //
static Func func_Boolean_json = { .name = "json",
  .selector = "Boolean_json",
  .args = &(PtrList) { .item = &(Var) { .name = "arg1",
                         .spec = &(TypeSpec) { .typeType = 10,
                           .collectionType = 0,
                           .name = "Boolean",
                           .dims = 0,
                           .nullable = 0 } } }, //
  .intrinsic = 1 };                             //
static Func func_Boolean_print = { .name = "print",
  .selector = "Boolean_print",
  .args = &(PtrList) { .item = &(Var) { .name = "arg1",
                         .spec = &(TypeSpec) { .typeType = 10,
                           .collectionType = 0,
                           .name = "Boolean",
                           .dims = 0,
                           .nullable = 0 } } }, //
  .intrinsic = 1 };                             //
static Func func_Boolean_describe = { .name = "describe",
  .selector = "Boolean_describe",
  .args = &(PtrList) { .item = &(Var) { .name = "arg1",
                         .spec = &(TypeSpec) { .typeType = 10,
                           .collectionType = 0,
                           .name = "Boolean",
                           .dims = 0,
                           .nullable = 0 } } }, //
  .intrinsic = 1 };                             //