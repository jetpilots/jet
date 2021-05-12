

static const Type type_Expr = {
    .name ="Expr",
    .body = &(PtrList) {
        .item = &(Var){
        .name = "meg",
        .spec = &(TypeSpec) {
.typeType = 25,
.name = "", }
, .init = &(Expr) { .kind = tkNumber,
.line = 22,
.col = 15,
.string = "33.2",
}},
        .next = &(PtrList) {
        .item = &(Var){
        .name = "bx",
        .spec = &(TypeSpec) {
.typeType = 15,
.name = "", }
, .init = &(Expr) { .kind = tkLT,
.line = 23,
.col = 16,
.left = &(Expr) { .kind = tkNumber,
.line = 23,
.col = 14,
.string = "4",
},
.right = &(Expr) { .kind = tkNumber,
.line = 23,
.col = 18,
.string = "5",
},
.prec = 41,
}},
        .next = &(PtrList) {
        .item = &(Var){
        .name = "f",
        .spec = &(TypeSpec) {
.typeType = 15,
.name = "", }
, .init = &(Expr) { .kind = tkLT,
.line = 26,
.col = 14,
.left = &(Expr) { .kind = tkNumber,
.line = 26,
.col = 13,
.string = "3",
},
.right = &(Expr) { .kind = tkNumber,
.line = 26,
.col = 15,
.string = "3",
},
.prec = 41,
}}}
}
    }
};
static const Type type_Another = {
    .name ="Another",
    .body = &(PtrList) {
        .item = &(Var){
        .name = "g",
        .spec = &(TypeSpec) {
.typeType = 25,
.name = "", }
, .init = &(Expr) { .kind = tkNumber,
.line = 39,
.col = 13,
.string = "12",
}},
        .next = &(PtrList) {
        .item = &(Var){
        .name = "exp",
        .spec = &(TypeSpec) {
.typeType = 4,
.type = &type_Expr, }
, .init = &(Expr) { .kind = tkFuncCallR,
.line = 40,
.col = 15,
.func = &func_Expr, 
.prec = 60,
}}}
    }
};
static const Type type_YetAnother = {
    .name ="YetAnother",
    .body = &(PtrList) {
        .item = &(Var){
        .name = "g",
        .spec = &(TypeSpec) {
.typeType = 4,
.type = &type_Another, }
, .init = &(Expr) { .kind = tkFuncCallR,
.line = 44,
.col = 13,
.func = &func_Another, 
.prec = 60,
}}    }
};
static const Type type_Other = {
    .name ="Other",
    .body = &(PtrList) {
        .item = &(Var){
        .name = "m",
        .spec = &(TypeSpec) {
.typeType = 25,
.name = "", }
, .init = &(Expr) { .kind = tkNumber,
.line = 51,
.col = 13,
.string = "43",
}},
        .next = &(PtrList) {
        .item = &(Var){
        .name = "we",
        .spec = &(TypeSpec) {
.typeType = 4,
.type = &type_Another, }
, .init = &(Expr) { .kind = tkFuncCallR,
.line = 52,
.col = 14,
.func = &func_Another, 
.prec = 60,
}}}
    }
};
static const Type type_Point = {
    .name ="Point",
    .body = &(PtrList) {
        .item = &(Var){
        .name = "x",
        .spec = &(TypeSpec) {
.typeType = 25,
.name = "", }
, .init = &(Expr) { .kind = tkFuncCallR,
.line = 58,
.col = 13,
.func = &func_Number_fxfunc, 
.left = &(Expr) { .kind = tkNumber,
.line = 58,
.col = 20,
.string = "3",
},
.prec = 60,
}},
        .next = &(PtrList) {
        .item = &(Var){
        .name = "y",
        .spec = &(TypeSpec) {
.typeType = 25,
.name = "", }
, .init = &(Expr) { .kind = tkNumber,
.line = 59,
.col = 13,
.string = "69.6723",
}},
        .next = &(PtrList) {
        .item = &(Var){
        .name = "z",
        .spec = &(TypeSpec) {
.typeType = 25,
.name = "", }
, .init = &(Expr) { .kind = tkPlus,
.line = 61,
.col = 15,
.left = &(Expr) { .kind = tkIdentR,
.line = 61,
.col = 13,
.var = &var_59_y, },
.right = &(Expr) { .kind = tkTimes,
.line = 61,
.col = 21,
.left = &(Expr) { .kind = tkNumber,
.line = 61,
.col = 17,
.string = "5.6",
},
.right = &(Expr) { .kind = tkIdentR,
.line = 61,
.col = 23,
.var = &var_58_x, },
.prec = 49,
},
.prec = 47,
}},
        .next = &(PtrList) {
        .item = &(Var){
        .name = "cstr",
        .spec = &(TypeSpec) {
.typeType = 14,
.name = "", }
, .init = &(Expr) { .kind = tkString,
.line = 63,
.col = 16,
.string = "xyz",
}}}
}
}
    }
};
static const Func func_Expr = { .name = "Expr", .selector = "Expr", .intrinsic = 0 };
static const Func func_Expr_json = { .name = "json", .selector = "Expr_json", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 4,
.type = &type_Expr, }
}}, .intrinsic = 1 };
static const Func func_Expr_print = { .name = "print", .selector = "Expr_print", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 4,
.type = &type_Expr, }
}}, .intrinsic = 1 };
static const Func func_Expr_describe = { .name = "describe", .selector = "Expr_describe", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 4,
.type = &type_Expr, }
}}, .intrinsic = 1 };
static const Func func_Another = { .name = "Another", .selector = "Another", .intrinsic = 0 };
static const Func func_Another_json = { .name = "json", .selector = "Another_json", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 4,
.type = &type_Another, }
}}, .intrinsic = 1 };
static const Func func_Another_print = { .name = "print", .selector = "Another_print", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 4,
.type = &type_Another, }
}}, .intrinsic = 1 };
static const Func func_Another_describe = { .name = "describe", .selector = "Another_describe", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 4,
.type = &type_Another, }
}}, .intrinsic = 1 };
static const Func func_YetAnother = { .name = "YetAnother", .selector = "YetAnother", .intrinsic = 0 };
static const Func func_YetAnother_json = { .name = "json", .selector = "YetAnother_json", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 4,
.type = &type_YetAnother, }
}}, .intrinsic = 1 };
static const Func func_YetAnother_print = { .name = "print", .selector = "YetAnother_print", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 4,
.type = &type_YetAnother, }
}}, .intrinsic = 1 };
static const Func func_YetAnother_describe = { .name = "describe", .selector = "YetAnother_describe", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 4,
.type = &type_YetAnother, }
}}, .intrinsic = 1 };
static const Func func_Other = { .name = "Other", .selector = "Other", .intrinsic = 0 };
static const Func func_Other_json = { .name = "json", .selector = "Other_json", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 4,
.type = &type_Other, }
}}, .intrinsic = 1 };
static const Func func_Other_print = { .name = "print", .selector = "Other_print", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 4,
.type = &type_Other, }
}}, .intrinsic = 1 };
static const Func func_Other_describe = { .name = "describe", .selector = "Other_describe", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 4,
.type = &type_Other, }
}}, .intrinsic = 1 };
static const Func func_Point = { .name = "Point", .selector = "Point", .intrinsic = 0 };
static const Func func_Point_json = { .name = "json", .selector = "Point_json", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 4,
.type = &type_Point, }
}}, .intrinsic = 1 };
static const Func func_Point_print = { .name = "print", .selector = "Point_print", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 4,
.type = &type_Point, }
}}, .intrinsic = 1 };
static const Func func_Point_describe = { .name = "describe", .selector = "Point_describe", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 4,
.type = &type_Point, }
}}, .intrinsic = 1 };
static const Func func_Number_fxfunc = { .name = "fxfunc", .selector = "Number_fxfunc", .args = &(PtrList) { .item = &(Var){
.name = "x",
.spec = &(TypeSpec) {
.typeType = 25,
.name = "Number", }
}}, .intrinsic = 0 };
static const Func func_Number_twoargs_y = { .name = "twoargs", .selector = "Number_twoargs_y", .args = &(PtrList) { .item = &(Var){
.name = "x",
.spec = &(TypeSpec) {
.typeType = 25,
.name = "Number", }
}, .next = &(PtrList) { .item = &(Var){
.name = "y",
.spec = &(TypeSpec) {
.typeType = 25,
.name = "Number", }
}} }, .intrinsic = 0 };
static const Func func_Number_Point = { .name = "Point", .selector = "Number_Point", .args = &(PtrList) { .item = &(Var){
.name = "x",
.spec = &(TypeSpec) {
.typeType = 25,
.name = "Number", }
}}, .intrinsic = 0 };
static const Func func_start = { .name = "start", .selector = "start", .intrinsic = 0 };
static const Func func_funky = { .name = "funky", .selector = "funky", .intrinsic = 0 };
static const Func func_joyce = { .name = "joyce", .selector = "joyce", .intrinsic = 0 };
static const Func func_String_json = { .name = "json", .selector = "String_json", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 14,
.name = "String", }
}}, .intrinsic = 1 };
static const Func func_String_print = { .name = "print", .selector = "String_print", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 14,
.name = "String", }
}}, .intrinsic = 1 };
static const Func func_String_describe = { .name = "describe", .selector = "String_describe", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 14,
.name = "String", }
}}, .intrinsic = 1 };
static const Func func_Number_json = { .name = "json", .selector = "Number_json", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 25,
.name = "Number", }
}}, .intrinsic = 1 };
static const Func func_Number_print = { .name = "print", .selector = "Number_print", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 25,
.name = "Number", }
}}, .intrinsic = 1 };
static const Func func_Number_describe = { .name = "describe", .selector = "Number_describe", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 25,
.name = "Number", }
}}, .intrinsic = 1 };
static const Func func_Boolean_json = { .name = "json", .selector = "Boolean_json", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 15,
.name = "Boolean", }
}}, .intrinsic = 1 };
static const Func func_Boolean_print = { .name = "print", .selector = "Boolean_print", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 15,
.name = "Boolean", }
}}, .intrinsic = 1 };
static const Func func_Boolean_describe = { .name = "describe", .selector = "Boolean_describe", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 15,
.name = "Boolean", }
}}, .intrinsic = 1 };
