static const Type type_Complex = {
    .name ="Complex",
    .body = &(PtrList) {
        .item = &(Var){
        .name = "re",
        .spec = &(TypeSpec) {
.typeType = 18,
.name = "", }
, .init = &(Expr) { .kind = tkNumber,
.line = 3,
.col = 14,
.string = "0",
}},
        .next = &(PtrList) {
        .item = &(Var){
        .name = "im",
        .spec = &(TypeSpec) {
.typeType = 18,
.name = "", }
, .init = &(Expr) { .kind = tkNumber,
.line = 4,
.col = 14,
.string = "0",
}}}
    }
};
static const Func func_Complex = { .name = "Complex", .selector = "Complex", .intrinsic = 0 };
static const Func func_Complex_json = { .name = "json", .selector = "Complex_json", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 3,
.type = &type_Complex, }
}}, .intrinsic = 1 };
static const Func func_Complex_print = { .name = "print", .selector = "Complex_print", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 3,
.type = &type_Complex, }
}}, .intrinsic = 1 };
static const Func func_Complex_describe = { .name = "describe", .selector = "Complex_describe", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 3,
.type = &type_Complex, }
}}, .intrinsic = 1 };
static const Func func_Number_Complex_im = { .name = "Complex", .selector = "Number_Complex_im", .args = &(PtrList) { .item = &(Var){
.name = "re",
.spec = &(TypeSpec) {
.typeType = 18,
.name = "Number", }
}, .next = &(PtrList) { .item = &(Var){
.name = "im",
.spec = &(TypeSpec) {
.typeType = 18,
.name = "Number", }
}} }, .intrinsic = 0 };
static const Func func_Number_Complex = { .name = "Complex", .selector = "Number_Complex", .args = &(PtrList) { .item = &(Var){
.name = "x",
.spec = &(TypeSpec) {
.typeType = 18,
.name = "Number", }
}}, .intrinsic = 0 };
static const Func func_Complex_add_b = { .name = "add", .selector = "Complex_add_b", .args = &(PtrList) { .item = &(Var){
.name = "a",
.spec = &(TypeSpec) {
.typeType = 3,
.type = &type_Complex, }
}, .next = &(PtrList) { .item = &(Var){
.name = "b",
.spec = &(TypeSpec) {
.typeType = 3,
.type = &type_Complex, }
}} }, .intrinsic = 0 };
static const Func func_Complex_sub_b = { .name = "sub", .selector = "Complex_sub_b", .args = &(PtrList) { .item = &(Var){
.name = "a",
.spec = &(TypeSpec) {
.typeType = 3,
.type = &type_Complex, }
}, .next = &(PtrList) { .item = &(Var){
.name = "b",
.spec = &(TypeSpec) {
.typeType = 3,
.type = &type_Complex, }
}} }, .intrinsic = 0 };
static const Func func_Complex_mul_b = { .name = "mul", .selector = "Complex_mul_b", .args = &(PtrList) { .item = &(Var){
.name = "a",
.spec = &(TypeSpec) {
.typeType = 3,
.type = &type_Complex, }
}, .next = &(PtrList) { .item = &(Var){
.name = "b",
.spec = &(TypeSpec) {
.typeType = 3,
.type = &type_Complex, }
}} }, .intrinsic = 0 };
static const Func func_Complex_div_b = { .name = "div", .selector = "Complex_div_b", .args = &(PtrList) { .item = &(Var){
.name = "a",
.spec = &(TypeSpec) {
.typeType = 3,
.type = &type_Complex, }
}, .next = &(PtrList) { .item = &(Var){
.name = "b",
.spec = &(TypeSpec) {
.typeType = 3,
.type = &type_Complex, }
}} }, .intrinsic = 0 };
static const Func func_Complex_recip = { .name = "recip", .selector = "Complex_recip", .args = &(PtrList) { .item = &(Var){
.name = "a",
.spec = &(TypeSpec) {
.typeType = 3,
.type = &type_Complex, }
}}, .intrinsic = 0 };
static const Func func_Complex_flip = { .name = "flip", .selector = "Complex_flip", .args = &(PtrList) { .item = &(Var){
.name = "a",
.spec = &(TypeSpec) {
.typeType = 3,
.type = &type_Complex, }
}}, .intrinsic = 0 };
static const Func func_Complex_addinv = { .name = "addinv", .selector = "Complex_addinv", .args = &(PtrList) { .item = &(Var){
.name = "a",
.spec = &(TypeSpec) {
.typeType = 3,
.type = &type_Complex, }
}}, .intrinsic = 0 };
static const Func func_Complex_conjugate = { .name = "conjugate", .selector = "Complex_conjugate", .args = &(PtrList) { .item = &(Var){
.name = "a",
.spec = &(TypeSpec) {
.typeType = 3,
.type = &type_Complex, }
}}, .intrinsic = 0 };
static const Func func_Complex_mulinv = { .name = "mulinv", .selector = "Complex_mulinv", .args = &(PtrList) { .item = &(Var){
.name = "a",
.spec = &(TypeSpec) {
.typeType = 3,
.type = &type_Complex, }
}}, .intrinsic = 0 };
static const Func func_String_json = { .name = "json", .selector = "String_json", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 7,
.name = "String", }
}}, .intrinsic = 1 };
static const Func func_String_print = { .name = "print", .selector = "String_print", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 7,
.name = "String", }
}}, .intrinsic = 1 };
static const Func func_String_describe = { .name = "describe", .selector = "String_describe", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 7,
.name = "String", }
}}, .intrinsic = 1 };
static const Func func_Number_json = { .name = "json", .selector = "Number_json", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 18,
.name = "Number", }
}}, .intrinsic = 1 };
static const Func func_Number_print = { .name = "print", .selector = "Number_print", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 18,
.name = "Number", }
}}, .intrinsic = 1 };
static const Func func_Number_describe = { .name = "describe", .selector = "Number_describe", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 18,
.name = "Number", }
}}, .intrinsic = 1 };
static const Func func_Boolean_json = { .name = "json", .selector = "Boolean_json", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 8,
.name = "Boolean", }
}}, .intrinsic = 1 };
static const Func func_Boolean_print = { .name = "print", .selector = "Boolean_print", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 8,
.name = "Boolean", }
}}, .intrinsic = 1 };
static const Func func_Boolean_describe = { .name = "describe", .selector = "Boolean_describe", .args = &(PtrList) { .item = &(Var){
.name = "arg1",
.spec = &(TypeSpec) {
.typeType = 8,
.name = "Boolean", }
}}, .intrinsic = 1 };
