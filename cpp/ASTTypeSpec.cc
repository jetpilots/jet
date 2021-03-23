#include "base.hh"

ASTTypeSpec* ASTTypeSpec::neew(TypeTypes tt, CollectionTypes ct) {
    ASTTypeSpec* ret = new ASTTypeSpec;
    ret->typeType = tt;
    ret->collectionType = ct;
    return ret;
}

const char* ASTTypeSpec::name() {
    switch (this->typeType) {
    case TYUnresolved:
        return this->name;
    case TYObject:
        return this->type->name;
    default:
        return TypeType::name(this->typeType);
    }
}

const char* ASTTypeSpec::cname() {
    switch (this->typeType) {
    case TYUnresolved:
        return this->name;
    case TYObject:
        return this->type->name;
    default:
        return TypeType::name(this->typeType);
    }
}

void ASTTypeSpec::emit(int level, bool isconst) {
    if (isconst) printf("const ");

    if (this->dims) {
        if (this->dims > 1)

            printf("SArray%dD(", this->dims);
        else
            printf("SArray(");
    }

    switch (this->typeType) {
    case TYObject:

        printf("%s", this->type->name);
        break;
    case TYUnresolved:
        unreachable(
            "unresolved: '%s' at %d:%d", this->name, this->line, this->col);
        printf("%s", *this->name ? this->name : "Error_Type");
        break;
    default:
        printf("%s", TypeType_name(this->typeType));
        break;
    }

    if (this->dims) printf("%s", ")");
}

void ASTTypeSpec::lint(int level) {
    switch (this->typeType) {
    case TYObject:
        printf("%s", this->type->name);
        break;
    case TYUnresolved:
        printf("%s", this->name);
        break;
    default:
        printf("%s", TypeType::name(this->typeType));
        break;
    }

    switch (this->collectionType) {
    case CTYDictS:
        printf("[DICTK]");
        break;
    case CTYArray:
        printf("[]");
        break;
    case CTYTensor:
        if (this->dims) {
            const char* dimsstr = ":,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:";

            printf("[%.*s]", 2 * this->dims - 1, dimsstr);
        } else {
            printf("[?]");
        }
        break;
    default:;
    }
}

void ASTTypeSpec::resolveTypeSpec(
    Parser* parser, ASTTypeSpec* typeSpec, ASTModule* mod) {

    if (this->typeType != TYUnresolved) return;
    if (!*this->name) return;

    TypeTypes tyty = TypeType_byName(this->name);
    if (tyty) {
        this->typeType = tyty;
    } else {
        ASTType* type = getType(mod, this->name);
        if (type) {
            this->typeType = TYObject;
            this->type = type;
            return;
        }
        Parser_errorUnrecognizedType(parser, typeSpec);
        return;
    }
    if (this->dims) { }
}
