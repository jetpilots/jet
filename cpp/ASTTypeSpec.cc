class ASTTypeSpec {
    union {
        ASTType* type;
        char* name;
        Interval* intv;

        ASTUnits* units;
    };
    struct {

        uint16_t dims;

        CollectionTypes collectionType : 6;
        bool hasRange : 1, hasUnits : 1;
        TypeTypes typeType : 7;
        bool nullable : 1;
    };
    ASTLocation loc[0];
    uint32_t line : 24, col, : 8;

private:
    static ASTTypeSpec* ASTTypeSpec::new (TypeTypes tt, CollectionTypes ct) {
        ASTTypeSpec* ret = NEW(ASTTypeSpec);
        ret->typeType = tt;
        ret->collectionType = ct;
        return ret;
    }

    static const char* ASTTypeSpec::name(ASTTypeSpec* self) {
        switch (self->typeType) {
        case TYUnresolved:
            return self->name;
        case TYObject:
            return self->type->name;
        default:
            return TypeType::name(self->typeType);
        }
    }

    static const char* ASTTypeSpec::cname(ASTTypeSpec* self) {
        switch (self->typeType) {
        case TYUnresolved:
            return self->name;
        case TYObject:
            return self->type->name;
        default:
            return TypeType::name(self->typeType);
        }
    }

    void emit(int level, bool isconst) {
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

    void lint(int level) {
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

    void resolveTypeSpec(
        Parser* parser, ASTTypeSpec* typeSpec, ASTModule* mod) {

        if (this->typeType != TYUnresolved) return;
        if (!*this->name) return;

        TypeTypes tyty = TypeType_byName(this->name);
        if (tyty) {
            this->typeType = tyty;
        } else {
            ASTType* type = ASTModule_getType(mod, this->name);
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
}