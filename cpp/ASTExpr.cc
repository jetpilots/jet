#include "base.hh"

bool ASTExpr::isCtrlExpr() {
    return this->kind == tkKeyword_if || this->kind == tkKeyword_for
        || this->kind == tkKeyword_while || this->kind == tkKeyword_else;
}
bool ASTExpr::isSelfMutOp() {
    return this->kind == tkPlusEq || this->kind == tkMinusEq
        || this->kind == tkSlashEq || this->kind == tkTimesEq
        || this->kind == tkPowerEq || this->kind == tkOpModEq
        || this->kind == tkOpAssign;
}

bool ASTExpr::isArithOp() {
    return this->kind == tkPlusEq || this->kind == tkMinusEq
        || this->kind == tkSlashEq || this->kind == tkTimesEq
        || this->kind == tkPowerEq || this->kind == tkOpModEq
        || this->kind == tkPlus || this->kind == tkMinus
        || this->kind == tkSlash || this->kind == tkTimes
        || this->kind == tkPower || this->kind == tkOpMod;
}

bool ASTExpr::isLiteralExpr() { return false; }
bool ASTExpr::isComparatorExpr() { return false; }

bool ASTExpr::isCmpOp() {
    return this->kind == tkOpLE || this->kind == tkOpLT || this->kind == tkOpGT
        || this->kind == tkOpGE || this->kind == tkOpEQ || this->kind == tkOpNE;
}

bool ASTExpr::isBoolOp() {
    return this->kind == tkKeyword_and || this->kind == tkKeyword_or
        || this->kind == tkKeyword_not;
}

void ASTExpr::emit_tkSubscriptResolved(int level) {
    char* name = this->var->name;
    ASTExpr* index = this->left;
    assert(index);

    switch (index->kind) {
    case tkNumber:
        printf("Array_get_%s(%s, %s)", this->var->typeSpec->cname(), name,
            index->string);
        break;

    case tkString:
    case tkRawString:
        printf("Dict_get_CString_%s(%s, %s)", this->var->typeSpec->cname(),
            name, index->string);
        break;

    case tkOpComma:

        printf("Tensor%dD_get_%s(%s, {", this->var->typeSpec->dims,
            this->var->typeSpec->cname(), name);
        index.emit(0);
        printf("})");

        break;

    case tkOpColon:

        printf("Array_getSlice_%s(%s, ", this->var->typeSpec->name(), name);
        index.emit(0);
        printf(")");
        break;

    case tkOpEQ:
    case tkOpLE:
    case tkOpGE:
    case tkOpGT:
    case tkOpLT:
    case tkOpNE:
    case tkKeyword_and:
    case tkKeyword_or:
    case tkKeyword_not:

        printf("Array_copy_filter_%s(%s, ", this->var->typeSpec->name(), name);
        index.emit(0);
        printf(")");
        break;

    default:
        unreachable("bad kind: %s", TokenKind_str[this->kind]);
        break;
    }
}

void ASTExpr::emit_tkFunctionCallResolved(int level) {
    char* tmp = this->func->selector;

    ASTExpr* arg1 = this->left;
    const char* tmpc = "";
    if (arg1) {
        if (arg1->kind == tkOpComma) arg1 = arg1->left;
        tmpc = arg1->collectionType->CollectionType_nativeName();
    }
    printf("%s%s", tmpc, tmp);
    if (*tmp >= 'A' && *tmp <= 'Z' && !strchr(tmp, '_')) printf("_new_");
    printf("(");

    if (this->left) this->left.emit(0);

    if (!this->func->isDeclare) {
        printf("\n#ifdef DEBUG\n"
               "      %c \"./\" THISFILE \":%d:%d:\\e[0m ",
            this->left ? ',' : ' ', this->line, this->col);
        this->lint(0, false, true);
        printf("\"\n"
               "#endif\n        ");
    }
    printf(")");
}

char* ASTExpr::strchrnul(char* str, char ch) {
    while (*str && *str != ch) str++;
    return str;
}
void ASTExpr::lineupmultilinestring(int indent) {
    return;
    char* pos = this->string;
    while (*(pos = strpbrk(pos, "\n"))) {
        int del = strspn(pos, " ") - indent;
        if (del <= 0) continue;
    }
}

void ASTExpr::printmultilstr(char* pos) {
    do {
        int p = strcspn(pos, "\n");
        printf("\"%.*s", p, pos);
        pos += p + 1;
        printf(*pos ? "\\n\"" : "\"");
    } while (*pos);
}

void ASTExpr::emit_tkString(int level) {
    this->lineupmultilinestring(level + STEP);
    if (!this->vars) {
        printmultilstr(this->string + 1);
    } else {
        char* pos = this->string;

        char* last = pos;
        ASTVar* v;
        PtrList* p = this->vars;
        ASTExpr* e = p->item;
        printf("strinterp_h(64, ");
        while (*pos) {
            while (*pos && *pos != '$') pos++;
            *pos++ = 0;

            printf("%s", last);

            pos[-1] = '$';
            last = pos;
            while (*pos && isalnum(*pos) || *pos == '.') pos++;

            if (e) {
                while (e->kind == tkPeriod) e = e->right;
                assert(e->kind == tkIdentifierResolved);
                assert(e->var);
                printf("%s", e->var->typeSpec->typeType.format(false));
                last = pos;
                e = ((p = p->next)) ? p->item : NULL;
            }
        }
        printf("\"");
        for (auto& e : this->vars) {
            printf(", ");
            e.emit(0);
        }
        printf(")");
    }
}

void ASTExpr::emit_tkNumber(int level) {
    size_t ls = this->string->CString_length();
    if (this->string[ls - 1] == 'i') {
        printf("_Complex_I*");
        this->string[ls - 1] = 0;
    }
    printf("%s", this->string);
}

void ASTExpr::emit_tkCheck(int level) {

    ASTExpr* checkExpr = this->right;
    ASTExpr* lhsExpr = checkExpr->left;
    ASTExpr* rhsExpr = checkExpr->right;
    printf("{\n");
    if (!checkExpr->unary) {
        printf("%.*s%s _lhs = ", level, spaces, lhsExpr->typeName());
        lhsExpr.emit(0);
        printf(";\n");
    }
    printf("%.*s%s _rhs = ", level, spaces, rhsExpr->typeName());
    rhsExpr.emit(0);
    printf(";\n");
    printf("%.*sif (!(", level, spaces);

    checkExpr.emit(0);

    printf(")) {\n");
    printf("%.*sprintf(\"\\n\\n\e[31mruntime error:\e[0m check "
           "failed at \e[36m./%%s:%d:%d:\e[0m\\n    %%s\\n\\n\",\n     "
           "       "
           "   THISFILE, \"",
        level + STEP, spaces, this->line, this->col + 6);
    checkExpr.lint(0, true, true);
    printf("\");\n");
    printf("#ifdef DEBUG\n%.*sCHECK_HELP_OPEN;\n", level + STEP, spaces);

    checkExpr.genPrintVars(level + STEP);

    if (!checkExpr->unary) {

        if (lhsExpr->collectionType == CTYNone && lhsExpr->kind != tkString
            && lhsExpr->kind != tkNumber && lhsExpr->kind != tkRawString
            && lhsExpr->kind != tkOpLE && lhsExpr->kind != tkOpLT) {

            if (lhsExpr->kind != tkIdentifierResolved
                || !lhsExpr->var->visited) {
                printf("%.*s%s", level + STEP, spaces, "printf(\"    %s = ");
                printf("%s", lhsExpr->typeType.format(true));
                printf("%s", "\\n\", \"");
                lhsExpr.lint(0, true, true);
                printf("%s", "\", _lhs);\n");
            }
        }
    }
    if (rhsExpr->collectionType == CTYNone && rhsExpr->kind != tkString
        && rhsExpr->kind != tkNumber && rhsExpr->kind != tkRawString) {
        if (rhsExpr->kind != tkIdentifierResolved || !rhsExpr->var->visited) {
            printf("%.*s%s", level + STEP, spaces, "printf(\"    %s = ");
            printf("%s", rhsExpr->typeType.format(true));
            printf("%s", "\\n\", \"");
            rhsExpr->lint(0, true, true);
            printf("%s", "\", _rhs);\n");
        }
    }

    checkExpr->unmarkVisited();

    printf("%.*sCHECK_HELP_CLOSE;\n", level + STEP, spaces);
    printf("#else\n%.*sCHECK_HELP_DISABLED;\n", level + STEP, spaces);
    printf("#endif\n%.*s}\n%.*s}", level, spaces, level, spaces);
}

void ASTExpr::emit(int level) {

    printf("%.*s", level, spaces);
    switch (this->kind) {
    case tkNumber:
        this->emit_tkNumber(level);
        break;

    case tkKeyword_no:
        printf("no");
        break;
    case tkKeyword_yes:
        printf("yes");
        break;
    case tkKeyword_nil:
        printf("nil");
        break;

    case tkMultiDotNumber:
    case tkIdentifier:
        printf("%s", this->string);
        break;

    case tkString:
        this->emit_tkString(level);

        break;

    case tkIdentifierResolved:
        printf("%s", this->var->name);
        break;

    case tkRawString:
        printf("\"%s\"", this->string + 1);
        break;

    case tkRegexp:
        printf("%s", this->string + 1);
        break;

    case tkLineComment:
        printf("~%s", this->string);
        break;

    case tkFunctionCall:
        unreachable("unresolved call to '%s'\n", this->string);
        break;

    case tkFunctionCallResolved:
        this->emit_tkFunctionCallResolved(level);
        break;

    case tkSubscript:
        unreachable("unresolved subscript on '%s'\n", this->string);
        break;

    case tkSubscriptResolved:
        this->emit_tkSubscriptResolved(level);
        break;

    case tkOpAssign:
    case tkPlusEq:
    case tkMinusEq:
    case tkTimesEq:
    case tkSlashEq:
    case tkPowerEq:
    case tkOpModEq:

        switch (this->left->kind) {
        case tkSubscriptResolved:
            switch (this->left->left->kind) {
            case tkNumber:
            case tkString:
            case tkRawString:

                printf("%s_set(%s, %s,%s, ", this->left->typeName(),
                    this->left->var->name, this->left->left->string,
                    this->kind.repr(true));
                this->right.emit(0);
                printf(")");
                break;

            case tkOpColon:
                printf("%s_setSlice(%s, ", this->left->typeName(),
                    this->left->var->name);
                this->left->left.emit(0);
                printf(",%s, ", this->kind.repr(true));
                this->right.emit(0);
                printf(")");
                break;

            case tkOpEQ:
            case tkOpGE:
            case tkOpNE:
            case tkOpGT:
            case tkOpLE:
            case tkOpLT:
            case tkKeyword_and:
            case tkKeyword_or:
            case tkKeyword_not:
                printf("%s_setFiltered(%s, ", this->left->typeName(),
                    this->left->var->name);
                this->left->left.emit(0);
                printf(",%s, ", this->kind.repr(true));
                this->right.emit(0);
                printf(")");
                break;

            case tkOpComma:

                break;
            case tkIdentifierResolved:

                break;
            case tkSubscriptResolved:

                break;
            case tkFunctionCallResolved:

                break;
            default:
                unreachable("%s\n", TokenKind_str[this->left->kind]);
                assert(0);
            }
            break;
        case tkIdentifierResolved:
        case tkPeriod:
            this->left->emit(0);
            printf("%s", this->kind.repr(true));
            this->right->emit(0);
            break;
        case tkIdentifier:
            unreachable("unresolved var %s", this->left->string);
            break;
        case tkArgumentLabel:

            this->right->emit(0);

            break;
        case tkString:
            break;
        default:

            unreachable(
                "found token kind %s\n", TokenKind_str[this->left->kind]);
        }

        break;

    case tkArrayOpen:

        if (!this->right) {
            printf("Array_init(%s)()", "double");
        } else {
            printf("Array_make(((%s[]) {", "double");

            this->right.emit(0);
            printf("})");
            printf(", %d)", this->right->countCommaList());
        }
        break;

    case tkBraceOpen: {
        const char* Ktype = "CString";
        const char* Vtype = "Real64";
        if (!this->right)
            printf("Dict_init(%s,%s)()", Ktype, Vtype);
        else {
            printf("Dict_make(%s,%s)(%d, (%s[]){", Ktype, Vtype,
                this->right->countCommaList(), Ktype);

            ASTExpr* p = this->right;
            while (p && p->kind == tkOpComma) {
                p->left->left.emit(0);
                printf(", ");
                p = p->right;
            };
            p->left.emit(0);
            printf("}, (%s[]){", Vtype);
            p = this->right;
            while (p && p->kind == tkOpComma) {
                p->left->right.emit(0);
                printf(", ");
                p = p->right;
            };
            p->right.emit(0);
            printf("})");
        }
    } break;

    case tkOpColon:

        printf(
            "%s(", this->left->kind != tkOpColon ? "range_to" : "range_to_by");
        if (this->left->kind == tkOpColon) {
            this->left->kind = tkOpComma;
            this->left->emit(0);
            this->left->kind = tkOpColon;
        } else
            this->left->emit(0);
        printf(", ");
        this->right->emit(0);
        printf(")");
        break;

    case tkVarAssign:

        if (this->var->init != NULL && this->var->used) {
            printf("%s = ", this->var->name);
            this->var->init->emit(0);
        } else {
            printf("", this->var->name, this->var->used ? "null" : "unused",
                this->line);
        }
        break;

    case tkKeyword_else:
        puts("else {");
        if (this->body) this->body.emit(level + STEP);
        printf("%.*s}", level, spaces);
        break;

    case tkKeyword_elif:
        puts("else if (");
        this->left.emit(0);
        puts(") {");
        if (this->body) this->body.emit(level + STEP);
        printf("%.*s}", level, spaces);
        break;

    case tkKeyword_match: {
        printf("{%s __match_cond = ", this->left->typeName());
        this->left.emit(0);
        if (this->left->typeType > TYInt8
            || (this->left->typeType == TYObject
                && this->left->getObjectType()->isEnum))
            puts("; switch (__match_cond) {");
        else
            puts("; { if (0) {}");

        if (this->body) this->body.emit(level);
        printf("%.*s}}", level, spaces);
        break;
    }

        /*
        This is how you walk a ASTExpr that is a tkOpComma (left to right):
            process(cond->left);
            while (cond->right->kind == tkOpComma)
                cond = cond->right, process(cond->left);
            process(cond->right);
        */

    case tkKeyword_case: {
        ASTExpr* cond = this->left;
        if (cond->kind == tkOpComma) {
            if (cond->typeType > TYInt8
                || (cond->typeType == TYObject && cond->getEnumType())) {
                printf("case ");
                cond->left.emit(0);
                printf(": ");
                while (cond->right->kind == tkOpComma) {
                    cond = cond->right;
                    printf("case ");
                    cond->left.emit(0);
                    printf(": ");
                }
                printf("case ");
                cond->right.emit(0);
                puts(": {");
            } else if (cond->typeType == TYString) {
                printf("else if (!strcmp(__match_cond, ");
                cond->left.emit(0);
                printf(")");
                while (cond->right->kind == tkOpComma) {
                    cond = cond->right;
                    printf(" || !strcmp(__match_cond, ");
                    cond->left.emit(0);
                    printf(")");
                }
                printf(" || !strcmp(__match_cond, ");
                cond->right.emit(0);
                puts(")) do {");
            } else {
                printf("else if (__match_cond == ");
                cond->left.emit(0);
                while (cond->right->kind == tkOpComma) {
                    cond = cond->right;
                    printf(" || __match_cond == (");
                    cond->left.emit(0);
                }
                cond->right.emit(0);
                puts(")) do {");
            };

        } else {
            if (cond->typeType > TYInt8
                || (cond->typeType == TYObject && cond->getEnumType())) {
                printf("case ");
                cond.emit(0);
                puts(": {");
            } else if (cond->typeType == TYString) {
                printf("else if (!strcmp(__match_cond, ");
                cond.emit(0);
                puts(")) do {");
            } else {
                printf("else if (__match_cond == (");
                cond.emit(0);
                puts(")) do {");
            };
        };
        if (this->body) this->body.emit(level);
        printf("%.*s}", level, spaces);
        if (cond->typeType > TYInt8
            || (cond->typeType == TYObject && cond->getEnumType()))
            printf(" break");
        else
            printf(" while(0)");
        break;
    }
    case tkKeyword_for:
    case tkKeyword_if:

    case tkKeyword_while:
        if (this->kind == tkKeyword_for)
            printf("FOR(");
        else
            printf("%s (", this->kind.repr(true));
        if (this->kind == tkKeyword_for) this->left->kind = tkOpComma;
        if (this->left) this->left.emit(0);
        if (this->kind == tkKeyword_for) this->left->kind = tkOpAssign;
        puts(") {");
        if (this->body) this->body.emit(level + STEP);
        printf("%.*s}", level, spaces);
        break;

    case tkPower:
        printf("pow(");
        this->left.emit(0);
        printf(",");
        this->right.emit(0);
        printf(")");
        break;

    case tkKeyword_return:
        printf("{_err_ = NULL; STACKDEPTH_DOWN; return ");
        if (this->right) this->right.emit(0);
        printf(";}\n");
        break;

    case tkKeyword_check:
        this->emit_tkCheck(0);
        break;

    case tkPeriod:
        this->left.emit(0);
        if (this->left->typeType == TYObject
            && this->left->getObjectType()->isEnum)
            printf("_");
        else
            printf("->");

        this->right.emit(0);
        break;

    case tkOpEQ:
    case tkOpNE:
    case tkOpGE:
    case tkOpLE:
    case tkOpGT:
    case tkOpLT:
        if ((this->kind == tkOpLE || this->kind == tkOpLT)
            && (this->left->kind == tkOpLE | this->left->kind == tkOpLT)) {
            printf("%s_cmp3way_%s_%s(", this->left->right->typeName(),
                this->kind.ascrepr(false), this->left->kind.ascrepr(false));
            this->left->left.emit(0);
            printf(", ");
            this->left->right.emit(0);
            printf(", ");
            this->right.emit(0);
            printf(")");
            break;
        } else if (this->right->typeType == TYString) {
            printf("CString_cmp(%s, ", tksrepr[this->kind]);
            this->left.emit(0);
            printf(", ");
            this->right.emit(0);
            printf(")");
            break;
        }
        fallthrough default : if (!this->prec) break;

        bool leftBr
            = this->left && this->left->prec && this->left->prec < this->prec;
        bool rightBr = this->right && this->right->prec
            && this->right->kind != tkKeyword_return
            && this->right->prec < this->prec;

        char lpo = '(';
        char lpc = ')';
        if (leftBr) putc(lpo, stdout);
        if (this->left) this->left.emit(0);
        if (leftBr) putc(lpc, stdout);

        if (this->kind == tkArrayOpen)
            putc('{', stdout);
        else
            printf("%s", this->kind.repr(true));

        char rpo = '(';
        char rpc = ')';
        if (rightBr) putc(rpo, stdout);
        if (this->right) this->right.emit(0);
        if (rightBr) putc(rpc, stdout);

        if (this->kind == tkArrayOpen) putc('}', stdout);
    }
}

void ASTExpr::emit(int level);

bool ASTExpr::mustPromote(const char* name) {

    if (!strcmp(name, "Array_any_filter")) return true;
    if (!strcmp(name, "Array_all_filter")) return true;
    if (!strcmp(name, "Array_count_filter")) return true;
    if (!strcmp(name, "Array_write_filter")) return true;
    if (!strcmp(name, "Strs_print_filter")) return true;
    return false;
}

void ASTExpr::unmarkVisited() {
    switch (this->kind) {
    case tkIdentifierResolved:
    case tkVarAssign:
        this->var->visited = false;
        break;
    case tkFunctionCallResolved:
    case tkFunctionCall:
    case tkSubscriptResolved:
    case tkSubscript:
    case tkKeyword_if:
    case tkKeyword_for:
    case tkKeyword_else:
    case tkKeyword_while:
        this->left->unmarkVisited();
        break;
    default:
        if (this->prec) {
            if (!this->unary) this->left->unmarkVisited();
            this->right->unmarkVisited();
        }
    }
}

void ASTExpr::genPrintVars(int level) {
    assert(this);

    switch (this->kind) {
    case tkIdentifierResolved:
    case tkVarAssign:
        if (this->var->visited) break;
        printf("%.*sprintf(\"    %s = %s\\n\", %s);\n", level, spaces,
            this->var->name, this->typeType.format(true), this->var->name);
        this->var->visited = true;
        break;

    case tkPeriod:

        break;

    case tkFunctionCallResolved:
    case tkFunctionCall:
    case tkSubscriptResolved:
    case tkSubscript:
    case tkKeyword_if:
    case tkKeyword_else:
    case tkKeyword_for:
    case tkKeyword_while:
        this->left.genPrintVars(level);
        break;

    default:
        if (this->prec) {
            if (!this->unary) this->left.genPrintVars(level);
            this->right.genPrintVars(level);
        }
    }
}

ASTExpr* ASTExpr::findPromotionCandidate() {
    assert(this);
    ASTExpr* ret;

    switch (this->kind) {
    case tkFunctionCallResolved:

        if (this->left && (ret = this->left->findPromotionCandidate()))
            return ret;
        else if (mustPromote(this->func->selector))
            return this;
        break;

    case tkSubscriptResolved:

        return this->left->findPromotionCandidate();

    case tkSubscript:
        return this->left->findPromotionCandidate();

    case tkKeyword_if:
    case tkKeyword_for:
    case tkKeyword_else:
    case tkKeyword_elif:
    case tkKeyword_while:
        if (this->left) return this->left->findPromotionCandidate();

    case tkVarAssign:
        if ((ret = this->var->init->findPromotionCandidate())) return ret;
        break;

    case tkFunctionCall:

        unreachable("unresolved call %s\n", this->string);
        if ((ret = this->left->findPromotionCandidate())) return ret;
        break;

    default:
        if (this->prec) {
            if (this->right && (ret = this->right->findPromotionCandidate()))
                return ret;
            if (!this->unary)
                if ((ret = this->left->findPromotionCandidate())) return ret;
        }
    }
    return NULL;
}

char* ASTExpr::newTmpVarName(int num, char c) {
    char buf[8];
    int l = snprintf(buf, 8, "_%c%d", c, num);
    return pstrndup(buf, l);
}

void ASTExpr::lint(int level, bool spacing, bool escapeStrings) {

    printf("%.*s", level, spaces);

    switch (this->kind) {
    case tkNumber:
    case tkMultiDotNumber:
        printf("%s", this->string);
        break;
    case tkRawString:
        printf("'%s'", this->string + 1);
        break;
    case tkRegexp:
        printf("`%s`", this->string + 1);
        break;

    case tkIdentifier:
    case tkArgumentLabel:
    case tkIdentifierResolved: {
        char* tmp = (this->kind != tkIdentifierResolved) ? this->string
                                                         : this->var->name;
        printf("%s", tmp);
    } break;

    case tkString:
        printf(escapeStrings ? "\\%s\\\"" : "%s\"", this->string);
        break;
    case tkKeyword_no:
        printf("no");
        break;
    case tkKeyword_yes:
        printf("yes");
        break;
    case tkKeyword_nil:
        printf("nil");
        break;

    case tkLineComment:
        printf("%s%s", tkLineComment.repr(*this->string != ' '), this->string);
        break;

    case tkFunctionCall:
    case tkFunctionCallResolved: {
        char* tmp = (this->kind == tkFunctionCallResolved) ? this->func->name
                                                           : this->string;
        printf("%s(", tmp);
        if (this->left) this->left.lint(0, false, escapeStrings);
        printf(")");
    } break;

    case tkSubscript:
    case tkSubscriptResolved: {
        char* tmp = (this->kind == tkSubscriptResolved) ? this->var->name
                                                        : this->string;
        printf("%s[", tmp);
        if (this->left) this->left.lint(0, false, escapeStrings);
        printf("]");
    } break;

    case tkObjectInit:
    case tkObjectInitResolved:
        break;

    case tkPeriod:
        if (this->left && this->left->typeType == TYObject
            && !this->left->var->typeSpec->type->isEnum)
            this->left.lint(0, spacing, escapeStrings);
        printf(".");
        this->right.lint(0, spacing, escapeStrings);
        break;

    case tkVarAssign:

        assert(this->var != NULL);
        this->var.lint(0);
        break;

    case tkArrayOpen:
    case tkBraceOpen:
        printf("%s", tkrepr[this->kind]);
        if (this->right)
            this->right.lint(level, this->kind != tkArrayOpen, escapeStrings);
        printf("%s", tkrepr[TokenKind::reverseBracket(this->kind)]);
        break;

    default:
        if (!this->prec) break;

        bool leftBr
            = this->left && this->left->prec && this->left->prec < this->prec;
        bool rightBr = this->right && this->right->prec
            && this->right->kind != tkKeyword_return
            && this->right->prec < this->prec;

        if (this->kind == tkOpColon) {

            if (this->left) switch (this->left->kind) {
                case tkNumber:
                case tkIdentifier:
                case tkString:
                case tkOpColon:
                case tkMultiDotNumber:
                case tkUnaryMinus:
                    break;
                default:
                    leftBr = true;
                }
            if (this->right) switch (this->right->kind) {
                case tkNumber:
                case tkIdentifier:
                case tkString:
                case tkOpColon:
                case tkMultiDotNumber:
                case tkUnaryMinus:
                    break;
                default:
                    rightBr = true;
                }
        }

        if (this->kind == tkPower && !spacing) putc('(', stdout);

        char lpo = leftBr && this->left->kind == tkOpColon ? '[' : '(';
        char lpc = leftBr && this->left->kind == tkOpColon ? ']' : ')';
        if (leftBr) putc(lpo, stdout);
        if (this->left)
            this->left.lint(0, spacing && !leftBr && this->kind != tkOpColon,
                escapeStrings);
        if (leftBr) putc(lpc, stdout);

        printf("%s", this->kind.repr(spacing));

        char rpo = rightBr && this->right->kind == tkOpColon ? '[' : '(';
        char rpc = rightBr && this->right->kind == tkOpColon ? ']' : ')';
        if (rightBr) putc(rpo, stdout);
        if (this->right)
            this->right.lint(0, spacing && !rightBr && this->kind != tkOpColon,
                escapeStrings);
        if (rightBr) putc(rpc, stdout);

        if (this->kind == tkPower && !spacing) putc(')', stdout);
    }
}

static ASTExpr* ASTExpr::fromToken(const Token* this) {
    ASTExpr* ret = new ASTExpr;
    ret->kind = this->kind;
    ret->line = this->line;
    ret->col = this->col;

    ret->prec = ret->kind->TokenKind::getPrecedence();
    if (ret->prec) {
        ret->rassoc = ret->kind->TokenKind::isRightAssociative();
        ret->unary = ret->kind->TokenKind::isUnary();
    }

    exprsAllocHistogram[ret->kind]++;

    switch (ret->kind) {
    case tkKeyword_cheater:
    case tkKeyword_for:
    case tkKeyword_while:
    case tkKeyword_if:
    case tkKeyword_end:
    case tkKeyword_enum:
    case tkKeyword_match:
    case tkKeyword_case:
    case tkKeyword_function:
    case tkKeyword_declare:
    case tkKeyword_test:
    case tkKeyword_check:
    case tkKeyword_not:
    case tkKeyword_notin:
    case tkKeyword_and:
    case tkKeyword_yes:
    case tkKeyword_no:
    case tkKeyword_nil:
    case tkKeyword_or:
    case tkKeyword_in:
    case tkKeyword_do:
    case tkKeyword_then:
    case tkKeyword_as:
    case tkKeyword_else:
    case tkKeyword_elif:
    case tkKeyword_type:
    case tkKeyword_return:
    case tkKeyword_result:
    case tkKeyword_extends:
    case tkKeyword_var:
    case tkKeyword_let:
    case tkKeyword_import:
    case tkIdentifier:
    case tkArgumentLabel:
    case tkFunctionCall:
    case tkSubscript:
    case tkObjectInit:
    case tkNumber:
    case tkString:
    case tkRawString:
    case tkRegexp:
    case tkMultiDotNumber:
    case tkLineComment:
        ret->string = this->pos;
        break;
    default:;
    }

    if (ret->kind == tkLineComment) ret->string++;

    if (ret->kind == tkNumber) {
        ret->string->CString_tr_ip('d', 'e', this->matchlen);
        ret->string->CString_tr_ip('D', 'e', this->matchlen);
        ret->string->CString_tr_ip('E', 'e', this->matchlen);
    }
    return ret;
}

static bool ASTExpr::throws() {

    if (!this) return false;
    switch (this->kind) {
    case tkNumber:
    case tkMultiDotNumber:
    case tkRawString:
    case tkRegexp:
    case tkIdentifier:
    case tkIdentifierResolved:
    case tkString:
    case tkLineComment:
        return false;
    case tkFunctionCall:
    case tkFunctionCallResolved:
        return true;

    case tkSubscript:
    case tkSubscriptResolved:
        return this->left->throws();
    case tkVarAssign:
        return this->var->used && this->var->init->throws();
    case tkKeyword_for:
    case tkKeyword_if:
    case tkKeyword_while:
        return false;
    default:
        if (!this->prec) return false;
        return this->left->throws() || this->right->throws();
    }
}
static ASTTypeSpec* ASTExpr::getObjectTypeSpec() {
    if (!this) return NULL;

    switch (this->kind) {
    case tkFunctionCallResolved:

        return this->func->returnSpec;
    case tkIdentifierResolved:
    case tkSubscriptResolved:

        return this->var->typeSpec;

    case tkPeriod:
    case tkOpComma:

        return this->right->getObjectTypeSpec();
    default:
        break;
    }
    return NULL;
}

static ASTType* ASTExpr::getEnumType() {
    ASTType* ret = NULL;
    if (!this || this->typeType != TYObject) return ret;

    switch (this->kind) {
    case tkFunctionCallResolved:
        ret = this->func->returnSpec->type;
        break;
    case tkIdentifierResolved:
    case tkSubscriptResolved:
        ret = this->var->typeSpec->type;
        break;

    case tkPeriod:
    case tkOpComma:
        ret = this->left->getEnumType();
    default:
        break;
    }
    if (ret && !ret->isEnum) ret = NULL;
    return ret;
}

static ASTType* ASTExpr::getObjectType() {
    if (!this || this->typeType != TYObject) return NULL;

    switch (this->kind) {
    case tkFunctionCallResolved:
        return this->func->returnSpec->type;
    case tkIdentifierResolved:
    case tkSubscriptResolved:
        return this->var->typeSpec->type;

    case tkPeriod:
    case tkOpComma:
        return this->right->getObjectType();
    default:
        break;
    }
    return NULL;
}

static ASTType* ASTExpr::getTypeOrEnum() {
    if (!this || this->typeType != TYObject) return NULL;

    switch (this->kind) {
    case tkFunctionCallResolved:
        return this->func->returnSpec->type;
    case tkIdentifierResolved:
    case tkSubscriptResolved:
        return this->var->typeSpec->type;

    case tkPeriod: {
        ASTType* type = this->left->getObjectType();
        if (!type->isEnum) type = this->right->getObjectType();
        return type;
    }
    case tkOpComma:
        return this->left->getTypeOrEnum();
    default:
        break;
    }
    return NULL;
}

const char* ASTExpr::typeName() {
    if (!this) return "";
    const char* ret = this->typeType->TypeType::name();
    if (!ret) return "<unknown>";
    if (*ret) return ret;

    switch (this->kind) {
    case tkFunctionCallResolved:
        return this->func->returnSpec->type->name;
    case tkIdentifierResolved:
    case tkSubscriptResolved:
        return this->var->typeSpec->type->name;

    case tkPeriod: {
        ASTType* type = this->left->getObjectType();
        return (type->isEnum) ? type->name : this->right->typeName();
    }
    case tkOpComma:
        return this->left->typeName();
    default:
        break;
    }
    return "<invalid>";
}

void ASTExpr::catarglabels() {
    switch (this->kind) {
    case tkOpComma:
        this->left->catarglabels();
        this->right->catarglabels();
        break;
    case tkOpAssign:
        printf("_%s", this->left->string);
        break;
    default:
        break;
    }
}

int ASTExpr::strarglabels(char* buf, int bufsize) {
    int ret = 0;
    switch (this->kind) {
    case tkOpComma:
        ret += this->left->strarglabels(buf, bufsize);
        ret += this->right->strarglabels(buf + ret, bufsize - ret);
        break;
    case tkOpAssign:
        ret += snprintf(buf, bufsize, "_%s", this->left->string);
        break;
    default:
        break;
    }
    return ret;
}

int ASTExpr::countCommaList() {
    int i = 0;
    ASTExpr* expr = this;
    for (i = 1; expr->right && expr->kind == tkOpComma; i++) expr = expr->right;
    return i;
}
