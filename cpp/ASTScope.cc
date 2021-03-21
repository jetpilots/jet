#include "base.hh"

class ASTScope {
    List<ASTExpr&> stmts;
    List<ASTVar&> locals;
    struct ASTScope* parent;

private:
    void lowerElementalOps() {
        for (auto& stmt : this->stmts) {

            if (stmt->isCtrlExpr() && stmt->body)
                stmt->body->lowerElementalOps();

            if (!stmt->elemental) continue;

            ASTExpr* ifblk = NEW(ASTExpr);
            ifblk->kind = tkKeyword_if;
            ifblk->left = NEW(ASTExpr);
            ifblk->left->kind = tkNumber;
            ifblk->string = "1";
        }
    }

    void promoteCandidates() {
        int tmpCount = 0;
        ASTExpr* pc = NULL;
        List<ASTExpr*>* prev = NULL;
        foreachn(ASTExpr*, stmt, stmts, this->stmts) {

            if (stmt->isCtrlExpr() && stmt->body)
                stmt->body->promoteCandidates();

        startloop:

            if (!(pc = stmt->ASTExpr_findPromotionCandidate())) {
                prev = stmts;
                continue;
            }
            if (pc == stmt) {

                prev = stmts;
                continue;
            }

            ASTExpr* pcClone = NEW(ASTExpr);
            *pcClone = *pc;

            ASTVar* tmpvar = NEW(ASTVar);
            tmpvar->name = newTmpVarName(++tmpCount, 'p');
            tmpvar->typeSpec = NEW(ASTTypeSpec);

            &this->locals->append(tmpvar);

            pc->kind = tkIdentifierResolved;
            pc->prec = 0;
            pc->var = tmpvar;

            if (!pcClone->left)
                pcClone->left = pc;
            else if (pcClone->left->kind != tkOpComma) {

                ASTExpr* com = NEW(ASTExpr);

                com->prec = tkOpComma->getPrecedence();
                com->kind = tkOpComma;
                com->left = pcClone->left;
                com->right = pc;
                pcClone->left = com;
            } else {
                ASTExpr* argn = pcClone->left;
                while (
                    argn->kind == tkOpComma && argn->right->kind == tkOpComma)
                    argn = argn->right;
                ASTExpr* com = NEW(ASTExpr);

                com->prec = tkOpComma->getPrecedence();
                com->kind = tkOpComma;
                com->left = argn->right;
                com->right = pc;
                argn->right = com;
            }

            if (!prev) {
                this->stmts = PtrList_with(pcClone);
                this->stmts->next = stmts;
                prev = this->stmts;
            } else {
                prev->next = PtrList_with(pcClone);
                prev->next->next = stmts;
                prev = prev->next;
            }

            goto startloop;

            prev = stmts;
        }
    }

    void emit(int level) {
        for (auto& local : this->locals)
            if (local->used) {
                local->emit(level, false);
                puts(";");
            }
        for (auto& stmt : this->stmts) {
            if (stmt->kind == tkLineComment) continue;

            if (genLineNumbers) printf("#line %d\n", stmt->line);

            if (stmt->kind != tkVarAssign || stmt->var->used) {
                if (genCoverage || genLineProfile)
                    printf("    /************/ ");
                if (genCoverage) printf("JET_COVERAGE_UP(%d); ", stmt->line);
                if (genLineProfile) {

                    printf("JET_PROFILE_LINE(%d);", stmt->line);
                }
                if (genCoverage || genLineProfile) puts(" /************/");
            }

            stmt->emit(level);
            if (!stmt->isCtrlExpr() && stmt->kind != tkKeyword_return)
                puts(";");
            else
                puts("");

            if (ASTExpr_throws(stmt))
                printf("%.*sTRACE_IF_ERROR;\n", level, spaces);
        }
    }

    void lint(int level) {
        foreachn(ASTExpr*, expr, exprList, this->stmts) {
            switch (expr->kind) {
            case tkKeyword_case:
            case tkKeyword_match:
                printf("%.*s", level, spaces);
                printf("%s ", expr->kind->repr(false));
                if (expr->left) expr->left->lint(0, true, false);
                puts("");

                if (expr->kind == tkKeyword_match) {
                    if (expr->body) expr->body->lint(level);
                    printf("%.*send %s\n", level, spaces, "");
                } else {
                    if (expr->body) expr->body->lint(level + STEP);
                }
                break;

            case tkKeyword_for:
            case tkKeyword_if:
            case tkKeyword_elif:
            case tkKeyword_else:
            case tkKeyword_while: {
                printf("%.*s", level, spaces);
                printf("%s ", expr->kind->repr(false));
                if (expr->left) expr->left->lint(0, true, false);
                puts("");
                if (expr->body) expr->body->lint(level + STEP);

                if (expr->kind == tkKeyword_if || expr->kind == tkKeyword_elif)
                    if (exprList->next) {
                        ASTExpr* next = exprList->next->item;
                        if (next->kind == tkKeyword_else
                            || next->kind == tkKeyword_elif)
                            break;
                    }
                printf("%.*send %s\n", level, spaces, "");
            } break;
            default:
                expr->lint(level, true, false);
                puts("");
            }
        }
    }

    size_t size() {
        size_t size = 0, sum = 0, subsize = 0, maxsubsize = 0;

        for (auto& stmt : this->stmts) {
            switch (stmt->kind) {
            case tkKeyword_if:
            case tkKeyword_else:
            case tkKeyword_for:
            case tkKeyword_while:
                subsize = stmt->body->calcSizeUsage();
                if (subsize > maxsubsize) maxsubsize = subsize;
                break;
            default:;
            }
        }

        for (auto& var : this->locals) {
            size = var->typeSpec->typeType->TypeType::size();
            if (!size)
                eprintf("warning: cannot find size for '%s' at %d:%d\n",
                    var->name, var->line, var->col);
            if (var->used) sum += size;
        }

        sum += maxsubsize;
        return sum;
    }

    ASTVar* var(const char* name) {

        for (auto& local : this->locals)
            if (name ^= local->name) return local;
        if (this->parent) return this->parent->getVar(name);
        return NULL;
    }
};