struct Scope {
    List<Var&> vars;
    List<Expr&> stmts;
    Scope* parent;
    Var* getVar(const char* name) {
        for (Var& v : vars)
            if (!strcasecmp(v.name, name)) return &v;
        return parent ? parent->getVar(name) : nullptr;
    }
    size_t calcSizeUsage() {
        size_t size = 0, sum = 0, subsize = 0, maxsubsize = 0;
        // all variables must be resolved before calling this
        for (Expr& stmt : stmts) {
            switch (stmt.kind) {
            case tkKeyword_if:
            case tkKeyword_else:
            case tkKeyword_for:
            case tkKeyword_while:
                subsize = stmt.body->calcSizeUsage();
                if (subsize > maxsubsize) maxsubsize = subsize;
                break;
            default:;
            }
        }
        // some vars are not assigned, esp. temporaries _1 _2 etc.
        for (Var& var : vars) {
            size = (var.typeInfo.size());
            if (!size)
                eprintf("warning: cannot find size for '%s' at %d:%d\n",
                    var.name, var.loc.line, var.loc.col);
            if (var.used) sum += size;
        }
        // add the largest size among the sizes of the sub-scopes
        sum += maxsubsize;
        return sum;
    }
};
static const int szScope = sizeof(Scope);
static_assert(sizeof(Scope) == 56);
