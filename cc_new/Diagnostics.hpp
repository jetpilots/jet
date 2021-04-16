enum class ErrKind : char {
    missing,
    invalid,
    unknown,
    unexpected,
    duplicate,
    unused
};
enum class ErrItem : char {
    type,
    func,
    var,
    arg,
    mvar,
    expr,
    operand,
    token,
    import,
    main
};

static const char* ErrKind_str[] = { //
    [(int)ErrKind::missing] = "missing",
    [(int)ErrKind::invalid] = "invalid",
    [(int)ErrKind::unknown] = "unknown",
    [(int)ErrKind::unexpected] = "unexpected",
    [(int)ErrKind::duplicate] = "duplicate",
    [(int)ErrKind::unused] = "unused"
};

static const char* ErrItem_str[] = { //
    [(int)ErrItem::type] = "type",
    [(int)ErrItem::func] = "function",
    [(int)ErrItem::var] = "variable",
    [(int)ErrItem::arg] = "argument",
    [(int)ErrItem::import] = "import",
    [(int)ErrItem::mvar] = "member variable",
    [(int)ErrItem::expr] = "expression",
    [(int)ErrItem::operand] = "operand",
    [(int)ErrItem::token] = "token",
    [(int)ErrItem::main] = "function main()"
};

enum class Severity : char { error, warning, info };
static const char* Severity_str[] = { //
    [(int)Severity::error] = "error",
    [(int)Severity::warning] = "warning",
    [(int)Severity::info] = "info"
};

struct Issue {
    SourceLoc loc;
    ErrKind kind;
    ErrItem item, item2;
    Severity severity;
    union Ptrs {
        Var* var;
        Func* func;
        Module* module;
        Type* type;
        Expr* expr;
        void* p;
    } ref, ref2;
    Issue(ErrKind kind, ErrItem item, Severity sev, SourceLoc loc, void* p)
        : loc(loc)
        , kind(kind)
        , item(item)
        , severity(sev)
        , ref({ .p = p }) { }
    Issue(ErrKind kind, ErrItem item, ErrItem item2, Severity sev,
        SourceLoc loc, void* p, void* p2)
        : loc(loc)
        , kind(kind)
        , item(item)
        , item2(item2)
        , severity(sev)
        , ref({ .p = p })
        , ref2({ .p = p2 }) { }
};
static_assert(sizeof(Issue) == 24, "");

struct Diagnostics {
    const char* filename = "";
    Array<Issue> issues = {};
    Array<char*> lines = {};
    bool collect = false;
    long count = 0, maxErrors = 20;
    // when you create sub-diagnostics set their maxErrors to 20-errs.count

    ~Diagnostics() {
        if (lines.count() and lines[0]) delete lines[0];
    }

    void printHeader(Issue& issue) {
        const char* severity = Severity_str[(int)issue.severity];
        printf("%s:%d:%d-%d: %s: #%d ", filename, issue.loc.line, issue.loc.col,
            issue.loc.col + issue.loc.len, severity, ++count);
    }
    void print(Issue& issue) {
        const char* str = "(unknown)";
        switch (issue.item) {
        case ErrItem::func: str = issue.ref.func->name; break;
        case ErrItem::var: str = issue.ref.var->name; break;
        case ErrItem::type: str = issue.ref.func->name; break;
        case ErrItem::expr:
        case ErrItem::operand:
        case ErrItem::token: str = issue.ref.var->expr->repr(); break;
        }
        printf("%s %s '%s'\n", ErrKind_str[(int)issue.kind],
            ErrItem_str[(int)issue.item], str);
    }
    void warning(ErrKind kind, ErrItem item, SourceLoc loc, void* ref) {
        Issue issue(kind, item, Severity::warning, loc, ref);
    }
    void error(ErrKind kind, ErrItem item, SourceLoc loc, void* ref) {
        Issue issue(kind, item, Severity::error, loc, ref);
        if (collect) issues.push(issue);
        print(issue);
        if (count >= maxErrors) { exit(8); }
    }
    void missingMain() {
        error(ErrKind::missing, ErrItem::main, { 1, 1, 1 }, nullptr);
    }
    void unknownVar(Expr& expr) {
        error(ErrKind::unknown, ErrItem::var, {}, &expr);
    }
    void unknownFunc(Expr& expr) {
        error(ErrKind::unknown, ErrItem::func, {}, &expr);
    }
    void unusedVar(Var& var) {
        warning(ErrKind::unused, ErrItem::var, var.loc, &var);
    }
    void unusedArg(Var& var, Func& func) {
        warning(ErrKind::unused, ErrItem::arg, var.loc, &var);
    }
    void unusedMemb(Var& var, Type& type) {
        warning(ErrKind::unused, ErrItem::mvar, var.loc, &var);
    }
    void unusedType(Type& type) {
        warning(ErrKind::unused, ErrItem::type, type.loc, &type);
    }
    void unusedFunc(Func& func) {
        warning(ErrKind::unused, ErrItem::func, func.loc, &func);
    }
    void unusedImport(Import& import) {
        warning(ErrKind::unused, ErrItem::import, import.loc, &import);
    }
    void syntaxError(Expr& expr) { }
    void unexpectedToken(Token& token, const char* msg) { }
    void initDimsMismatch(Expr& expr, int haveDims) { }
    void initMismatch(Expr& expr) { }
    void indexDimsMismatch(Expr& expr, int haveDims) { }
    void typeMismatch(Expr& expr1, Expr& expr2) { }
    void missingInit(Expr& expr) {
        error(ErrKind::missing, ErrItem::init, {}, &expr);
    }
    void unexpectedExpr(Expr& expr, const char* message) {
        error(ErrKind::unexpected, ErrItem::expr, {}, &expr);
    }
};