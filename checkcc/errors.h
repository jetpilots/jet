#define RELF(s) (*s == '/' ? "" : "./"), s

#define fatal(str, ...)                                                    \
    {                                                                      \
        eprintf(str, __VA_ARGS__);                                         \
        exit(1);                                                           \
    }

static void Parser_errorIncrement(Parser* const this)
{
    if (++this->errCount >= this->errLimit)
        fatal("\ntoo many errors (%d), quitting\n", this->errLimit);
}

static void Parser_errorExpectedToken(
    Parser* const this, const TokenKind expected)
{
    eprintf("\n(%d) \e[31merror:\e[0m at %s%s:%d:%d\n"
            "      expected '%s' found '%s'\n",
        this->errCount + 1, RELF(this->filename), this->token.line,
        this->token.col, TokenKind_repr(expected, false),
        TokenKind_repr(this->token.kind, false));
    Parser_errorIncrement(this);
}

static void Parser_errorParsingExpr(Parser* const this)
{
    eprintf("\n(%d) \e[31merror:\e[0m syntax error at %s%s:%d/%d\n",
        this->errCount + 1, RELF(this->filename), this->token.line - 1,
        this->token.line);
    Parser_errorIncrement(this);
}

static void Parser_errorInvalidIdent(Parser* const this)
{
    eprintf("\n(%d) \e[31merror:\e[0m invalid name '%.*s' at "
            "%s%s:%d:%d\n",
        this->errCount + 1, this->token.matchlen, this->token.pos,
        RELF(this->filename), this->token.line, this->token.col);
    Parser_errorIncrement(this);
}

static void Parser_errorInvalidTypeMember(Parser* const this)
{
    eprintf("\n(%d) \e[31merror:\e[0m invalid member at %s%s:%d\n",
        this->errCount + 1, RELF(this->filename), this->token.line - 1);
    Parser_errorIncrement(this);
}

static void Parser_errorUnrecognizedVar(
    Parser* const this, const ASTExpr* const expr)
{
    eprintf("\n(%d) \e[31merror:\e[0m unknown variable "
            "\e[34m%s\e[0m at %s%s:%d:%d\n",
        this->errCount + 1, expr->string, RELF(this->filename), expr->line,
        expr->col);
    Parser_errorIncrement(this);
}

static void Parser_warnUnusedArg(
    Parser* const this, const ASTVar* const var)
{
    eprintf("\n(%d) \e[33mwarning:\e[0m unused argument "
            "\e[34m%s\e[0m at %s%s:%d:%d\n",
        ++this->warnCount, var->name, RELF(this->filename), var->line,
        var->col);
}

static void Parser_warnUnusedVar(
    Parser* const this, const ASTVar* const var)
{
    eprintf("\n(%d) \e[33mwarning:\e[0m unused variable "
            "\e[34m%s\e[0m at %s%s:%d:%d\n",
        ++this->warnCount, var->name, RELF(this->filename), var->line,
        var->col);
}

static void Parser_errorDuplicateVar(
    Parser* const this, const ASTVar* const var, const ASTVar* const orig)
{
    eprintf("\n(%d) \e[31merror:\e[0m duplicate variable "
            "\e[34m%s\e[0m at %s%s:%d:%d\n   "
            "          already declared at %s%s:%d:%d\n",
        this->errCount + 1, var->name, RELF(this->filename), var->line,
        var->col, RELF(this->filename), orig->line, orig->col);
    Parser_errorIncrement(this);
}

static void Parser_errorDuplicateType(Parser* const this,
    const ASTType* const type, const ASTType* const orig)
{
    eprintf("\n(%d) \e[31merror:\e[0m duplicate type "
            "\e[34m%s\e[0m at %s%s:%d:%d\n   "
            "          already declared at %s%s:%d:%d\n",
        this->errCount + 1, type->name, RELF(this->filename), type->line,
        type->col, RELF(this->filename), orig->line, orig->col);
    Parser_errorIncrement(this);
}

static void Parser_errorTypeInheritsSelf(
    Parser* const this, const ASTType* const type)
{
    eprintf("\n(%d) \e[31merror:\e[0m type inherits from self "
            "\e[34m%s\e[0m at %s%s:%d:%d\n",
        this->errCount + 1, type->name, RELF(this->filename), type->line,
        type->col);
    Parser_errorIncrement(this);
}
static void Parser_errorCtorHasType(Parser* const this,
    const ASTFunc* const func, const ASTType* const orig)
{
    eprintf("\n(%d) \e[31merror:\e[0m constructor needs no return "
            "type: \e[34m%s\e[0m at %s%s:%d:%d\n"
            "             type declared at %s%s:%d:%d\n"
            "             remove the return type specification\n",
        this->errCount + 1, func->name, RELF(this->filename), func->line, 1,
        RELF(this->filename), orig->line, orig->col);
    Parser_errorIncrement(this);
}
static void Parser_warnCtorCase(Parser* const this,
    const ASTFunc* const func, const ASTType* const orig)
{
    eprintf("\n(%d) \e[33mwarning:\e[0m wrong case "
            "\e[34m%s\e[0m for constructor at %s%s:%d:%d\n"
            "             type declared at %s%s:%d:%d\n"
            "             change it to \e[34m%s\e[0m or lint the file\n",
        ++this->warnCount, func->name, RELF(this->filename), func->line, 1,
        RELF(this->filename), orig->line, orig->col, orig->name);
}

static void Parser_errorDuplicateFunc(Parser* const this,
    const ASTFunc* const func, const ASTFunc* const orig)
{
    eprintf("\n(%d) \e[31merror:\e[0m duplicate function "
            "\e[34m%s\e[0m at %s%s:%d:%d\n"
            "             already declared at %s%s:%d:%d\n"
            "             selector is \e[34m%s\e[0m\n",
        this->errCount + 1, func->name, RELF(this->filename), func->line, 1,
        RELF(this->filename), orig->line, 1, func->selector);
    Parser_errorIncrement(this);
}

static void Parser_errorUnrecognizedFunc(Parser* const this,
    const ASTExpr* const expr, const char* const selector)
{
    eprintf("\n(%d) \e[31merror:\e[0m can't resolve call to "
            "\e[34m%s\e[0m at %s%s:%d:%d\n"
            "        selector is \e[34m%s\e[0m (%d args)\n",
        this->errCount + 1, expr->string, RELF(this->filename), expr->line,
        expr->col, selector, ASTExpr_countCommaList(expr->left));
    Parser_errorIncrement(this);
}

static void Parser_errorArgsCountMismatch(
    Parser* const this, const ASTExpr* const expr)
{
    assert(expr->kind == tkFunctionCallResolved);
    eprintf("\n(%d) \e[31merror:\e[0m arg count mismatch for "
            "\e[34m%s\e[0m at %s%s:%d:%d\n"
            "          have %d args, need %d, func defined at %s%s:%d\n",
        this->errCount + 1, expr->func->name, RELF(this->filename),
        expr->line, expr->col, ASTExpr_countCommaList(expr->left),
        expr->func->argCount, RELF(this->filename), expr->func->line);
    Parser_errorIncrement(this);
}

static void Parser_errorIndexDimsMismatch(
    Parser* const this, const ASTExpr* const expr)
{
    assert(expr->kind == tkSubscriptResolved);
    int reqdDims = expr->var->typeSpec->dims;
    if (not reqdDims)
        eprintf("\n(%d) \e[31merror:\e[0m not an array: "
                "\e[34m%s\e[0m at %s%s:%d:%d\n"
                "          indexing a non-array with %d dims, var defined "
                "at %s%s:%d\n",
            this->errCount + 1, expr->var->name, RELF(this->filename),
            expr->line, expr->col, ASTExpr_countCommaList(expr->left),
            RELF(this->filename), expr->var->typeSpec->line);
    else
        eprintf(
            "(%d) \e[31merror:\e[0m index dims mismatch for "
            "\e[34m%s\e[0m at %s%s:%d:%d\n"
            "          have %d indexes, need %d, var defined at %s%s:%d\n",
            this->errCount + 1, expr->var->name, RELF(this->filename),
            expr->line, expr->col, ASTExpr_countCommaList(expr->left),
            reqdDims, RELF(this->filename), expr->var->typeSpec->line);
    Parser_errorIncrement(this);
}

static void Parser_errorMissingInit(
    Parser* const this, const ASTExpr* const expr)
{
    assert(expr->kind == tkVarAssign);
    eprintf("\n(%d) \e[31merror:\e[0m missing initializer for "
            "\e[34m%s\e[0m at %s%s:%d-%d\n",
        this->errCount + 1, expr->var->name, RELF(this->filename),
        expr->line - 1, expr->line);
    Parser_errorIncrement(this);
}

static void Parser_errorUnrecognizedType(
    Parser* const this, const ASTTypeSpec* const typeSpec)
{
    eprintf("\n(%d) \e[31merror:\e[0m unknown typespec \e[33m%s\e[0m "
            "at %s%s:%d:%d\n",
        this->errCount + 1, typeSpec->name, RELF(this->filename),
        typeSpec->line, typeSpec->col);
    Parser_errorIncrement(this);
}

static void Parser_errorUnrecognizedCtor(
    Parser* const this, const ASTFunc* const func)
{
    eprintf("\n(%d) \e[31merror:\e[0m unknown type \e[33m%s\e[0m "
            "for constructor at %s%s:%d\n",
        this->errCount + 1, func->name, RELF(this->filename), func->line);
    Parser_errorIncrement(this);
}

static void Parser_errorTypeMismatchBinOp(
    Parser* const this, const ASTExpr* const expr)
{
    eprintf("\n(%d) \e[31merror:\e[0m type mismatch at %s%s:%d:%d\n"
            "             can't apply '\e[34m%s\e[0m' to \e[34m%s\e[0m"
            " and \e[34m%s\e[0m\n",
        this->errCount + 1, RELF(this->filename), expr->line, expr->col,
        TokenKind_repr(expr->kind, false), ASTExpr_typeName(expr->left),
        ASTExpr_typeName(expr->right));
    Parser_errorIncrement(this);
}

static void Parser_errorReadOnlyVar(
    Parser* const this, const ASTExpr* const expr)
{
    eprintf("\n(%d) \e[31merror:\e[0m mutating read-only variable '"
            "\e[34m%s\e[0m' at %s%s:%d:%d\n",
        this->errCount + 1, expr->var->name, RELF(this->filename),
        expr->line, expr->col);
    Parser_errorIncrement(this);
}

static void Parser_errorInvalidTypeForOp(
    Parser* const this, const ASTExpr* const expr)
{
    eprintf("\n(%d) \e[31merror:\e[0m invalid types for operator '"
            "\e[34m%s\e[0m' at %s%s:%d:%d\n",
        this->errCount + 1, TokenKind_repr(expr->kind, false),
        RELF(this->filename), expr->line, expr->col);
    Parser_errorIncrement(this);
}

static void Parser_errorArgTypeMismatch(
    Parser* const this, const ASTExpr* const expr, const ASTVar* const var)
{
    eprintf("\n(%d) \e[31merror:\e[0m type mismatch for argument '"
            "\e[34m%s\e[0m' at %s%s:%d:%d\n",
        this->errCount + 1, var->name, RELF(this->filename), expr->line,
        expr->col);
    Parser_errorIncrement(this);
}

static void Parser_errorUnexpectedToken(Parser* const this)
{
    eprintf("\n(%d) \e[31merror:\e[0m at %s%s:%d:%d\n      unexpected "
            "token '%.*s'\n",
        this->errCount + 1, RELF(this->filename), this->token.line,
        this->token.col, this->token.matchlen, this->token.pos);
    Parser_errorIncrement(this);
}

static void Parser_errorUnexpectedExpr(
    Parser* const this, const ASTExpr* const expr)
{
    eprintf("\n(%d) \e[31merror:\e[0m at %s%s:%d:%d\n"
            "      unexpected expr '%s'",
        this->errCount + 1, RELF(this->filename), expr->line, expr->col,
        expr->opPrec ? TokenKind_repr(expr->kind, false) : expr->name);
    Parser_errorIncrement(this);
}
