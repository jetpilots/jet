#define RELF(s) (*s == '/' ? "" : "./"), s

#define fatal(str, ...)                                                        \
    {                                                                          \
        eprintf(str, __VA_ARGS__);                                             \
        exit(1);                                                               \
    }

static void Parser_errorIncrement(Parser* const parser)
{
    if (++parser->errCount >= parser->errLimit)
        fatal("\ntoo many errors (%d), quitting\n", parser->errLimit);
}

static void Parser_errorExpectedToken(
    Parser* const parser, const TokenKind expected)
{
    eprintf("\n(%d) \e[31merror:\e[0m at %s%s:%d:%d\n"
            "      expected '%s' (%s) but found '%s'\n",
        parser->errCount + 1, RELF(parser->filename), parser->token.line,
        parser->token.col, TokenKind_repr(expected, false),
        TokenKind_str[expected] + 2, TokenKind_repr(parser->token.kind, false));
    Parser_errorIncrement(parser);
}

static void Parser_errorParsingExpr(Parser* const parser, ASTExpr* expr)
{
    eprintf("\n(%d) \e[31merror:\e[0m syntax error at %s%s:%d/%d\n"
            "    probably around %d:%d\n",
        parser->errCount + 1, RELF(parser->filename), parser->token.line - 1,
        parser->token.line, expr->line, expr->col);
    Parser_errorIncrement(parser);
}

static void Parser_errorInvalidIdent(Parser* const parser)
{
    eprintf("\n(%d) \e[31merror:\e[0m invalid name '%.*s' at "
            "%s%s:%d:%d\n",
        parser->errCount + 1, parser->token.matchlen, parser->token.pos,
        RELF(parser->filename), parser->token.line, parser->token.col);
    Parser_errorIncrement(parser);
}

static void Parser_errorInvalidTypeMember(Parser* const parser)
{
    eprintf("\n(%d) \e[31merror:\e[0m invalid member at %s%s:%d\n",
        parser->errCount + 1, RELF(parser->filename), parser->token.line - 1);
    Parser_errorIncrement(parser);
}

static void Parser_errorUnrecognizedVar(
    Parser* const parser, const ASTExpr* const expr)
{
    eprintf("\n(%d) \e[31merror:\e[0m unknown variable "
            "\e[34m%s\e[0m at %s%s:%d:%d\n",
        parser->errCount + 1, expr->string, RELF(parser->filename), expr->line,
        expr->col);
    Parser_errorIncrement(parser);
}

static void Parser_errorUnrecognizedMember(
    Parser* const parser, const ASTType* const type, const ASTExpr* const expr)
{
    eprintf("\n(%d) \e[31merror:\e[0m type \e[34m%s\e[0m has no member "
            "\e[34m%s\e[0m at %s%s:%d:%d\n",
        parser->errCount + 1, type->name, expr->string, RELF(parser->filename),
        expr->line, expr->col);
    Parser_errorIncrement(parser);
}

static void Parser_warnUnusedArg(Parser* const parser, const ASTVar* const var)
{
    if (not parser->warnUnusedArg) return;
    eprintf("\n(%d) \e[33mwarning:\e[0m unused argument "
            "\e[34m%s\e[0m at %s%s:%d:%d\n",
        ++parser->warnCount, var->name, RELF(parser->filename), var->line,
        var->col);
}

static void Parser_warnUnusedVar(Parser* const parser, const ASTVar* const var)
{
    if (not parser->warnUnusedVar) return;
    eprintf("\n(%d) \e[33mwarning:\e[0m unused variable "
            "\e[34m%s\e[0m at %s%s:%d:%d\n",
        ++parser->warnCount, var->name, RELF(parser->filename), var->line,
        var->col);
}

static void Parser_warnUnusedFunc(
    Parser* const parser, const ASTFunc* const func)
{
    if (not parser->warnUnusedFunc) return;
    eprintf("\n(%d) \e[33mwarning:\e[0m unused function "
            "\e[34m%s\e[0m at %s%s:%d\n"
            "            selector is \e[34m%s\e[0m\n",
        ++parser->warnCount, func->name, RELF(parser->filename), func->line,
        func->selector);
}

static void Parser_warnUnusedType(
    Parser* const parser, const ASTType* const type)
{
    if (not parser->warnUnusedType) return;
    eprintf("\n(%d) \e[33mwarning:\e[0m unused type "
            "\e[34m%s\e[0m at %s%s:%d:%d\n",
        ++parser->warnCount, type->name, RELF(parser->filename), type->line,
        type->col);
}

static void Parser_errorDuplicateVar(
    Parser* const parser, const ASTVar* const var, const ASTVar* const orig)
{
    eprintf("\n(%d) \e[31merror:\e[0m duplicate variable "
            "\e[34m%s\e[0m at %s%s:%d:%d\n   "
            "          already declared at %s%s:%d:%d\n",
        parser->errCount + 1, var->name, RELF(parser->filename), var->line,
        var->col, RELF(parser->filename), orig->line, orig->col);
    Parser_errorIncrement(parser);
}

static void Parser_errorDuplicateType(
    Parser* const parser, const ASTType* const type, const ASTType* const orig)
{
    if (orig)
        eprintf("\n(%d) \e[31merror:\e[0m duplicate type "
                "\e[34m%s\e[0m at %s%s:%d:%d\n   "
                "          already declared at %s%s:%d:%d\n",
            parser->errCount + 1, type->name, RELF(parser->filename),
            type->line, type->col, RELF(parser->filename), orig->line,
            orig->col);
    else
        eprintf("\n(%d) \e[31merror:\e[0m invalid type name "
                "\e[34m%s\e[0m at %s%s:%d:%d\n   "
                "          refers to a built-in type\n",
            parser->errCount + 1, type->name, RELF(parser->filename),
            type->line, type->col);
    Parser_errorIncrement(parser);
}

static void Parser_errorDuplicateEnum(
    Parser* const parser, const ASTEnum* const en, const ASTEnum* const orig)
{
    if (orig)
        eprintf("\n(%d) \e[31merror:\e[0m duplicate enum "
                "\e[34m%s\e[0m at %s%s:%d:%d\n   "
                "          already declared at %s%s:%d:%d\n",
            parser->errCount + 1, en->name, RELF(parser->filename), en->line,
            en->col, RELF(parser->filename), orig->line, orig->col);
    else
        eprintf("\n(%d) \e[31merror:\e[0m invalid enum name "
                "\e[34m%s\e[0m at %s%s:%d:%d\n   "
                "          refers to a built-in type\n",
            parser->errCount + 1, en->name, RELF(parser->filename), en->line,
            en->col);
    Parser_errorIncrement(parser);
}

static void Parser_errorTypeInheritsSelf(
    Parser* const parser, const ASTType* const type)
{
    eprintf("\n(%d) \e[31merror:\e[0m type inherits from parser "
            "\e[34m%s\e[0m at %s%s:%d:%d\n",
        parser->errCount + 1, type->name, RELF(parser->filename), type->line,
        type->col);
    Parser_errorIncrement(parser);
}
static void Parser_errorCtorHasType(
    Parser* const parser, const ASTFunc* const func, const ASTType* const orig)
{
    eprintf("\n(%d) \e[31merror:\e[0m constructor needs no return "
            "type: \e[34m%s\e[0m at %s%s:%d:%d\n"
            "             type declared at %s%s:%d:%d\n"
            "             remove the return type specification\n",
        parser->errCount + 1, func->name, RELF(parser->filename), func->line, 1,
        RELF(parser->filename), orig->line, orig->col);
    Parser_errorIncrement(parser);
}
static void Parser_warnCtorCase(Parser* const parser, const ASTFunc* const func)
{
    const ASTType* const orig = func->returnSpec->type;
    eprintf("\n(%d) \e[33mwarning:\e[0m wrong case "
            "\e[34m%s\e[0m for constructor at %s%s:%d:%d\n"
            "             type declared at %s%s:%d:%d\n"
            "             change it to \e[34m%s\e[0m or lint the file\n",
        ++parser->warnCount, func->name, RELF(parser->filename), func->line, 1,
        RELF(parser->filename), orig->line, orig->col, orig->name);
}

static void Parser_errorDuplicateFunc(
    Parser* const parser, const ASTFunc* const func, const ASTFunc* const orig)
{
    eprintf("\n(%d) \e[31merror:\e[0m duplicate function "
            "\e[34m%s\e[0m at %s%s:%d:%d\n"
            "             already declared at %s%s:%d:%d\n"
            "             selector is \e[34m%s\e[0m\n",
        parser->errCount + 1, func->name, RELF(parser->filename), func->line, 1,
        RELF(parser->filename), orig->line, 1, func->selector);
    Parser_errorIncrement(parser);
}

static void Parser_errorDuplicateTest(
    Parser* const parser, const ASTTest* const test, const ASTTest* const orig)
{
    eprintf("\n(%d) \e[31merror:\e[0m duplicate test "
            "\e[34m%s\e[0m at %s%s:%d:%d\n"
            "             already declared at %s%s:%d:%d\n",
        parser->errCount + 1, test->name, RELF(parser->filename), test->line, 1,
        RELF(parser->filename), orig->line, 1);
    Parser_errorIncrement(parser);
}

static void Parser_errorUnrecognizedFunc(
    Parser* const parser, const ASTExpr* const expr, const char* const selector)
{
    if (*selector == '<') return; // invalid type; already error'd
    eprintf(
        "\n\e[31;1;4m ERROR                                               "
        "                       \e[0m\n %s%s:%d:%d:\n This "
        "\e[1m%s\e[0m call could not be resolved.\n"
        " There is no method with selector \e[1m%s\e[0m and %d arguments.\n",
        RELF(parser->filename), expr->line, expr->col, expr->string, selector,
        ASTExpr_countCommaList(expr->left));
    Parser_errorIncrement(parser);
}

static void Parser_errorStringInterp(
    Parser* const parser, const ASTExpr* const expr, const char* const pos)
{
    eprintf("\n\e[31;1;4m ERROR                                               "
            "                       \e[0m\n %s%s:%d:%d:\n There is a syntax "
            "error within"
            " this string:\n"
            "     %s\"\n"
            " $ is used to specify a variable whose value is to be "
            "interpolated into the\n"
            " string. If you want a literal $ sign in the string, write it as "
            "\\$.\n",
        RELF(parser->filename), expr->line,
        expr->col + (int)(pos - expr->string), expr->string);
    Parser_errorIncrement(parser);
}

static void Parser_errorCallingFuncWithVoid(
    Parser* const parser, const ASTExpr* const expr, const ASTExpr* const arg)
{
    eprintf("\n\e[31;1;4m ERROR                                               "
            "                       \e[0m\n %s%s:%d:%d:\n The "
            "\e[1m%s\e[0m function does not return a value.\n"
            " You cannot use it as an argument in the call to \e[1m%s\e[0m.\n",
        RELF(parser->filename), expr->line, expr->col, arg->func->name,
        expr->string);
    Parser_errorIncrement(parser);
}

static void Parser_errorInheritanceCycle(
    Parser* const parser, const ASTType* const type)
{
    eprintf("\n\e[31;1;4m ERROR                                               "
            "                       \e[0m\n %s%s:%d:%d:\n Type "
            "\e[1m%s\e[0m has a cycle in its inheritance graph.",
        RELF(parser->filename), type->line, type->col, type->name);
    ASTType* super = type->super->type;
    eputs("\e[;2m");
    do {
        eprintf("\n extends \e[;1;2m%s\e[0;2m (defined at %s%s:%d:%d)",
            super->name, RELF(parser->filename), super->line, super->col);
        if (super == super->super->type or super == type) {
            if (type != super) eprintf("\n extends %s", super->name);
            break;
        }
        super = super->super->type;
    } while (1);
    eputs("\n ...\e[0m\n");
    Parser_errorIncrement(parser);
}

static void Parser_errorConstructorHasCycle(
    Parser* const parser, const ASTType* const type)
{
    eprintf("\n\e[31;1;4m ERROR                                               "
            "                       \e[0m\n %s%s:%d:%d:\n Type "
            "\e[1m%s\e[0m has an endless cycle in its initialization.\n",
        RELF(parser->filename), type->line, type->col, type->name);
    Parser_errorIncrement(parser);
}

static void Parser_errorArgsCountMismatch(
    Parser* const parser, const ASTExpr* const expr)
{
    assert(expr->kind == tkFunctionCallResolved);
    eprintf("\n(%d) \e[31merror:\e[0m arg count mismatch for "
            "\e[34m%s\e[0m at %s%s:%d:%d\n"
            "          have %d args, need %d, func defined at %s%s:%d\n",
        parser->errCount + 1, expr->func->name, RELF(parser->filename),
        expr->line, expr->col, ASTExpr_countCommaList(expr->left),
        expr->func->argCount, RELF(parser->filename), expr->func->line);
    Parser_errorIncrement(parser);
}

static void Parser_errorIndexDimsMismatch(
    Parser* const parser, const ASTExpr* const expr)
{
    assert(expr->kind == tkSubscriptResolved);
    int reqdDims = expr->var->typeSpec->dims;
    if (not reqdDims)
        eprintf("\n(%d) \e[31merror:\e[0m not an array: "
                "\e[34m%s\e[0m at %s%s:%d:%d\n"
                "          indexing a non-array with %d dims, var defined "
                "at %s%s:%d\n",
            parser->errCount + 1, expr->var->name, RELF(parser->filename),
            expr->line, expr->col, ASTExpr_countCommaList(expr->left),
            RELF(parser->filename), expr->var->typeSpec->line);
    else
        eprintf("(%d) \e[31merror:\e[0m index dims mismatch for "
                "\e[34m%s\e[0m at %s%s:%d:%d\n"
                "          have %d indexes, need %d, var defined at %s%s:%d\n",
            parser->errCount + 1, expr->var->name, RELF(parser->filename),
            expr->line, expr->col, ASTExpr_countCommaList(expr->left), reqdDims,
            RELF(parser->filename), expr->var->typeSpec->line);
    Parser_errorIncrement(parser);
}

static void Parser_errorMissingInit(
    Parser* const parser, const ASTExpr* const expr)
{
    assert(expr->kind == tkVarAssign);
    eprintf("\n(%d) \e[31merror:\e[0m missing initializer for "
            "\e[34m%s\e[0m at %s%s:%d-%d\n",
        parser->errCount + 1, expr->var->name, RELF(parser->filename),
        expr->line - 1, expr->line);
    Parser_errorIncrement(parser);
}

static void Parser_errorUnrecognizedType(
    Parser* const parser, const ASTTypeSpec* const typeSpec)
{
    eprintf("\n(%d) \e[31merror:\e[0m unknown typespec \e[33m%s\e[0m "
            "at %s%s:%d:%d\n",
        parser->errCount + 1, typeSpec->name, RELF(parser->filename),
        typeSpec->line, typeSpec->col);
    Parser_errorIncrement(parser);
}

static void Parser_errorUnrecognizedCtor(
    Parser* const parser, const ASTFunc* const func)
{
    eprintf("\n(%d) \e[31merror:\e[0m unknown type \e[33m%s\e[0m "
            "for constructor at %s%s:%d\n",
        parser->errCount + 1, func->name, RELF(parser->filename), func->line);
    Parser_errorIncrement(parser);
}

static void Parser_errorInvalidTestName(Parser* const parser)
{
    eprintf("\n(%d) \e[31merror:\e[0m invalid test name "
            "\e[33m%.*s\e[0m at %s%s:%d"
            "\n       test names must be strings\n",
        parser->errCount + 1, parser->token.matchlen, parser->token.pos,
        RELF(parser->filename), parser->token.line);
    Parser_errorIncrement(parser);
}

static void Parser_errorTypeMismatchBinOp(
    Parser* const parser, const ASTExpr* const expr)
{
    // if one of the types is "<invalid>", an error has already been
    // reported for it; so don't bother
    const char* const leftTypeName = ASTExpr_typeName(expr->left);
    const char* const rightTypeName = ASTExpr_typeName(expr->right);
    if (*leftTypeName == '<' or *rightTypeName == '<') return;
    eprintf("\n(%d) \e[31merror:\e[0m type mismatch at %s%s:%d:%d\n"
            "             can't apply '\e[34m%s\e[0m' to \e[34m%s\e[0m"
            " and \e[34m%s\e[0m\n",
        parser->errCount + 1, RELF(parser->filename), expr->line, expr->col,
        TokenKind_repr(expr->kind, false), leftTypeName, rightTypeName);
    Parser_errorIncrement(parser);
}

static void Parser_errorInitMismatch(
    Parser* const parser, const ASTExpr* const expr)
{
    // if one of the types is "<invalid>", an error has already been
    // reported for it; so don't bother
    const char* const leftTypeName = ASTTypeSpec_name(expr->var->typeSpec);
    const char* const rightTypeName = ASTExpr_typeName(expr->var->init);
    //    if (*leftTypeName == '<' or *rightTypeName == '<') return;
    eprintf("\n(%d) \e[31merror:\e[0m initializer mismatch at %s%s:%d:%d\n"
            "             can't init \e[34m%s\e[0m with an expression of "
            "type \e[34m%s\e[0m\n"
            "             just remove the type, the linter will take "
            "care of it.\n",
        parser->errCount + 1, RELF(parser->filename), expr->line, expr->col,
        leftTypeName, rightTypeName);
    Parser_errorIncrement(parser);
}

static void Parser_errorInitDimsMismatch(
    Parser* const parser, const ASTExpr* const expr, int dims)
{
    // if one of the types is "<invalid>", an error has already been
    // reported for it; so don't bother
    //    const char* const leftTypeName =
    //    ASTTypeSpec_name(expr->var->typeSpec); const char* const rightTypeName
    //    = ASTExpr_typeName(expr->var->init); if (*leftTypeName == '<' or
    //    *rightTypeName == '<') return;
    eprintf("\n(%d) \e[31merror:\e[0m dimensions mismatch at %s%s:%d:%d\n"
            "             can't init a \e[34m%dD\e[0m array \e[34m%s\e[0m with "
            "a \e[34m%dD\e[0m literal. \n"
            "             just remove the dimension specification, the linter "
            "will take "
            "care of it.\n",
        parser->errCount + 1, RELF(parser->filename), expr->line, expr->col,
        expr->var->typeSpec->dims, expr->var->name, dims);
    Parser_errorIncrement(parser);
}

static void Parser_errorReadOnlyVar(
    Parser* const parser, const ASTExpr* const expr)
{
    eprintf("\n(%d) \e[31merror:\e[0m mutating read-only variable '"
            "\e[34m%s\e[0m' at %s%s:%d:%d\n",
        parser->errCount + 1, expr->var->name, RELF(parser->filename),
        expr->line, expr->col);
    Parser_errorIncrement(parser);
}

static void Parser_errorInvalidTypeForOp(
    Parser* const parser, const ASTExpr* const expr)
{
    eprintf("\n(%d) \e[31merror:\e[0m invalid types for operator '"
            "\e[34m%s\e[0m' at %s%s:%d:%d\n",
        parser->errCount + 1, TokenKind_repr(expr->kind, false),
        RELF(parser->filename), expr->line, expr->col);
    Parser_errorIncrement(parser);
}

static void Parser_errorArgTypeMismatch(
    Parser* const parser, const ASTExpr* const expr, const ASTVar* const var)
{
    eprintf("\n(%d) \e[31merror:\e[0m type mismatch for argument '"
            "\e[34m%s\e[0m' at %s%s:%d:%d\n"
            "    need %s (%d), got %s (%d)\n",
        parser->errCount + 1, var->name, RELF(parser->filename), expr->line,
        expr->col, ASTTypeSpec_name(var->typeSpec), var->typeSpec->typeType,
        ASTExpr_typeName(expr), expr->typeType);
    Parser_errorIncrement(parser);
}

static void Parser_errorUnexpectedToken(Parser* const parser)
{
    eprintf("\n(%d) \e[31merror:\e[0m at %s%s:%d:%d\n      unexpected "
            "token '%.*s'\n",
        parser->errCount + 1, RELF(parser->filename), parser->token.line,
        parser->token.col, parser->token.matchlen, parser->token.pos);
    Parser_errorIncrement(parser);
}

static void Parser_errorUnexpectedExpr(
    Parser* const parser, const ASTExpr* const expr)
{
    eprintf("\n(%d) \e[31merror:\e[0m at %s%s:%d:%d\n"
            "      unexpected expr '%s' (%s)\n",
        parser->errCount + 1, RELF(parser->filename), expr->line, expr->col,
        expr->prec ? TokenKind_repr(expr->kind, false) : expr->string,
        TokenKind_str[expr->kind] + 2);
    Parser_errorIncrement(parser);
}
