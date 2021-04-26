#define RELF(s) (*s == '/' ? "" : "./"), s

#define fatal(str, ...)                                                        \
    {                                                                          \
        eprintf(str, __VA_ARGS__);                                             \
        exit(1);                                                               \
    }

#define noPoison 0
static void Parser__errHeaderWithLoc(
    Parser* parser, int line, int col, int len) {
    eprintf("%s%s:%d:%d-%d: error: #%d;;", RELF(parser->filename), line, col,
        col + len, parser->issues.errCount + 1);
}
static void Parser__errHeader(Parser* parser) {
    Parser__errHeaderWithLoc(parser, parser->token.line, parser->token.col,
        parser->token.col + parser->token.matchlen);
}
static void Parser__warnHeaderWithLoc(
    Parser* parser, int line, int col, int len) {
    eprintf("%s%s:%d:%d-%d: warning: #%d;;", RELF(parser->filename), line, col,
        col + len, ++parser->issues.warnCount);
}

static void Parser__errHeaderWithExpr(Parser* parser, JetExpr* expr) {
    int len;
    int col = expr->col;
    switch (expr->kind) {
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
    case tkKeyword_import: len = strlen(TokenKind_repr[expr->kind]); break;
    case tkIdentifier:
    case tkArgumentLabel:
    case tkFunctionCall:
    case tkSubscript:
    case tkObjectInit:
    case tkNumber:
    case tkString: len = strlen(expr->string); break;
    case tkIdentifierResolved:
    case tkSubscriptResolved: len = strlen(expr->var->name); break;
    case tkFunctionCallResolved: len = strlen(expr->func->name); break;
    case tkVarAssign:
        len = 4 + strlen(expr->var->name);
        col -= 3;
        break;
    default: len = 1;
    }
    Parser__errHeaderWithLoc(
        parser, expr->line ? expr->line : parser->token.line, col, len);
}
static void Parser_errorIncrement(Parser* parser) {
    if (++parser->issues.errCount < parser->issues.errLimit) return;
    if (parser->mode == PMLint) {
        fatal(
            "\n*** too many errors (%d), quitting\n", parser->issues.errLimit);
    } else {
        fatal("\n*** %s has errors, please lint it first.\n", parser->filename);
    }
}
static const char* const carets
    = "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
      "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^";
#define _PRLINE(line)                                                          \
    if (line > 0 && line <= parser->orig.used)                                 \
        eprintf("%4d | %s\n", line + offs, parser->orig.ref[line + offs - 1]);
void _PRREDLINE(
    Parser* parser, int line, int col, int len, int offs, char* msg) {
    char* c = parser->orig.ref[line + offs - 1];
    if (!col) len = strlen(c), col = 1;
    eprintf("%4d | %.*s", line + offs, col - 1, c);
    eprintf("%.*s", len, c + col - 1);
    eprintf("%s\n", c + col + len - 1);
    eprintf("       %.*s%.*s %s\n", col - 1, spaces, len, carets, msg);
}

void Parser__printSourceLinesWithOffset(
    Parser* parser, int line, int col, int len, int offs, char* msg) {
    _PRLINE(line - 2)
    _PRLINE(line - 1)
    _PRREDLINE(parser, line, col, len, offs, msg);
    _PRLINE(line + 1)
    _PRLINE(line + 2)

    eputs("\n\n");
}

void Parser__printSourceLines(
    Parser* parser, int line, int col, int len, char* msg) {
    Parser__printSourceLinesWithOffset(parser, line, col, len, 0, msg);
}

static void Parser_errorExpectedToken(Parser* parser, TokenKind expected) {
    Parser__errHeader(parser);

    eprintf("expected '%s' (%s) but found '%s'\n", TokenKind_repr[expected],
        TokenKind_names[expected] + 2, TokenKind_repr[parser->token.kind]);

    char msg[128];
    msg[127] = 0;
    snprintf(msg, 127, "expected '%s' here", TokenKind_repr[expected]);

    Parser__printSourceLines(parser, parser->token.line, parser->token.col,
        parser->token.matchlen, msg);

    // when you have an unexpected token on one line, the rest are also
    // going to be unexpected. so skip to the next newline.
    while (!Parser_matches(parser, tkNewline)
        && !Parser_matches(parser, tkNullChar))
        Token_advance(&parser->token);

    parser->issues.hasParseErrors = 1;
    Parser_errorIncrement(parser);
}

bool isKeywordKind(TokenKind kind) {
    switch (kind) {
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
    case tkKeyword_import: return true;
    default:;
    }
    return false;
}

static void Parser_errorParsingExpr(Parser* parser, JetExpr* expr, char* what) {

    Parser__errHeaderWithExpr(parser, expr);
    eprintf(
        "invalid syntax at '%s' (%s);;", TokenKind_repr[expr->kind], what); //,

    if (isKeywordKind(expr->kind))
        eputs("info: keywords cannot be used as identifiers;;");

    parser->issues.hasParseErrors = 1;
    Parser_errorIncrement(parser);
}

static void Parser_errorInvalidIdent(Parser* parser) {
    Parser__errHeader(parser);
    eprintf("invalid name '%.*s'\n", parser->token.matchlen, parser->token.pos);
    Parser_errorIncrement(parser);
}

static void Parser_errorInvalidTypeMember(Parser* parser) {
    Parser__errHeader(parser);
    eputs("invalid member\n");
    Parser_errorIncrement(parser);
}

static void Parser_errorUnrecognizedVar(Parser* parser, JetExpr* expr) {
    Parser__errHeaderWithExpr(parser, expr);
    eprintf("unknown variable '%.*s'\n", 64, expr->string);

    Parser__printSourceLines(parser, expr->line, expr->col,
        strlen(expr->string), "unknown variable");

    Parser_errorIncrement(parser);
}

static void Parser_errorUnrecognizedMember(
    Parser* parser, JetType* type, JetExpr* expr) {
    Parser__errHeaderWithExpr(parser, expr);
    eprintf("%s '%s' has no member '%s';;", type->isEnum ? "enum" : "type",
        type->name, expr->string);

    if (type->body) {
        foreach (JetVar*, var, type->body->locals) {
            unsigned l1 = strlen(var->name);
            unsigned l2 = strlen(expr->string);

            if (leven(var->name, expr->string, l1, l2) <= 3)
                eprintf("did you mean: '%s'?;;", var->name);
        }
        eputs("members: ");
        foreachn(JetVar*, var, vi, type->body->locals) if (var->name
            && *var->name) eprintf("%s%s", var->name, vi->next ? ", " : "");
        eputs("\n");
    }
    Parser_errorIncrement(parser);
}

static void Parser_warnUnusedArg(Parser* parser, JetVar* var) {
    if (!parser->issues.warnUnusedArg) return;
    Parser__warnHeaderWithLoc(parser, var->line, var->col, strlen(var->name));
    eprintf("unused or unnecessary argument '%s'\n", var->name);
}

static void Parser_warnUnusedVar(Parser* parser, JetVar* var) {
    if (!parser->issues.warnUnusedVar) return;
    Parser__warnHeaderWithLoc(
        parser, var->line, var->col - 4, 4 + strlen(var->name));
    eprintf("unused or unnecessary variable '%s'\n", var->name);
}

static void Parser_warnUnusedFunc(Parser* parser, JetFunc* func) {
    if (!parser->issues.warnUnusedFunc) return;
    Parser__warnHeaderWithLoc(
        parser, func->line, func->col - 5, 5 + strlen(func->name));
    eprintf("unused or unnecessary function '%s';;selector: '%s'\n", func->name,
        func->selector);
}

static void Parser_warnUnusedType(Parser* parser, JetType* type) {
    if (!parser->issues.warnUnusedType) return;
    Parser__warnHeaderWithLoc(
        parser, type->line, type->col, strlen(type->name));
    eprintf("unused type '%s'\n", type->name);
}

static void Parser_warnSameExpr(Parser* parser, JetExpr* e1, JetExpr* e2) {
    Parser__warnHeaderWithLoc(parser, e1->line, e1->col, 1);
    eprintf("CSE candidate '%s' for %d:%d\n", TokenKind_repr[e1->kind],
        e2->line, e2->col);

    Parser__warnHeaderWithLoc(parser, e2->line, e2->col, 1);
    eprintf("CSE candidate '%s' for %d:%d\n", TokenKind_repr[e2->kind],
        e1->line, e2->col);
}

static void Parser_errorDuplicateVar(
    Parser* parser, JetVar* var, int line, int col) {
    Parser__errHeaderWithLoc(parser, var->line, var->col, strlen(var->name));
    eprintf("duplicate variable '%s' already declared at %s%s:%d:%d\n",
        var->name, RELF(parser->filename), line, col);
    Parser_errorIncrement(parser);
}

static void Parser_errorDuplicateType(
    Parser* parser, JetType* type, JetType* orig) {
    Parser__errHeaderWithLoc(parser, type->line, type->col, strlen(type->name));
    if (orig)
        eprintf("duplicate type '%s' already declared at %s%s:%d:%d\n",
            type->name, RELF(parser->filename), orig->line, orig->col);
    else
        eprintf(
            "invalid type name '%s' refers to a built-in type\n", type->name);
    Parser_errorIncrement(parser);
}

static void Parser_errorDuplicateEnum(
    Parser* parser, JetType* en, JetType* orig) {
    Parser__errHeaderWithLoc(parser, en->line, en->col, strlen(en->name));
    if (orig)
        eprintf("duplicate enum '%s' already declared at %s%s:%d:%d\n",
            en->name, RELF(parser->filename), orig->line, orig->col);
    else
        eprintf("invalid enum name '%s' refers to a built-in type\n", en->name);
    Parser_errorIncrement(parser);
}

static void Parser_errorTypeInheritsSelf(Parser* parser, JetType* type) {
    Parser__errHeader(parser);
    eprintf("type inherits from self %s at %s%s:%d:%d\n", type->name,
        RELF(parser->filename), type->line, type->col);
    Parser_errorIncrement(parser);
}

static void Parser_errorCtorHasType(
    Parser* parser, JetFunc* func, JetType* orig) {
    Parser__errHeader(parser);
    eprintf("constructor needs no return "
            "type: %s at %s%s:%d:%d\n"
            "             type declared at %s%s:%d:%d\n"
            "             remove the return type specification\n",
        func->name, RELF(parser->filename), func->line, 1,
        RELF(parser->filename), orig->line, orig->col);
    Parser_errorIncrement(parser);
}

static void Parser_warnCtorCase(Parser* parser, JetFunc* func) {
    JetType* orig = func->spec->type;
    Parser__errHeader(parser);
    eprintf("\n(%d) warning: wrong case "
            "%s for constructor at %s%s:%d:%d\n"
            "             type declared at %s%s:%d:%d\n"
            "             change it to %s or lint the "
            "file\n",
        ++parser->issues.warnCount, func->name, RELF(parser->filename),
        func->line, 1, RELF(parser->filename), orig->line, orig->col,
        orig->name);
}

static void Parser_errorDuplicateFunc(
    Parser* parser, JetFunc* func, JetFunc* orig) {
    Parser__errHeaderWithLoc(parser, func->line, 5, strlen(func->name));
    eprintf("duplicate function "
            "'%s' already declared at %s%s:%d:%d with selector %s\n",
        func->name, RELF(parser->filename), orig->line, 5, func->selector);
    Parser_errorIncrement(parser);
}

static void Parser_errorDuplicateTest(
    Parser* parser, JetTest* test, JetTest* orig) {
    Parser__errHeaderWithLoc(parser, test->line, 5, strlen(test->name));
    eprintf("duplicate test \"%s\" already declared at %s%s:%d:%d\n",
        test->name, RELF(parser->filename), orig->line, 1);
    Parser_errorIncrement(parser);
}

static void Parser_errorUnrecognizedFunc(
    Parser* parser, JetExpr* expr, char* selector) {
    if (noPoison && *selector == '<') return; // invalid type; already error'd
    Parser__errHeaderWithExpr(parser, expr);

    eprintf("cannot resolve call to '%s'\n", expr->string);

    Parser__printSourceLines(parser, expr->line, expr->col,
        strlen(expr->string), "unknown function");

    eprintf("info: no function with selector '%s'\n", selector);

    Parser_errorIncrement(parser);
}

static void Parser_errorStringInterp(Parser* parser, JetExpr* expr, char* pos) {
    Parser__errHeader(parser);
    eprintf("\n ERROR                                    "
            "           "
            "                       \n %s%s:%d:%d:\n There is "
            "a syntax "
            "error within"
            " this string:\n"
            "     %s\"\n"
            " $ is used to specify a variable whose value is to be "
            "interpolated into the\n"
            " string. If you want a literal $ sign in the string, "
            "write it as "
            "\\$.\n",
        RELF(parser->filename), expr->line,
        expr->col + (int)(pos - expr->string), expr->string);
    Parser_errorIncrement(parser);
}

static void Parser_errorCallingFuncWithVoid(
    Parser* parser, JetExpr* expr, JetExpr* arg) {
    Parser__errHeader(parser);
    eprintf("\n ERROR                                    "
            "           "
            "                       \n %s%s:%d:%d:\n The "
            "%s function does not return a value.\n"
            " You cannot use it as an argument in the call to "
            "%s.\n",
        RELF(parser->filename), expr->line, expr->col, arg->func->name,
        expr->string);
    Parser_errorIncrement(parser);
}

static void Parser_errorInheritanceCycle(Parser* parser, JetType* type) {
    Parser__errHeader(parser);
    eprintf("\n ERROR                                    "
            "           "
            "                       \n %s%s:%d:%d:\n Type "
            "%s has a cycle in its inheritance graph.",
        RELF(parser->filename), type->line, type->col, type->name);
    JetType* super = type->super->type;
    eputs("");
    do {
        eprintf("\n extends %s (defined at %s%s:%d:%d)", super->name,
            RELF(parser->filename), super->line, super->col);
        if (super == super->super->type || super == type) {
            if (type != super) eprintf("\n extends %s", super->name);
            break;
        }
        super = super->super->type;
    } while (1);
    eputs("\n ...\n");
    Parser_errorIncrement(parser);
}

static void Parser_errorConstructorHasCycle(Parser* parser, JetType* type) {
    Parser__errHeader(parser);
    eprintf("\n ERROR                                    "
            "           "
            "                       \n %s%s:%d:%d:\n Type "
            "%s has an endless cycle in its "
            "initialization.\n",
        RELF(parser->filename), type->line, type->col, type->name);
    Parser_errorIncrement(parser);
}

static void Parser_errorArgsCountMismatch(Parser* parser, JetExpr* expr) {
    assert(expr->kind == tkFunctionCallResolved);
    Parser__errHeader(parser);
    eprintf("arg count mismatch for "
            "%s at %s%s:%d:%d\n"
            "          have %d args, need %d, func defined at "
            "%s%s:%d\n",
        expr->func->name, RELF(parser->filename), expr->line, expr->col,
        JetExpr_countCommaList(expr->left), expr->func->argCount,
        RELF(parser->filename), expr->func->line);
    Parser_errorIncrement(parser);
}

static void Parser_errorIndexDimsMismatch(
    Parser* parser, JetExpr* expr, int nhave) {
    assert(expr->kind == tkSubscriptResolved);
    if (expr->var->spec->typeType == TYErrorType
        || expr->var->spec->typeType == TYUnresolved)
        return;
    // ^ type resolution failed (and must have raised error) so
    // don't process further
    int reqdDims = expr->var->spec->dims;
    Parser__errHeaderWithExpr(parser, expr);
    if (!reqdDims)
        eprintf("can't index a scalar '%s' with %d dims (defined at %s%s:%d)\n",
            expr->var->name, nhave, RELF(parser->filename),
            expr->var->spec->line);
    else {
        eprintf("dims mismatch for '%s': have %d indexes, need %d (defined "
                "at %s%s:%d)\n",
            expr->var->name, nhave, reqdDims, RELF(parser->filename),
            expr->var->spec->line);
    }
    Parser_errorIncrement(parser);
}

static void Parser_errorMissingInit(Parser* parser, JetExpr* expr) {
    assert(expr->kind == tkVarAssign);
    Parser__errHeaderWithExpr(parser, expr);
    eprintf("missing initializer for "
            "%s\n",
        expr->var->name);
    parser->issues.hasParseErrors = 1;
    Parser_errorIncrement(parser);
}

static void Parser_errorUnrecognizedType(Parser* parser, JetTypeSpec* spec) {
    Parser__errHeaderWithLoc(parser, spec->line, spec->col, strlen(spec->name));
    eprintf("unknown typespec '%s'\n", spec->name);
    Parser_errorIncrement(parser);
}

static void Parser_errorUnrecognizedCtor(Parser* parser, JetFunc* func) {
    Parser__errHeaderWithLoc(parser, func->line, 5, strlen(func->name));
    eprintf("unknown type '%s' for constructor\n", func->name);
    Parser_errorIncrement(parser);
}

static void Parser_errorInvalidTestName(Parser* parser) {
    Parser__errHeader(parser);
    eprintf("invalid test name "
            "'%.*s'; must be a string\n",
        parser->token.matchlen, parser->token.pos);
    Parser_errorIncrement(parser);
}

static void Parser_errorTypeMismatchBinOp(Parser* parser, JetExpr* expr) {
    // if one of the types is "<invalid>", an error has already been
    // reported for it; so don't bother
    const char* leftTypeName = JetExpr_typeName(expr->left);
    const char* rightTypeName = JetExpr_typeName(expr->right);
    if (noPoison && (*leftTypeName == '<' || *rightTypeName == '<')) return;
    Parser__errHeaderWithExpr(parser, expr);
    eprintf("type mismatch; can't apply '%s' to '%s' and '%s'\n",
        TokenKind_repr[expr->kind], leftTypeName, rightTypeName);
    Parser_errorIncrement(parser);
}

static void Parser_errorTypeMismatch(Parser* parser, JetExpr* e1, JetExpr* e2) {
    const char* leftTypeName = JetExpr_typeName(e1);
    const char* rightTypeName = JetExpr_typeName(e2);
    if (noPoison && (*leftTypeName == '<' || *rightTypeName == '<')) return;
    Parser__errHeaderWithExpr(parser, e2);
    eprintf("type mismatch: '%s' here must be '%s' instead (from %s%s:%d:%d)\n",
        leftTypeName, rightTypeName, RELF(parser->filename), e1->line, e1->col);
    Parser_errorIncrement(parser);
}

static void Parser_errorInitMismatch(Parser* parser, JetExpr* expr) {
    const char* leftTypeName = JetTypeSpec_name(expr->var->spec);
    const char* rightTypeName = JetExpr_typeName(expr->var->init);
    //    if (*leftTypeName == '<' or *rightTypeName == '<') return;

    // for collections, RHS is allowed to be an empty [] or {} to
    // indicate that the array starts out empty. Any-dim arrays can
    // be initialized with []. e.g. var arr[:,:,:] as Number = [] of
    // course, the LHS must have a type, you cannot have e.g. var
    // arr[:,:,:] = [] that would be an error.
    if ((expr->var->init->kind == tkArrayOpen
            || expr->var->init->kind == tkBraceOpen)
        && expr->var->spec->collectionType != CTYNone && !expr->var->init->right
        && expr->var->spec->typeType != TYUnresolved)
        return;
    Parser__errHeaderWithExpr(parser, expr);

    eprintf("can't init '%s' with an expression of type '%s' (hint: just "
            "remove the type annotation.)\n",
        leftTypeName, rightTypeName);
    Parser_errorIncrement(parser);
}

static void Parser_errorInitDimsMismatch(
    Parser* parser, JetExpr* expr, int dims) {

    Parser__errHeaderWithExpr(parser, expr);
    eprintf("can't init %dD array '%s' with a %dD literal. "
            "(hint: just remove the dimension specification.)\n",
        expr->var->spec->dims, expr->var->name, dims);
    Parser_errorIncrement(parser);
}

static void Parser_errorBinOpDimsMismatch(Parser* parser, JetExpr* expr) {
    Parser__errHeaderWithExpr(parser, expr);

    eprintf("can't apply '%s' to %dD array and %dD array\n",
        TokenKind_repr[expr->kind], expr->left->dims, expr->right->dims);
    Parser_errorIncrement(parser);
}

static void Parser_errorReadOnlyVar(Parser* parser, JetExpr* expr) {
    Parser__errHeaderWithExpr(parser, expr);
    eprintf("can't mutate read-only variable '%s'\n", expr->var->name);
    Parser_errorIncrement(parser);
}

static void Parser_errorNoEnumInferred(Parser* parser, JetExpr* expr) {
    Parser__errHeaderWithExpr(parser, expr);
    eprintf("could not infer enum type for '.%s'\n", expr->string);
    Parser_errorIncrement(parser);
}

static void Parser_errorInvalidTypeForOp(Parser* parser, JetExpr* expr) {
    if (expr->left->typeType == TYErrorType
        || expr->right->typeType == TYErrorType)
        return;
    Parser__errHeaderWithExpr(parser, expr);

    eprintf("invalid types for '%s'\n", TokenKind_repr[expr->kind]);
    Parser_errorIncrement(parser);
}

static void Parser_errorArgTypeMismatch(
    Parser* parser, JetExpr* expr, JetVar* var) {
    Parser__errHeaderWithExpr(parser, expr);

    eprintf("type '%s' for argument '%s' should be '%s' instead (from "
            "%s%s:%d:%d)\n",
        JetExpr_typeName(expr), var->name, JetTypeSpec_name(var->spec),
        RELF(parser->filename), var->line, var->col);
    parser->issues.hasParseErrors = 1;
    Parser_errorIncrement(parser);
}

static void Parser_errorUnexpectedToken(Parser* parser, char* msg) {
    Parser__errHeader(parser);

    eprintf(
        "unexpected token '%.*s'\n", parser->token.matchlen, parser->token.pos);

    Parser__printSourceLines(parser, parser->token.line, parser->token.col,
        parser->token.matchlen, msg);

    // when you have an unexpected token on one line, the rest are also going to
    // be unexpected. so skip to the next newline.
    while (!Parser_matches(parser, tkNewline)
        && !Parser_matches(parser, tkNullChar))
        Token_advance(&parser->token);

    parser->issues.hasParseErrors = 1;
    Parser_errorIncrement(parser);
}

static void Parser_errorUnexpectedExpr(Parser* parser, JetExpr* expr) {
    Parser__errHeaderWithExpr(parser, expr);
    eprintf("unexpected expr '%s' (%s)\n",
        expr->prec ? TokenKind_repr[expr->kind] : expr->string,
        TokenKind_names[expr->kind] + 2);
    Parser_errorIncrement(parser);
}
