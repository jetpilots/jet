#define RELF(s) (*s == '/' ? "" : "./"), s

#define fatal(str, ...)                                                        \
    {                                                                          \
        eprintf(str, __VA_ARGS__);                                             \
        exit(1);                                                               \
    }

enum ErrorEntity {
    eeType,
    eeFunc,
    eeVar,
    eeTest,
    eeExpr,
    eeBinop,
    eeUnop,
    ee__count
};
enum ErrorKinds { ekMissing, ekUnknown, ekMismatch, ekUnexpected, ek__count };

// probably not a good idea
static const char* ErrorStrings[ek__count][ee__count] = {//
    [ekUnknown] = { //
        [eeeType] = "unknown type '%s'",
        [eeFunc] = "unknown func '%s' (selector '%s')",
        [eeVar] = "unknown var '%s'"//
    },
    [ekMismatch] = {
        [eeBinop] = "type mismatch for binary op '%s': '%s' and '%s' are not compatible"
    }
};

#define noPoison 0
static void _errHeaderWithLoc(Compiler& com, int line, int col, int len) {
    // "%s%s:%d:%d: error: "
    eprintf("%s%s:%d:%d-%d: error: #%d;;",
        RELF(com->filename), //
        line, //
        col, //
        col + len, //
        com->issues.errCount + 1);
}
static void _errHeader(Compiler& com) {
    _errHeaderWithLoc(parser, com->token.line, com->token.col,
        com->token.col + com->token.matchlen);
}

static void _errHeaderWithExpr(Compiler& com, ASTExpr& expr) {
    int len;
    switch (expr->kind) {
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
        len = strlen(expr->string);
        break;
    case tkIdentifierResolved:
    case tkSubscriptResolved:
        len = strlen(expr->var->name);
        break;
    case tkFunctionCallResolved:
        len = strlen(expr->func->name);
        break;
    default:
        len = 1;
    }
    _errHeaderWithLoc(
        parser, expr->line ? expr->line : com->token.line, expr->col, len);
}
static void errorIncrement(Compiler& com) {
    if (++com->issues.errCount < com->issues.errLimit) return;
    if (com->mode == PMLint) {
        fatal("\n*** too many errors (%d), quitting\n", com->issues.errLimit);
    } else {
        fatal("\n*** %s has errors, please lint it first.\n", com->filename);
    }
}
static const char* const carets
    = "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
      "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^";
#define _PRLINE(line)                                                          \
    if (line > 0 && line <= com->orig.used)                                    \
        eprintf("%4d | %s\n", line + offs, com->orig.ref[line + offs - 1]);
void _PRREDLINE(
    Compiler& com, int line, int col, int len, int offs, char* msg) {
    char* c = com->orig.ref[line + offs - 1];
    if (!col) len = strlen(c), col = 1;
    eprintf("%4d | %.*s", line + offs, col - 1, c);
    eprintf("%.*s", len, c + col - 1);
    eprintf("%s\n", c + col + len - 1);
    eprintf("       %.*s%.*s %s\n", col - 1, spaces, len, carets, msg);
}

void _printSourceLinesWithOffset(
    Compiler& com, int line, int col, int len, int offs, char* msg) {
    // eputs("\n"); //-----+\n");
    _PRLINE(line - 2)
    _PRLINE(line - 1)
    _PRREDLINE(parser, line, col, len, offs, msg);
    _PRLINE(line + 1)
    _PRLINE(line + 2)

    eputs("\n\n"); //-----+\n");
}

void _printSourceLines(Compiler& com, int line, int col, int len, char* msg) {
    _printSourceLinesWithOffset(parser, line, col, len, 0, msg);
}

static void errorExpectedToken(Compiler& com, TokenKind expected) {
    _errHeader(parser);

    eprintf("expected '%s' (%s) but found '%s'\n",
        // com->issues.errCount + 1, //
        TokenKind_repr(expected, false), //
        TokenKind_str[expected] + 2, //
        TokenKind_repr(com->token.kind, false) //
        // RELF(com->filename), //
        // com->token.line, //
        // com->token.col //
    );

    char msg[128];
    msg[127] = 0;
    snprintf(msg, 127, "expected '%s' here", TokenKind_repr(expected, false));

    _printSourceLines(
        parser, com->token.line, com->token.col, com->token.matchlen, msg);

    // when you have an unexpected token on one line, the rest are also
    // going to be unexpected. so skip to the next newline.
    while (!matches(parser, tkNewline) && !matches(parser, tkNullChar))
        Token_advance(&com->token);

    // eprintf("at %s%s:%d:%d\n"
    //         "      expected '%s' (%s) but found '%s'\n",
    //     com->issues.errCount + 1, //
    //     RELF(com->filename), //
    //     com->token.line,
    //     com->token.col, //
    //     TokenKind_repr(expected, false), //
    //     TokenKind_str[expected] + 2, //
    //     TokenKind_repr(com->token.kind, false));

    errorIncrement(parser);
}

bool isKeywordKind(TokenKind kind) {
    switch (kind) {
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
        return true;
    default:
    }
    return false;
}

static void errorParsingExpr(Compiler& com, ASTExpr& expr, char* what) {

    // eprintf("syntax error at %s%s:%d/%d\n"
    //         "    probably around %d:%d\n",
    // com->issues.errCount + 1, RELF(com->filename),
    // expr->line ? expr->line : com->token.line - 1, expr->col);

    _errHeaderWithExpr(parser, expr);
    eprintf("invalid syntax (%s);;", what); //,

    //  _printSourceLines(parser,
    //     expr->line ? expr->line : com->token.line - 1, expr->col, len,
    //     "syntax error");

    if (isKeywordKind(expr->kind))
        eputs("info: keywords cannot be used as identifiers;;");

    com->issues.hasParseErrors = 1;
    errorIncrement(parser);
}

static void errorInvalidIdent(Compiler& com) {
    _errHeader(parser);
    eprintf("invalid name '%.*s'\n",
        com->token.matchlen, //
        com->token.pos);
    errorIncrement(parser);
}

static void errorInvalidTypeMember(Compiler& com) {
    _errHeader(parser);
    eputs("invalid member\n");
    errorIncrement(parser);
}

static void errorUnrecognizedVar(Compiler& com, ASTExpr& expr) {
    _errHeaderWithExpr(parser, expr);
    eprintf("unknown variable '%.*s'\n", 64, expr->string);

    _printSourceLines(parser, expr->line, expr->col, strlen(expr->string),
        "unknown variable");

    errorIncrement(parser);
}

static void errorUnrecognizedMember(
    Compiler& com, ASTType& type, ASTExpr& expr) {
    _errHeaderWithExpr(parser, expr);
    eprintf("%s '%s' has no member '%s';;", type->isEnum ? "enum" : "type",
        type->name, expr->string);

    if (type->body) {
        for (ASTVar& var : type->body->locals) {
            unsigned l1 = strlen(var->name);
            unsigned l2 = strlen(expr->string);

            if (leven(var->name, expr->string, l1, l2) <= 3)
                eprintf("did you mean: '%s'?;;", var->name);
        }
        eputs("members: ");
        for (ASTVar& var : vi)
            if (var.name && *var.name)
                eprintf("%s%s", var.name, vi->next ? ", " : "");
        eputs("\n");
    }
    errorIncrement(parser);
}

static void warnUnusedArg(Compiler& com, ASTVar& var) {
    if (!com->issues.warnUnusedArg) return;
    _errHeader(parser);
    eprintf("\n(%d) warning: unused argument "
            "%s at %s%s:%d:%d\n",
        ++com->issues.warnCount, //
        var->name, //
        RELF(com->filename), //
        var->line, //
        var->col);
}

static void warnUnusedVar(Compiler& com, ASTVar& var) {
    if (!com->issues.warnUnusedVar) return;
    _errHeader(parser);
    eprintf("\n(%d) warning: unused variable "
            "%s at %s%s:%d:%d\n",
        ++com->issues.warnCount, //
        var->name, //
        RELF(com->filename), //
        var->line, //
        var->col);
}

static void warnUnusedFunc(Compiler& com, ASTFunc& func) {
    if (!com->issues.warnUnusedFunc) return;
    _errHeader(parser);
    eprintf("\n(%d) warning: unused function "
            "%s at %s%s:%d\n"
            "            selector is %s\n",
        ++com->issues.warnCount, //
        func->name, //
        RELF(com->filename),
        func->line, //
        func->selector);
}

static void warnUnusedType(Compiler& com, ASTType& type) {
    if (!com->issues.warnUnusedType) return;
    _errHeader(parser);
    eprintf("\n(%d) warning: unused type "
            "%s at %s%s:%d:%d\n",
        ++com->issues.warnCount, //
        type->name, //
        RELF(com->filename),
        type->line, //
        type->col);
}

static void errorDuplicateVar(Compiler& com, ASTVar& var, int line, int col) {
    _errHeaderWithLoc(parser, var->line, var->col, strlen(var->name));
    eprintf("duplicate variable '%s' already declared at %s%s:%d:%d\n",
        // com->issues.errCount + 1, //
        var->name, //
        // RELF(com->filename),
        // var->line, //
        // var->col, //
        RELF(com->filename), //
        line, //
        col);
    errorIncrement(parser);
}

static void errorDuplicateType(Compiler& com, ASTType& type, ASTType& orig) {
    _errHeaderWithLoc(parser, type->line, type->col, strlen(type->name));
    if (orig)
        eprintf("duplicate type '%s' already declared at %s%s:%d:%d\n",
            // com->issues.errCount + 1,
            type->name,
            //  RELF(com->filename),
            // type->line, type->col,
            RELF(com->filename), orig->line, orig->col);
    else
        eprintf("invalid type name '%s' refers to a built-in type\n",
            // com->issues.errCount + 1,
            type->name
            //  RELF(com->filename),
            // type->line, type->col
        );
    errorIncrement(parser);
}

static void errorDuplicateEnum(Compiler& com, ASTType& en, ASTType& orig) {
    _errHeaderWithLoc(parser, en->line, en->col, strlen(en->name));
    if (orig)
        eprintf("duplicate enum '%s' already declared at %s%s:%d:%d\n",
            en->name, RELF(com->filename), orig->line, orig->col);
    else
        eprintf("invalid enum name '%s' refers to a built-in type\n", en->name);
    errorIncrement(parser);
}

static void errorTypeInheritsSelf(Compiler& com, ASTType& type) {
    _errHeader(parser);
    eprintf("type inherits from parser "
            "%s at %s%s:%d:%d\n",
        type->name, RELF(com->filename), type->line, type->col);
    errorIncrement(parser);
}

static void errorCtorHasType(Compiler& com, ASTFunc* func, ASTType& orig) {
    _errHeader(parser);
    eprintf("constructor needs no return "
            "type: %s at %s%s:%d:%d\n"
            "             type declared at %s%s:%d:%d\n"
            "             remove the return type specification\n",
        func->name, RELF(com->filename), func->line, 1, RELF(com->filename),
        orig->line, orig->col);
    errorIncrement(parser);
}

static void warnCtorCase(Compiler& com, ASTFunc* func) {
    ASTType& orig = func->returnSpec->type;
    _errHeader(parser);
    eprintf("\n(%d) warning: wrong case "
            "%s for constructor at %s%s:%d:%d\n"
            "             type declared at %s%s:%d:%d\n"
            "             change it to %s or lint the "
            "file\n",
        ++com->issues.warnCount, func->name, RELF(com->filename), func->line, 1,
        RELF(com->filename), orig->line, orig->col, orig->name);
}

static void errorDuplicateFunc(Compiler& com, ASTFunc* func, ASTFunc* orig) {
    _errHeaderWithLoc(parser, func->line, 5, strlen(func->name));
    eprintf("duplicate function "
            "'%s' already declared at %s%s:%d:%d with selector %s\n",
        // com->issues.errCount + 1,
        func->name,
        //  RELF(com->filename), func->line, 1,
        RELF(com->filename), orig->line, 5, func->selector);
    errorIncrement(parser);
}

static void errorDuplicateTest(Compiler& com, ASTTest* test, ASTTest* orig) {
    _errHeaderWithLoc(parser, test->line, 5, strlen(test->name));
    eprintf("duplicate test \"%s\" already declared at %s%s:%d:%d\n",
        // com->issues.errCount + 1,
        test->name,
        //   RELF(com->filename),
        // test->line, 1,
        RELF(com->filename), orig->line, 1);
    errorIncrement(parser);
}

static void errorUnrecognizedFunc(
    Compiler& com, ASTExpr& expr, char* selector) {
    if (noPoison && *selector == '<') return; // invalid type; already error'd
    _errHeaderWithExpr(parser, expr);

    eprintf("cannot resolve call to '%s'\n",
        // com->issues.errCount + 1, //
        expr->string //, //
        // RELF(com->filename), //
        // expr->line, //
        // expr->col //
    );

    _printSourceLines(parser, expr->line, expr->col, strlen(expr->string),
        "unknown function");

    eprintf("info: no function with selector '%s'\n", selector);

    // eprintf("\n ERROR                                    "
    //         "           "
    //         "                       \n %s%s:%d:%d:\n This "
    //         "%s call could not be resolved.\n"
    //         " There is no method with selector %s and %d "
    //         "arguments.\n",
    //     RELF(com->filename), expr->line, expr->col, expr->string,
    //     selector, ASTExpr_countCommaList(expr->left));
    errorIncrement(parser);
}

static void errorStringInterp(Compiler& com, ASTExpr& expr, char* pos) {
    _errHeader(parser);
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
        RELF(com->filename), expr->line, expr->col + (int)(pos - expr->string),
        expr->string);
    errorIncrement(parser);
}

static void errorCallingFuncWithVoid(
    Compiler& com, ASTExpr& expr, ASTExpr& arg) {
    _errHeader(parser);
    eprintf("\n ERROR                                    "
            "           "
            "                       \n %s%s:%d:%d:\n The "
            "%s function does not return a value.\n"
            " You cannot use it as an argument in the call to "
            "%s.\n",
        RELF(com->filename), expr->line, expr->col, arg->func->name,
        expr->string);
    errorIncrement(parser);
}

static void errorInheritanceCycle(Compiler& com, ASTType& type) {
    _errHeader(parser);
    eprintf("\n ERROR                                    "
            "           "
            "                       \n %s%s:%d:%d:\n Type "
            "%s has a cycle in its inheritance graph.",
        RELF(com->filename), type->line, type->col, type->name);
    ASTType& super = type->super->type;
    eputs("");
    do {
        eprintf("\n extends %s (defined at %s%s:%d:%d)", super->name,
            RELF(com->filename), super->line, super->col);
        if (super == super->super->type || super == type) {
            if (type != super) eprintf("\n extends %s", super->name);
            break;
        }
        super = super->super->type;
    } while (1);
    eputs("\n ...\n");
    errorIncrement(parser);
}

static void errorConstructorHasCycle(Compiler& com, ASTType& type) {
    _errHeader(parser);
    eprintf("\n ERROR                                    "
            "           "
            "                       \n %s%s:%d:%d:\n Type "
            "%s has an endless cycle in its "
            "initialization.\n",
        RELF(com->filename), type->line, type->col, type->name);
    errorIncrement(parser);
}

static void errorArgsCountMismatch(Compiler& com, ASTExpr& expr) {
    assert(expr->kind == tkFunctionCallResolved);
    _errHeader(parser);
    eprintf("arg count mismatch for "
            "%s at %s%s:%d:%d\n"
            "          have %d args, need %d, func defined at "
            "%s%s:%d\n",
        // com->issues.errCount + 1,
        expr->func->name, RELF(com->filename), expr->line, expr->col,
        ASTExpr_countCommaList(expr->left), expr->func->argCount,
        RELF(com->filename), expr->func->line);
    errorIncrement(parser);
}

static void errorIndexDimsMismatch(Compiler& com, ASTExpr& expr, int nhave) {
    assert(expr->kind == tkSubscriptResolved);
    if (expr->var->typeSpec->typeType == TYErrorType
        || expr->var->typeSpec->typeType == TYUnresolved)
        return;
    // ^ type resolution failed (and must have raised error) so
    // don't process further
    int reqdDims = expr->var->typeSpec->dims;
    _errHeaderWithExpr(parser, expr);
    if (!reqdDims)
        eprintf("can't index a scalar '%s' with %d dims (defined at %s%s:%d)\n",
            // com->issues.errCount + 1,
            expr->var->name,
            // RELF(com->filename), expr->line, expr->col,
            nhave, RELF(com->filename), expr->var->typeSpec->line);
    else {
        eprintf("dims mismatch for '%s': have %d indexes, need %d (defined "
                "at %s%s:%d)\n",
            // com->issues.errCount + 1,
            expr->var->name,
            // RELF(com->filename), expr->line, expr->col,
            nhave, reqdDims, RELF(com->filename), expr->var->typeSpec->line);
    }
    errorIncrement(parser);
}

static void errorMissingInit(Compiler& com, ASTExpr& expr) {
    assert(expr->kind == tkVarAssign);
    _errHeaderWithExpr(parser, expr);
    eprintf("missing initializer for "
            "%s\n",
        // com->issues.errCount + 1,
        expr->var->name
        //  ,
        //  RELF(com->filename),
        // expr->line - 1, expr->line
    );
    com->issues.hasParseErrors = 1;
    errorIncrement(parser);
}

static void errorUnrecognizedType(Compiler& com, ASTTypeSpec* typeSpec) {
    _errHeaderWithLoc(
        parser, typeSpec->line, typeSpec->col, strlen(typeSpec->name));
    eprintf("unknown typespec '%s'\n",
        // com->issues.errCount + 1,
        typeSpec->name
        //  , RELF(com->filename),
        // typeSpec->line, typeSpec->col
    );
    errorIncrement(parser);
}

static void errorUnrecognizedCtor(Compiler& com, ASTFunc* func) {
    _errHeaderWithLoc(parser, func->line, 5, strlen(func->name));
    eprintf("unknown type '%s' for constructor\n",
        // com->issues.errCount + 1,
        func->name
        // ,
        // RELF(com->filename),
        // func->line
    );
    errorIncrement(parser);
}

static void errorInvalidTestName(Compiler& com) {
    _errHeader(parser);
    eprintf("invalid test name "
            "'%.*s'; must be a string\n",
        // com->issues.errCount + 1,
        com->token.matchlen, com->token.pos
        // ,
        // RELF(com->filename), com->token.line
    );
    errorIncrement(parser);
}

static void errorTypeMismatchBinOp(Compiler& com, ASTExpr& expr) {
    // if one of the types is "<invalid>", an error has already been
    // reported for it; so don't bother
    const char* leftTypeName = ASTExpr_typeName(expr->left);
    const char* rightTypeName = ASTExpr_typeName(expr->right);
    if (noPoison && (*leftTypeName == '<' || *rightTypeName == '<')) return;
    _errHeaderWithExpr(parser, expr);
    eprintf("type mismatch; can't apply '%s' to '%s' and '%s'\n",
        // com->issues.errCount + 1,
        // RELF(com->filename), expr->line, expr->col,
        TokenKind_repr(expr->kind, false), leftTypeName, rightTypeName);
    errorIncrement(parser);
}

static void errorTypeMismatch(Compiler& com, ASTExpr& e1, ASTExpr& e2) {
    // if one of the types is "<invalid>", an error has already been
    // reported for it; so don't bother
    const char* leftTypeName = ASTExpr_typeName(e1);
    const char* rightTypeName = ASTExpr_typeName(e2);
    if (noPoison && (*leftTypeName == '<' || *rightTypeName == '<')) return;
    _errHeaderWithExpr(parser, e2);
    eprintf("type mismatch: '%s' here must be '%s' instead (from %s%s:%d:%d)\n",
        // com->issues.errCount + 1,
        leftTypeName,
        //  RELF(com->filename), e2->line, e2->col,
        rightTypeName, RELF(com->filename), e1->line, e1->col);
    errorIncrement(parser);
}

static void errorInitMismatch(Compiler& com, ASTExpr& expr) {
    // if one of the types is "<invalid>", an error has already been
    // reported for it; so don't bother
    const char* leftTypeName = ASTTypeSpec_name(expr->var->typeSpec);
    const char* rightTypeName = ASTExpr_typeName(expr->var->init);
    //    if (*leftTypeName == '<' or *rightTypeName == '<') return;

    // for collections, RHS is allowed to be an empty [] or {} to
    // indicate that the array starts out empty. Any-dim arrays can
    // be initialized with []. e.g. var arr[:,:,:] as Number = [] of
    // course, the LHS must have a type, you cannot have e.g. var
    // arr[:,:,:] = [] that would be an error.
    if ((expr->var->init->kind == tkArrayOpen
            || expr->var->init->kind == tkBraceOpen)
        && expr->var->typeSpec->collectionType != CTYNone
        && !expr->var->init->right
        && expr->var->typeSpec->typeType != TYUnresolved)
        return;
    _errHeaderWithExpr(parser, expr);

    eprintf("can't init '%s' with an expression of type '%s' (hint: just "
            "remove the type annotation.)\n",
        // com->issues.errCount + 1, //
        // RELF(com->filename), //
        // expr->line,
        // expr->col, //
        leftTypeName, //
        rightTypeName);
    errorIncrement(parser);
}

static void errorInitDimsMismatch(Compiler& com, ASTExpr& expr, int dims) {
    // if one of the types is "<invalid>", an error has already been
    // reported for it; so don't bother
    //    char* leftTypeName =
    //    ASTTypeSpec_name(expr->var->typeSpec); char* rightTypeName
    //    = ASTExpr_typeName(expr->var->init); if (*leftTypeName ==
    //    '<' or *rightTypeName == '<') return;
    _errHeaderWithExpr(parser, expr);
    eprintf("can't init %dD array '%s' with a %dD literal. "
            "(hint: just remove the dimension specification.)\n",
        // com->issues.errCount + 1, //
        // RELF(com->filename), //
        // expr->line,
        // expr->col, //
        expr->var->typeSpec->dims, //
        expr->var->name, dims);
    errorIncrement(parser);
}

static void errorBinOpDimsMismatch(Compiler& com, ASTExpr& expr) {
    _errHeaderWithExpr(parser, expr);

    eprintf("can't apply '%s' to %dD array and %dD array\n",
        // com->issues.errCount + 1, //
        // RELF(com->filename), //
        // expr->line,
        // expr->col, //
        tkrepr[expr->kind], //
        expr->left->dims, //
        expr->right->dims);
    errorIncrement(parser);
}

static void errorReadOnlyVar(Compiler& com, ASTExpr& expr) {
    _errHeaderWithExpr(parser, expr);
    eprintf("can't mutate read-only variable '%s'\n",
        // com->issues.errCount + 1, //
        expr->var->name
        // , //
        // RELF(com->filename), //
        // expr->line, //
        // expr->col
    );
    errorIncrement(parser);
}

static void errorNoEnumInferred(Compiler& com, ASTExpr& expr) {
    _errHeaderWithExpr(parser, expr);
    eprintf("could not infer enum type for '.%s'\n",
        // com->issues.errCount + 1, //
        expr->string
        // , //
        // RELF(com->filename), //
        // expr->line, //
        // expr->col
    );
    errorIncrement(parser);
}

static void errorInvalidTypeForOp(Compiler& com, ASTExpr& expr) {
    if (expr->left->typeType == TYErrorType
        || expr->right->typeType == TYErrorType)
        return;
    _errHeaderWithExpr(parser, expr);

    eprintf("invalid types for '%s'\n",
        // com->issues.errCount + 1, //
        TokenKind_repr(expr->kind, false)
        // , //
        // RELF(com->filename), //
        // expr->line, expr->col
    );
    errorIncrement(parser);
}

static void errorArgTypeMismatch(Compiler& com, ASTExpr& expr, ASTVar& var) {
    _errHeaderWithExpr(parser, expr);

    eprintf("type '%s' for argument '%s' should be '%s' instead (from "
            "%s%s:%d:%d)\n",
        // com->issues.errCount + 1, //
        ASTExpr_typeName(expr), //
        var->name, //
        // var->typeSpec->typeType, //
        ASTTypeSpec_name(var->typeSpec),
        RELF(com->filename), //
        var->line, //
        var->col //, //
        // expr->typeType
    );
    com->issues.hasParseErrors = 1;
    errorIncrement(parser);
}

static void errorUnexpectedToken(Compiler& com, char* msg) {
    _errHeader(parser);

    eprintf( //"\n_________________________________" //
             //"____________________________________ #%d\n" //
        // "%s%s:%d:%d: error: "
        "unexpected token '%.*s'\n",
        // com->issues.errCount + 1, //
        // RELF(com->filename), //
        // com->token.line, //
        // com->token.col, //
        com->token.matchlen, //
        com->token.pos //
    );

    _printSourceLines(
        parser, com->token.line, com->token.col, com->token.matchlen, msg);

    // when you have an unexpected token on one line, the rest are also going to
    // be unexpected. so skip to the next newline.
    while (!matches(parser, tkNewline) && !matches(parser, tkNullChar))
        Token_advance(&com->token);

    errorIncrement(parser);
}

static void errorUnexpectedExpr(Compiler& com, ASTExpr& expr) {
    _errHeaderWithExpr(parser, expr);
    eprintf("unexpected expr '%s' (%s)\n",
        // com->issues.errCount + 1, //
        // RELF(com->filename), //
        // expr->line, //
        // expr->col,
        expr->prec //
            ? TokenKind_repr(expr->kind, false) //
            : expr->string,
        TokenKind_str[expr->kind] + 2);
    errorIncrement(parser);
}
