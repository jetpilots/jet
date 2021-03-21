
static const uint8_t TokenKindTable[256] = { tkNullChar, tkUnknown, tkUnknown,
    tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown,
    tkNewline, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown,
    tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown,
    tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown,
    tkUnknown, tkSpaces, tkExclamation, tkStringBoundary, tkHash, tkDollar,
    tkOpMod, tkAmpersand, tkRawStringBoundary, tkParenOpen, tkParenClose,
    /* 42 * */ tkTimes, tkPlus, tkOpComma, tkMinus, tkPeriod, tkSlash, tkDigit,
    tkDigit, tkDigit, tkDigit, tkDigit, tkDigit, tkDigit, tkDigit, tkDigit,
    tkDigit, tkOpColon, tkOpSemiColon, tkOpLT, tkOpAssign, tkOpGT, tkQuestion,
    tkAt, tkAlphabet, tkAlphabet, tkAlphabet, tkAlphabet, tkAlphabet,
    tkAlphabet, tkAlphabet, tkAlphabet, tkAlphabet, tkAlphabet, tkAlphabet,
    tkAlphabet, tkAlphabet, tkAlphabet, tkAlphabet, tkAlphabet, tkAlphabet,
    tkAlphabet, tkAlphabet, tkAlphabet, tkAlphabet, tkAlphabet, tkAlphabet,
    tkAlphabet, tkAlphabet, tkAlphabet, tkArrayOpen, tkBackslash, tkArrayClose,
    tkPower, tkUnderscore, tkRegexpBoundary, tkAlphabet, tkAlphabet, tkAlphabet,
    tkAlphabet, tkAlphabet, tkAlphabet, tkAlphabet, tkAlphabet, tkAlphabet,
    tkAlphabet, tkAlphabet, tkAlphabet, tkAlphabet, tkAlphabet, tkAlphabet,
    tkAlphabet, tkAlphabet, tkAlphabet, tkAlphabet, tkAlphabet, tkAlphabet,
    tkAlphabet, tkAlphabet, tkAlphabet, tkAlphabet, tkAlphabet, tkBraceOpen,
    tkPipe, tkBraceClose, tkTilde, tkUnknown, tkUnknown, tkUnknown, tkUnknown,
    tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown,
    tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown,
    tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown,
    tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown,
    tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown,
    tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown,
    tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown,
    tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown,
    tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown,
    tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown,
    tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown,
    tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown,
    tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown,
    tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown,
    tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown,
    tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown,
    tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown,
    tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown, tkUnknown };

typedef struct Token {

    char* pos;
    uint32_t matchlen : 24;
    struct {
        bool skipWhiteSpace : 1, mergeArrayDims : 1, noKeywosrdDetect : 1,
            strictSpacing : 1;
    };
    uint16_t line;
    uint8_t col;
    TokenKind kind : 8;
} Token;

static char Token_peekCharAfter(Token* token) {
    char* s = token->pos + token->matchlen;
    if (token->skipWhiteSpace)
        while (*s == ' ') s++;
    return *s;
}

#define Token_compareKeywordAlt(tok, actual)                                   \
    if (sizeof(#tok) - 1 == l && !strncasecmp(#tok, s, l)) {                   \
        token->kind = tkKeyword_##actual;                                      \
        return;                                                                \
    }
#define Token_compareKeyword(tok) Token_compareKeywordAlt(tok, tok)

static void Token_tryKeywordMatch(Token* token) {

    if (token->kind != tkIdentifier) return;

    const char* s = token->pos;
    const int l = token->matchlen;

    Token_compareKeyword(and)
    Token_compareKeyword(cheater)
    Token_compareKeyword(for)
    Token_compareKeyword(do)
    Token_compareKeyword(while)
    Token_compareKeyword(if)
    Token_compareKeyword(then)
    Token_compareKeyword(end)
    Token_compareKeyword(enum)
    Token_compareKeyword(match)
    Token_compareKeyword(case)
    Token_compareKeyword(function)
    Token_compareKeywordAlt(func, function)
    Token_compareKeyword(declare)
    Token_compareKeyword(test)
    Token_compareKeyword(and)
    Token_compareKeyword(yes)
    Token_compareKeyword(no)
    Token_compareKeyword(nil)
    Token_compareKeyword(or)
    Token_compareKeyword(in)

    Token_compareKeyword(type)
    Token_compareKeyword(check)
    Token_compareKeyword(extends)
    Token_compareKeyword(var)
    Token_compareKeyword(let)
    Token_compareKeyword(import)
    Token_compareKeyword(return)
    Token_compareKeyword(result)
    Token_compareKeyword(as)

    if (! strncasecmp("else if ", s, 8))
    {
        token->kind = tkKeyword_elif;
        token->matchlen = 7;
        return;
    }
    if (!strncasecmp("not in ", s, 7)) {
        token->kind = tkKeyword_notin;
        token->matchlen = 6;
        return;
    }
    Token_compareKeyword(else) Token_compareKeyword(not )
}

static TokenKind Token_getType(Token* token, const size_t offset) {
    const char c = token->pos[offset];
    const char cn = c ? token->pos[1 + offset] : 0;
    TokenKind ret = (TokenKind)TokenKindTable[(unsigned char)c];
    switch (c) {
    case '<':
        switch (cn) {
        case '=':
            return tkOpLE;
        default:
            return tkOpLT;
        }
    case '>':
        switch (cn) {
        case '=':
            return tkOpGE;
        default:
            return tkOpGT;
        }
    case '=':
        switch (cn) {
        case '=':
            return tkOpEQ;
        case '>':
            return tkOpResults;
        default:
            return tkOpAssign;
        }
    case '+':
        switch (cn) {
        case '=':
            return tkPlusEq;
        }
        return tkPlus;
    case '-':
        switch (cn) {
        case '=':
            return tkMinusEq;
        }
        return tkMinus;
    case '*':
        switch (cn) {
        case '=':
            return tkTimesEq;
        }
        return tkTimes;
    case '/':
        switch (cn) {
        case '=':
            return tkSlashEq;
        }
        return tkSlash;
    case '^':
        switch (cn) {
        case '=':
            return tkPowerEq;
        }
        return tkPower;
    case '%':
        switch (cn) {
        case '=':
            return tkOpModEq;
        }
        return tkOpMod;
    case '!':
        switch (cn) {
        case '=':
            return tkOpNE;
        }
        return tkExclamation;
    case ':':
        switch (cn) {
        case '=':
            return tkColEq;
        default:
            return tkOpColon;
        }
    default:
        return ret;
    }
}

static bool Token_isUnaryAfter(TokenKind tk) {
    switch (tk) {
    case tkParenClose:
    case tkIdentifier:
    case tkNumber:
    case tkArrayClose:
    case tkArrayDims:
    case tkMultiDotNumber:
        return true;
    default:;
    }
    return false;
}

static void Token_detect(Token* token) {
    TokenKind tt = Token_getType(token, 0);
    TokenKind tt_ret = tkUnknown;
    static TokenKind tt_last = tkUnknown;

    static TokenKind tt_lastNonSpace = tkUnknown;

    TokenKind tmp;
    char* start = token->pos;
    bool found_e = false, found_dot = false;

    switch (tt) {
    case tkStringBoundary:
    case tkRegexpBoundary:
    case tkRawStringBoundary:
        tmp = tt;

        while (tt != tkNullChar) {

            token->pos++;
            tt = Token_getType(token, 0);
            if (tt == tkNullChar || tt == tmp) {
                *token->pos = 0;
                token->pos++;
                break;
            }
            if (tt == tkBackslash && Token_getType(token, 1) == tmp)
                token->pos++;
            if (tt == tkNewline) {
                token->line++;
                token->col = 0;
            }
        }
        switch (tmp) {
        case tkStringBoundary:
            tt_ret = tkString;
            break;
        case tkRegexpBoundary:
            tt_ret = tkRegexp;
            break;
        case tkRawStringBoundary:
            tt_ret = tkRawString;
            break;
        default:
            tt_ret = tkUnknown;
            printf("unreachable %s:%d\n", __FILE__, __LINE__);
        }
        break;

    case tkSpaces:
        if (tt_last == tkOneSpace)

            while (tt != tkNullChar) {

                tt = Token_getType(token, 1);
                token->pos++;
                if (tt != tkSpaces) break;
            }
        else
            token->pos++;

        tt_ret = tkSpaces;
        break;

    case tkColEq:
        token->pos++;

    case tkOpComma:
    case tkOpSemiColon:
    case tkOpAssign:

        tt_ret = tt;

        while (tt != tkNullChar) {
            tt = Token_getType(token, 1);
            token->pos++;
            if (tt == tkHash) {
                while (*token->pos != '\n' && *token->pos != '\0') token->pos++;
                tt = Token_getType(token, 0);
            }
            if (tt == tkNewline) {
                token->line++;
                token->col = 0;
            }
            if (tt != tkSpaces && tt != tkNewline && tt != tkHash) break;
        }
        break;

    case tkArrayOpen:
    case tkBraceOpen:

        while (token->pos && *token->pos) {
            token->pos++;
            if (*token->pos == '#')
                while (*token->pos && *token->pos != '\n') token->pos++;

            if (*token->pos == '\n') {
                token->line++;
                token->col = 0;
            }
            if (*token->pos != ' ' && *token->pos != '\n'
                && *token->pos != '#') {
                token->pos--;
                break;
            }
        }

        if (!token->mergeArrayDims) goto defaultToken;

        while (tt != tkNullChar) {
            tt = Token_getType(token, 1);
            token->pos++;
            if (tt != tkOpColon && tt != tkOpComma) break;
        }
        tt = Token_getType(token, 0);
        if (tt != tkArrayClose) {
            unreachable(
                "expected a ']', found a '%c'. now what?\n", *token->pos);
        }
        token->pos++;
        tt_ret = tkArrayDims;
        break;

    case tkAlphabet:
    case tkUnderscore:
        while (tt != tkNullChar) {
            tt = Token_getType(token, 1);
            token->pos++;
            if (tt != tkAlphabet && tt != tkDigit && tt != tkUnderscore) break;
        }
        tt_ret = tkIdentifier;
        break;

    case tkTilde:
        while (tt != tkNullChar) {
            tt = Token_getType(token, 1);
            token->pos++;
            if (tt == tkNewline) break;
        }
        tt_ret = tkLineComment;
        break;

    case tkPipe:
        while (tt != tkNullChar) {
            tt = Token_getType(token, 1);
            token->pos++;
            if (tt != tkAlphabet && tt != tkDigit && tt != tkSlash
                && tt != tkPeriod)
                break;
        }
        tt_ret = tkUnits;
        break;

    case tkDigit:
        tt_ret = tkNumber;

        while (tt != tkNullChar) {
            tt = Token_getType(token, 1);

            token->pos++;

            if (*token->pos == 'e' || *token->pos == 'E' || *token->pos == 'd'
                || *token->pos == 'D') {
                found_e = true;
                continue;
            }
            if (found_e) {
                found_e = false;
                continue;
            }
            if (tt == tkPeriod) {
                found_dot = true;
                continue;
            }
            if (found_dot && tt == tkPeriod) tt_ret = tkMultiDotNumber;

            if (tt != tkDigit && tt != tkPeriod && *token->pos != 'i') break;
        }
        break;

    case tkMinus:
        tt_ret = Token_isUnaryAfter(tt_lastNonSpace) ? tkUnaryMinus : tt;

        token->pos++;
        break;

    case tkOpNotResults:

        token->pos++;
    case tkOpEQ:
    case tkOpGE:
    case tkOpLE:
    case tkOpNE:
    case tkOpResults:
    case tkBackslash:
    case tkPlusEq:
    case tkMinusEq:
    case tkTimesEq:
    case tkSlashEq:
    case tkPowerEq:
    case tkOpModEq:

        token->pos++;
    default:
    defaultToken:
        tt_ret = tt;
        token->pos++;
        break;
    }

    token->matchlen = (uint32_t)(token->pos - start);
    token->pos = start;
    token->kind = tt_ret;

    if (token->kind == tkIdentifier) Token_tryKeywordMatch(token);

    if (token->kind == tkSpaces && token->matchlen == 1)
        token->kind = tkOneSpace;

    tt_last = token->kind;
    if (tt_last != tkOneSpace && tt_last != tkSpaces) tt_lastNonSpace = tt_last;
}

static void Token_advance(Token* token) {
    switch (token->kind) {
    case tkNullChar:
        unreachable("Advancing token at end of file!", "");
        return;
    case tkIdentifier:
    case tkString:
    case tkNumber:
    case tkMultiDotNumber:
    case tkFunctionCall:
    case tkSubscript:
    case tkDigit:
    case tkAlphabet:
    case tkRawString:
    case tkRegexp:
    case tkUnits:
    case tkKeyword_cheater:
    case tkKeyword_for:
    case tkKeyword_while:
    case tkKeyword_if:
    case tkKeyword_end:
    case tkKeyword_function:
    case tkKeyword_test:
    case tkKeyword_not:
    case tkKeyword_and:
    case tkKeyword_or:
    case tkKeyword_in:
    case tkKeyword_do:
    case tkKeyword_then:
    case tkKeyword_as:
    case tkKeyword_else:
    case tkKeyword_type:
    case tkKeyword_return:
    case tkKeyword_extends:
    case tkKeyword_var:
    case tkKeyword_let:
    case tkKeyword_import:
    case tkUnknown:

        break;
    default:
        *token->pos = 0;
    }

    token->pos += token->matchlen;
    token->col += token->matchlen;
    token->matchlen = 0;
    Token_detect(token);

    if (token->kind == tkNewline) {

        token->line++;
        token->col = 0;
    }
    if (token->skipWhiteSpace
        && (token->kind == tkSpaces
            || (token->strictSpacing && token->kind == tkOneSpace)))
        Token_advance(token);
}
