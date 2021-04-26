#include "../c99/TokenKind.h"

struct SourceLoc {
    unsigned line : 16, col : 8, len : 8;
};
static_assert(sizeof(SourceLoc) == 4, "");

struct Token {
    char *pos, *end;
    TokenKind kind;
    unsigned long line, col, len;
    bool skipWhiteSpace, strictSpacing, mergeArrayDims;
    void format() { return; }
    bool is(TokenKind k) { return kind == k; }
    bool in(TokenKind k1, TokenKind k2) { return kind == k1 or kind == k2; }
    bool in(TokenKind k1, TokenKind k2, TokenKind k3) {
        return kind == k1 or in(k2, k3);
    }
    bool in(TokenKind k1, TokenKind k2, TokenKind k3, TokenKind k4) {
        return kind == k1 or in(k2, k3, k4);
    }
    SourceLoc loc() { return (SourceLoc) { line, col, len }; }
    void skipLine() {
        while (pos < end and not in(tkNewline, tkLineComment, tkNullChar))
            advance();
    }
    int count(char c) {
        int count = 0;
        for (int i = 0; i < len; i++)
            if (pos[i] == ':') count++;
        return count;
    }

    char peek() {
        char* s = pos + len;
        if (skipWhiteSpace)
            while (*s == ' ') s++;
        return *s;
    }

    // TokenKind getType(const size_t offset);
    // void detect();
    // void advance();
    // void tryKeywordMatch();

    Expr* expr() {
        Expr* ret = new Expr(kind, (SourceLoc) { line, col, len });

        if ((ret->prec = getPrecedence(ret->kind))) {
            ret->rassoc = isRightAssociative(ret->kind);
            ret->unary = isUnary(ret->kind);
        }

        exprsAllocHistogram[ret->kind]++;

        switch (ret->kind) {
        // case tkKeyword_cheater:
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
        case tkLineComment: // Comments go in the AST like regular stmts
            ret->string = pos;
            break;
        default:;
        }
        // the '!' will be trampled
        if (ret->kind == tkLineComment) ret->string++;
        // turn all 1.0234[DdE]+01 into 1.0234e+01.
        if (ret->kind == tkNumber) {
            CString_tr_ip(ret->string, 'd', 'e', len);
            CString_tr_ip(ret->string, 'D', 'e', len);
            CString_tr_ip(ret->string, 'E', 'e', len);
        }
        return ret;
    }
    // Peek at the char after the current (complete) token

#define compareKeywordAlt(tok, actual)                                         \
    if (sizeof(#tok) - 1 == l && !strncasecmp(#tok, s, l)) {                   \
        kind = tkKeyword_##actual;                                             \
        return;                                                                \
    }
#define compareKeyword(tok) compareKeywordAlt(tok, tok)

    // #define compareKeyword(tok) compareKeywordWith(tok,tok)

    // Check if an (ident) self->token matches a keyword and return its type
    // accordingly.
    void tryKeywordMatch() {
        // TODO: USE A DICT OR MPH FOR THIS!
        if (kind != tkIdentifier) return;

        const char* s = pos;
        const int l = len;

    compareKeyword(and)
    // compareKeyword(cheater)
    compareKeyword(for)
    compareKeyword(do)
    compareKeyword(while)
    compareKeyword(if)
    compareKeyword(then)
    compareKeyword(end)
    compareKeyword(enum)
    compareKeyword(match)
    compareKeyword(case)
    // compareKeyword(function)
    compareKeywordAlt(func, function)
    compareKeyword(declare)
    compareKeyword(test)
    compareKeyword(and)
    compareKeyword(yes)
    compareKeyword(no)
    compareKeyword(nil)
    compareKeyword(or)
    compareKeyword(in)
    // compareKeyword(elif)
    compareKeyword(type)
    compareKeyword(check)
    compareKeyword(extends)
    compareKeyword(var)
    compareKeyword(let)
    compareKeyword(import)
    compareKeyword(return)
    compareKeyword(result)
    compareKeyword(as)

    if (! strncasecmp("else if ", s, 8))
    {
        kind = tkKeyword_elif;
        len = 7;
        return;
    }
    if (!strncasecmp("not in ", s, 7)) {
        kind = tkKeyword_notin;
        len = 6;
        return;
    }
    compareKeyword(else) compareKeyword(not )
    }

    // Get the token kind based only on the char at the current position
    // (or an offset).
    TokenKind getType(const size_t offset) {
        const char c = pos[offset];
        const char cn = c ? pos[1 + offset] : 0;
        TokenKind ret = (TokenKind)TokenKindTable[(unsigned char)c];
        switch (c) {
        case '<':
            switch (cn) {
            case '=': return tkOpLE;
            default: return tkOpLT;
            }
        case '>':
            switch (cn) {
            case '=': return tkOpGE;
            default: return tkOpGT;
            }
        case '=':
            switch (cn) {
            case '=': return tkOpEQ;
            case '>': return tkOpResults;
            default: return tkOpAssign;
            }
        case '+':
            switch (cn) {
            case '=': return tkOpPlusEq;
            }
            return tkOpPlus;
        case '-':
            switch (cn) {
            case '=': return tkOpMinusEq;
            }
            return tkOpMinus;
        case '*':
            switch (cn) {
            case '=': return tkOpTimesEq;
            }
            return tkOpTimes;
        case '/':
            switch (cn) {
            case '=': return tkOpSlashEq;
            }
            return tkOpSlash;
        case '^':
            switch (cn) {
            case '=': return tkOpPowerEq;
            }
            return tkOpPower;
        case '%':
            switch (cn) {
            case '=': return tkOpModEq;
            }
            return tkOpMod;
        case '!':
            switch (cn) {
            case '=': return tkOpNE;
            }
            return tkExclamation;
        case ':':
            switch (cn) {
            case '=': return tkOpColEq;
            default: return tkOpColon;
            }
        default: return ret;
        }
    }

    static bool isUnaryAfter(TokenKind tk) {
        switch (tk) {
        case tkParenClose:
        case tkIdentifier: // TODO: keywords too?
        case tkNumber:
        case tkArrayClose:
        case tkArrayDims:
        case tkMultiDotNumber: return false;
        default:;
        }
        return true;
    }

    void detect() {
        TokenKind tt = getType(0);
        TokenKind tt_ret = tkUnknown; // = tt;
        static TokenKind tt_last = tkUnknown;
        // the previous self->token that was found
        static TokenKind tt_lastNonSpace = tkUnknown;
        // the last non-space self->token found
        TokenKind tmp;
        char* start = pos;
        bool found_e = false, found_dot = false; //, found_cmt = false;
        //    uint8_t found_spc = 0;

        switch (tt) {
        case tkStringBoundary:
        case tkRegexpBoundary:
        case tkRawStringBoundary:
            tmp = tt; // remember which it is exactly

            // Incrementing pos is a side effect of getTypeAtCurrentPos(...)
            while (tt != tkNullChar) {
                // here we want to consume the ending " so we move next
                // before
                pos++;
                tt = getType(0);
                if (tt == tkNullChar || tt == tmp) {
                    *pos = 0;
                    pos++;
                    break;
                }
                if (tt == tkOpBackslash && getType(1) == tmp) pos++;
                if (tt == tkNewline) {
                    line++;
                    col = 0;
                }
            }
            switch (tmp) {
            case tkStringBoundary: tt_ret = tkString; break;
            case tkRegexpBoundary: tt_ret = tkRegexp; break;
            case tkRawStringBoundary: tt_ret = tkRawString; break;
            default:
                tt_ret = tkUnknown;
                printf("unreachable %s:%d\n", __FILE__, __LINE__);
            }
            break;

        case tkSpaces:
            if (tt_last == tkOneSpace) // if prev char was a space return
                                       // this as a run of spaces
                while (tt != tkNullChar) {
                    // here we dont want to consume the end char, so break
                    // before
                    tt = getType(1);
                    pos++;
                    if (tt != tkSpaces) break;
                }
            else
                pos++;
            // else its a single space
            tt_ret = tkSpaces;
            break;

        case tkOpColEq:
            pos++; // 2-char token
            // fallthrough, since tkColEq is also a line continuation token
            // like , and ;
        case tkOpComma:
        case tkOpSemiColon:
        case tkOpAssign:
            //        line continuation tokens
            tt_ret = tt;

            while (tt != tkNullChar) {
                tt = getType(1);
                pos++;
                if (tt == tkHash) {
                    while (*pos != '\n' && *pos != '\0') pos++;
                    tt = getType(0);
                }
                if (tt == tkNewline) {
                    line++;
                    col = 0;
                }
                if (tt != tkSpaces && tt != tkNewline && tt != tkHash) break;
            }
            break;

        case tkArrayOpen:
        case tkBraceOpen:
            // go on for array open [. here you need to identify [:,:,:] as a
            // single token
            //        self->pos++;

            while (pos && *pos) {
                pos++;
                if (*pos == '#')
                    while (*pos && *pos != '\n') pos++;

                if (*pos == '\n') {
                    line++;
                    col = 0;
                }
                if (*pos != ' ' && *pos != '\n' && *pos != '#') {
                    pos--;
                    break;
                }
            }
            // tt_ret = tt == tkArrayOpen ? tkListLiteral : tkDictLiteral;

            // mergearraydims should be set only when reading func args
            if (!mergeArrayDims) goto defaultToken;

            // during mergeDims [:,:] is considered 1 token.
            // hopefully nobody embeds spaces and comments here, but...
            // TODO: parse comments and spaces embedded in tkArrayDims
            while (tt != tkNullChar) {
                tt = getType(1);
                pos++;
                if (tt != tkOpColon && tt != tkOpComma) break;
            }
            tt = getType(0);
            if (tt != tkArrayClose) {
                unreachable("expected a ']', found a '%c'. now what?\n", *pos);
            }
            pos++;
            tt_ret = tkArrayDims;
            break;

        case tkPeriod:
            tt_ret = isUnaryAfter(tt_lastNonSpace) ? tkUnaryDot : tt;
            pos++;
            break;

        case tkAlphabet:
        case tkUnderscore:
            while (tt != tkNullChar) {
                tt = getType(1);
                pos++;
                if (tt != tkAlphabet && tt != tkDigit && tt != tkUnderscore)
                    // and tt != tkPeriod)
                    break; /// validate in parser not here
            }
            if (tt == tkExclamation) pos++; // include it in ident
            tt_ret = tkIdentifier;
            break;

        case tkTilde: // tkExclamation:
            while (tt != tkNullChar) {
                tt = getType(1);
                pos++;
                if (tt == tkNewline) break;
            }
            tt_ret = tkLineComment;
            break;

        case tkPipe:
            while (tt != tkNullChar) {
                tt = getType(1);
                pos++;
                if (tt != tkAlphabet && tt != tkDigit && tt != tkOpSlash
                    && tt != tkPeriod)
                    break;
            }
            tt_ret = tkUnits;
            break;

        case tkDigit:
            tt_ret = tkNumber;

            while (tt != tkNullChar) // EOF, basically null char
            {
                tt = getType(1); // getType should not be used for lookahead!
                // use peek instead

                // numbers such as 1234500.00 are allowed
                // very crude, error-checking is parser's job not tokenizer's
                pos++;

                if (isin(*pos, "eEdD", 4)) { // will all be changed to e btw
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

                if (tt != tkDigit && tt != tkPeriod && *pos != 'i') break;
            }
            break;

        case tkOpMinus:
            tt_ret = isUnaryAfter(tt_lastNonSpace) ? tkOpUnaryMinus : tt;
            pos++;
            break;

        case tkOpNotResults:
            // 3-char tokens
            pos++;
        case tkOpEQ:
        case tkOpGE:
        case tkOpLE:
        case tkOpNE:
        case tkOpResults:
        case tkOpBackslash:
        case tkOpPlusEq:
        case tkOpMinusEq:
        case tkOpTimesEq:
        case tkOpSlashEq:
        case tkOpPowerEq:
        case tkOpModEq:

            // 2-char tokens
            pos++;
        default:
        defaultToken:
            tt_ret = tt;
            pos++;
            break;
        }

        len = (uint32_t)(pos - start);
        pos = start;
        kind = tt_ret;

        if (is(tkIdentifier)) tryKeywordMatch();

        if (is(tkSpaces) && len == 1) kind = tkOneSpace;

        tt_last = kind;
        if (tt_last != tkOneSpace && tt_last != tkSpaces)
            tt_lastNonSpace = tt_last;
    }

    // Advance to the next self->token (skip whitespace if `skipws` is set).
    void advance() {
        // Lexer& lex = *this;
        switch (kind) {
        case tkNullChar:
            unreachable("Advancing token at end of file! %p", pos);
            exit(1);
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
        // case tkKeyword_cheater:
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
        case tkUnknown: // bcz start of the file is this
            //    case tkArrayOpen:
            //    case tkBraceOpen:
            break;
        default:
            *pos = 0; // trample it so that idents etc. can be assigned
                      // in-situ
        }

        pos += len;
        col += len;
        len = 0;
        detect();

        if (kind == tkNewline) {
            // WHY don't you do self->token advance here?
            // TODO: if ( col>80) warning
            line++;
            col = 0; // position of the nl itself is 0
        }
        if (skipWhiteSpace
            && (is(tkSpaces) || (strictSpacing && kind == tkOneSpace)))
            advance();
    }
};

bool isin(char c, const char* s, int len) {
    while (--len > 0)
        if (s[len] == c) return true;
    return false;
}