
static const uint8_t TokenKindTable[256] = {
    /* 0 */ tkNullChar, /* 1 */ tkUnknown, /* 2 */ tkUnknown,
    /* 3 */ tkUnknown, /* 4 */ tkUnknown, /* 5 */ tkUnknown,
    /* 6 */ tkUnknown, /* 7 */ tkUnknown, /* 8 */ tkUnknown,
    /* 9 */ tkUnknown, /* 10 */ tkNewline, /* 11 */ tkUnknown,
    /* 12 */ tkUnknown, /* 13 */ tkUnknown, /* 14 */ tkUnknown,
    /* 15 */ tkUnknown, /* 16 */ tkUnknown, /* 17 */ tkUnknown,
    /* 18 */ tkUnknown, /* 19 */ tkUnknown, /* 20 */ tkUnknown,
    /* 21 */ tkUnknown, /* 22 */ tkUnknown, /* 23 */ tkUnknown,
    /* 24 */ tkUnknown, /* 25 */ tkUnknown, /* 26 */ tkUnknown,
    /* 27 */ tkUnknown, /* 28 */ tkUnknown, /* 29 */ tkUnknown,
    /* 30 */ tkUnknown, /* 31 */ tkUnknown,
    /* 32   */ tkSpaces, /* 33 ! */ tkExclamation,
    /* 34 " */ tkStringBoundary, /* 35 # */ tkHash, /* 36 $ */ tkDollar,
    /* 37 % */ tkOpMod, /* 38 & */ tkAmpersand, /* 39 ' */ tkRawStringBoundary,
    /* 40 ( */ tkParenOpen, /* 41 ) */ tkParenClose, /* 42 * */ tkTimes,
    /* 43 + */ tkPlus, /* 44 , */ tkOpComma, /* 45 - */ tkMinus,
    /* 46 . */ tkPeriod, /* 47 / */ tkSlash, /* 48 0 */ tkDigit,
    /* 49 1 */ tkDigit, /* 50 2 */ tkDigit, /* 51 3 */ tkDigit,
    /* 52 4 */ tkDigit, /* 53 5 */ tkDigit, /* 54 6 */ tkDigit,
    /* 55 7 */ tkDigit, /* 56 8 */ tkDigit, /* 57 9 */ tkDigit,
    /* 58 : */ tkOpColon, /* 59 ; */ tkOpSemiColon, /* 60 < */ tkOpLT,
    /* 61 = */ tkOpAssign, /* 62 > */ tkOpGT, /* 63 ? */ tkQuestion,
    /* 64 @ */ tkAt,
    /* 65 A */ tkAlphabet, /* 66 B */ tkAlphabet, /* 67 C */ tkAlphabet,
    /* 68 D */ tkAlphabet, /* 69 E */ tkAlphabet, /* 70 F */ tkAlphabet,
    /* 71 G */ tkAlphabet, /* 72 H */ tkAlphabet, /* 73 I */ tkAlphabet,
    /* 74 J */ tkAlphabet, /* 75 K */ tkAlphabet, /* 76 L */ tkAlphabet,
    /* 77 M */ tkAlphabet, /* 78 N */ tkAlphabet, /* 79 O */ tkAlphabet,
    /* 80 P */ tkAlphabet, /* 81 Q */ tkAlphabet, /* 82 R */ tkAlphabet,
    /* 83 S */ tkAlphabet, /* 84 T */ tkAlphabet, /* 85 U */ tkAlphabet,
    /* 86 V */ tkAlphabet, /* 87 W */ tkAlphabet, /* 88 X */ tkAlphabet,
    /* 89 Y */ tkAlphabet, /* 90 Z */ tkAlphabet,
    /* 91 [ */ tkArrayOpen, /* 92 \ */ tkBackslash, /* 93 ] */ tkArrayClose,
    /* 94 ^ */ tkPower, /* 95 _ */ tkUnderscore,
    /* 96 ` */ tkRegexpBoundary,
    /* 97 a */ tkAlphabet, /* 98 b */ tkAlphabet, /* 99 c */ tkAlphabet,
    /* 100 d */ tkAlphabet, /* 101 e */ tkAlphabet, /* 102 f */ tkAlphabet,
    /* 103 g */ tkAlphabet, /* 104 h */ tkAlphabet, /* 105 i */ tkAlphabet,
    /* 106 j */ tkAlphabet, /* 107 k */ tkAlphabet, /* 108 l */ tkAlphabet,
    /* 109 m */ tkAlphabet, /* 110 n */ tkAlphabet, /* 111 o */ tkAlphabet,
    /* 112 p */ tkAlphabet, /* 113 q */ tkAlphabet, /* 114 r */ tkAlphabet,
    /* 115 s */ tkAlphabet, /* 116 t */ tkAlphabet, /* 117 u */ tkAlphabet,
    /* 118 v */ tkAlphabet, /* 119 w */ tkAlphabet, /* 120 x */ tkAlphabet,
    /* 121 y */ tkAlphabet, /* 122 z */ tkAlphabet,
    /* 123 { */ tkBraceOpen, /* 124 | */ tkPipe, /* 125 } */ tkBraceClose,
    /* 126 ~ */ tkTilde,
    /* 127 */ tkUnknown, /* 128 */ tkUnknown, /* 129 */ tkUnknown,
    /* 130 */ tkUnknown, /* 131 */ tkUnknown, /* 132 */ tkUnknown,
    /* 133 */ tkUnknown, /* 134 */ tkUnknown, /* 135 */ tkUnknown,
    /* 136 */ tkUnknown, /* 137 */ tkUnknown, /* 138 */ tkUnknown,
    /* 139 */ tkUnknown, /* 140 */ tkUnknown, /* 141 */ tkUnknown,
    /* 142 */ tkUnknown, /* 143 */ tkUnknown, /* 144 */ tkUnknown,
    /* 145 */ tkUnknown, /* 146 */ tkUnknown, /* 147 */ tkUnknown,
    /* 148 */ tkUnknown, /* 149 */ tkUnknown, /* 150 */ tkUnknown,
    /* 151 */ tkUnknown, /* 152 */ tkUnknown, /* 153 */ tkUnknown,
    /* 154 */ tkUnknown, /* 155 */ tkUnknown, /* 156 */ tkUnknown,
    /* 157 */ tkUnknown, /* 158 */ tkUnknown, /* 159 */ tkUnknown,
    /* 160 */ tkUnknown, /* 161 */ tkUnknown, /* 162 */ tkUnknown,
    /* 163 */ tkUnknown, /* 164 */ tkUnknown, /* 165 */ tkUnknown,
    /* 166 */ tkUnknown, /* 167 */ tkUnknown, /* 168 */ tkUnknown,
    /* 169 */ tkUnknown, /* 170 */ tkUnknown, /* 171 */ tkUnknown,
    /* 172 */ tkUnknown, /* 173 */ tkUnknown, /* 174 */ tkUnknown,
    /* 175 */ tkUnknown, /* 176 */ tkUnknown, /* 177 */ tkUnknown,
    /* 178 */ tkUnknown, /* 179 */ tkUnknown, /* 180 */ tkUnknown,
    /* 181 */ tkUnknown, /* 182 */ tkUnknown, /* 183 */ tkUnknown,
    /* 184 */ tkUnknown, /* 185 */ tkUnknown, /* 186 */ tkUnknown,
    /* 187 */ tkUnknown, /* 188 */ tkUnknown, /* 189 */ tkUnknown,
    /* 190 */ tkUnknown, /* 191 */ tkUnknown, /* 192 */ tkUnknown,
    /* 193 */ tkUnknown, /* 194 */ tkUnknown, /* 195 */ tkUnknown,
    /* 196 */ tkUnknown, /* 197 */ tkUnknown, /* 198 */ tkUnknown,
    /* 199 */ tkUnknown, /* 200 */ tkUnknown, /* 201 */ tkUnknown,
    /* 202 */ tkUnknown, /* 203 */ tkUnknown, /* 204 */ tkUnknown,
    /* 205 */ tkUnknown, /* 206 */ tkUnknown, /* 207 */ tkUnknown,
    /* 208 */ tkUnknown, /* 209 */ tkUnknown, /* 210 */ tkUnknown,
    /* 211 */ tkUnknown, /* 212 */ tkUnknown, /* 213 */ tkUnknown,
    /* 214 */ tkUnknown, /* 215 */ tkUnknown, /* 216 */ tkUnknown,
    /* 217 */ tkUnknown, /* 218 */ tkUnknown, /* 219 */ tkUnknown,
    /* 220 */ tkUnknown, /* 221 */ tkUnknown, /* 222 */ tkUnknown,
    /* 223 */ tkUnknown, /* 224 */ tkUnknown, /* 225 */ tkUnknown,
    /* 226 */ tkUnknown, /* 227 */ tkUnknown, /* 228 */ tkUnknown,
    /* 229 */ tkUnknown, /* 230 */ tkUnknown, /* 231 */ tkUnknown,
    /* 232 */ tkUnknown, /* 233 */ tkUnknown, /* 234 */ tkUnknown,
    /* 235 */ tkUnknown, /* 236 */ tkUnknown, /* 237 */ tkUnknown,
    /* 238 */ tkUnknown, /* 239 */ tkUnknown, /* 240 */ tkUnknown,
    /* 241 */ tkUnknown, /* 242 */ tkUnknown, /* 243 */ tkUnknown,
    /* 244 */ tkUnknown, /* 245 */ tkUnknown, /* 246 */ tkUnknown,
    /* 247 */ tkUnknown, /* 248 */ tkUnknown, /* 249 */ tkUnknown,
    /* 250 */ tkUnknown, /* 251 */ tkUnknown, /* 252 */ tkUnknown,
    /* 253 */ tkUnknown, /* 254 */ tkUnknown, /* 255 */ tkUnknown
};
// Holds information about a syntax self->token.
typedef struct Lexer {
    char *_pos, *end;
    uint32_t _matchlen : 24;
    struct {
        bool _skipWhiteSpace : 1,
            _mergeArrayDims : 1, // merge [:,:,:] into one self->token
            _noKeywosrdDetect : 1, // leave keywords as idents
            _strictSpacing : 1; // missing spacing around operators etc. is a
                                // compile error YES YOU HEARD IT RIGHT
                                // but why need this AND skipWhiteSpace?
    };
    uint16_t _line;
    uint8_t _col;
    TokenKind _kind : 8;

    bool is(TokenKind k) { return _kind == k; }

    ASTExpr* expr() {
        ASTExpr* expr = fromToken(com->token);
        advance();
        return expr;
    }

    ASTExpr* next_token_node(TokenKind expected, const bool ignore_error) {
        if (is(expected)) {
            return expr();
        } else {
            if (!ignore_error) errorExpectedToken(parser, expected);
            return NULL;
        }
    }
    ASTExpr* match(TokenKind expected) {
        return next_token_node(expected, false);
    }

    ASTExpr* trymatch(TokenKind expected) {
        return next_token_node(expected, true);
    }

    bool ignore(TokenKind expected) {
        bool ret;
        if ((ret = is(expected))) advance();
        return ret;
    }

    // this is same as match without return
    void discard(TokenKind expected) {
        if (!ignore(expected)) errorExpectedToken(parser, expected);
    }

    char peek() {
        char* s = _pos + _matchlen;
        if (_skipWhiteSpace)
            while (*s == ' ') s++;
        return *s;
    }

    // TokenKind getType(const size_t offset);
    // void detect();
    // void advance();
    // void tryKeywordMatch();

    ASTExpr* fromToken() {
        ASTExpr* ret = NEW(ASTExpr);
        ret->kind = _kind;
        ret->line = _line;
        ret->col = _col;

        ret->prec = TokenKind_getPrecedence(ret->kind);
        if (ret->prec) {
            ret->rassoc = TokenKind_isRightAssociative(ret->kind);
            ret->unary = TokenKind_isUnary(ret->kind);
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
        case tkLineComment: // Comments go in the AST like regular stmts
            ret->string = _pos;
            break;
        default:;
        }
        // the '!' will be trampled
        if (ret->kind == tkLineComment) ret->string++;
        // turn all 1.0234[DdE]+01 into 1.0234e+01.
        if (ret->kind == tkNumber) {
            CString_tr_ip(ret->string, 'd', 'e', _matchlen);
            CString_tr_ip(ret->string, 'D', 'e', _matchlen);
            CString_tr_ip(ret->string, 'E', 'e', _matchlen);
        }
        return ret;
    }
    // Peek at the char after the current (complete) token

#define compareKeywordAlt(tok, actual)                                         \
    if (sizeof(#tok) - 1 == l && !strncasecmp(#tok, s, l)) {                   \
        _kind = tkKeyword_##actual;                                            \
        return;                                                                \
    }
#define compareKeyword(tok) compareKeywordAlt(tok, tok)

    // #define compareKeyword(tok) compareKeywordWith(tok,tok)

    // Check if an (ident) self->token matches a keyword and return its type
    // accordingly.
    void tryKeywordMatch() {
        // TODO: USE A DICT OR MPH FOR THIS!
        if (_kind != tkIdentifier) return;

        const char* s = _pos;
        const int l = _matchlen;

    compareKeyword(and)
    compareKeyword(cheater)
    compareKeyword(for)
    compareKeyword(do)
    compareKeyword(while)
    compareKeyword(if)
    compareKeyword(then)
    compareKeyword(end)
    compareKeyword(enum)
    compareKeyword(match)
    compareKeyword(case)
    compareKeyword(function)
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
        _kind = tkKeyword_elif;
        _matchlen = 7;
        return;
    }
    if (!strncasecmp("not in ", s, 7)) {
        _kind = tkKeyword_notin;
        _matchlen = 6;
        return;
    }
    compareKeyword(else) compareKeyword(not )
    }

    // Get the token _kind based only on the char at the current position
    // (or an offset).
    TokenKind getType(const size_t offset) {
        const char c = _pos[offset];
        const char cn = c ? _pos[1 + offset] : 0;
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

    static bool isUnaryAfter(TokenKind tk) {
        switch (tk) {
        case tkParenClose:
        case tkIdentifier: // TODO: keywords too?
        case tkNumber:
        case tkArrayClose:
        case tkArrayDims:
        case tkMultiDotNumber:
            return false;
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
        char* start = _pos;
        bool found_e = false, found_dot = false; //, found_cmt = false;
        //    uint8_t found_spc = 0;

        switch (tt) {
        case tkStringBoundary:
        case tkRegexpBoundary:
        case tkRawStringBoundary:
            tmp = tt; // remember which it is exactly

            // Incrementing _pos is a side effect of getTypeAtCurrentPos(...)
            while (tt != tkNullChar) {
                // here we want to consume the ending " so we move next
                // before
                _pos++;
                tt = getType(0);
                if (tt == tkNullChar || tt == tmp) {
                    *_pos = 0;
                    _pos++;
                    break;
                }
                if (tt == tkBackslash && getType(1) == tmp) _pos++;
                if (tt == tkNewline) {
                    _line++;
                    _col = 0;
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
            if (tt_last == tkOneSpace) // if prev char was a space return
                                       // this as a run of spaces
                while (tt != tkNullChar) {
                    // here we dont want to consume the end char, so break
                    // before
                    tt = getType(1);
                    _pos++;
                    if (tt != tkSpaces) break;
                }
            else
                _pos++;
            // else its a single space
            tt_ret = tkSpaces;
            break;

        case tkColEq:
            _pos++; // 2-char token
            // fallthrough, since tkColEq is also a _line continuation token
            // like , and ;
        case tkOpComma:
        case tkOpSemiColon:
        case tkOpAssign:
            //        _line continuation tokens
            tt_ret = tt;

            while (tt != tkNullChar) {
                tt = getType(1);
                _pos++;
                if (tt == tkHash) {
                    while (*_pos != '\n' && *_pos != '\0') _pos++;
                    tt = getType(0);
                }
                if (tt == tkNewline) {
                    _line++;
                    _col = 0;
                }
                if (tt != tkSpaces && tt != tkNewline && tt != tkHash) break;
            }
            break;

        case tkArrayOpen:
        case tkBraceOpen:
            // go on for array open [. here you need to identify [:,:,:] as a
            // single token
            //        self->_pos++;

            while (_pos && *_pos) {
                _pos++;
                if (*_pos == '#')
                    while (*_pos && *_pos != '\n') _pos++;

                if (*_pos == '\n') {
                    _line++;
                    _col = 0;
                }
                if (*_pos != ' ' && *_pos != '\n' && *_pos != '#') {
                    _pos--;
                    break;
                }
            }
            // tt_ret = tt == tkArrayOpen ? tkListLiteral : tkDictLiteral;

            // mergearraydims should be set only when reading func args
            if (!_mergeArrayDims) goto defaultToken;

            // during mergeDims [:,:] is considered 1 token.
            // hopefully nobody embeds spaces and comments here, but...
            // TODO: parse comments and spaces embedded in tkArrayDims
            while (tt != tkNullChar) {
                tt = getType(1);
                _pos++;
                if (tt != tkOpColon && tt != tkOpComma) break;
            }
            tt = getType(0);
            if (tt != tkArrayClose) {
                unreachable("expected a ']', found a '%c'. now what?\n", *_pos);
            }
            _pos++;
            tt_ret = tkArrayDims;
            break;

        case tkPeriod:
            tt_ret = isUnaryAfter(tt_lastNonSpace) ? tkUnaryDot : tt;
            _pos++;
            break;

        case tkAlphabet:
        case tkUnderscore:
            while (tt != tkNullChar) {
                tt = getType(1);
                _pos++;
                if (tt != tkAlphabet && tt != tkDigit && tt != tkUnderscore)
                    // and tt != tkPeriod)
                    break; /// validate in parser not here
            }
            if (tt == tkExclamation) _pos++; // include it in ident
            tt_ret = tkIdentifier;
            break;

        case tkTilde: // tkExclamation:
            while (tt != tkNullChar) {
                tt = getType(1);
                _pos++;
                if (tt == tkNewline) break;
            }
            tt_ret = tkLineComment;
            break;

        case tkPipe:
            while (tt != tkNullChar) {
                tt = getType(1);
                _pos++;
                if (tt != tkAlphabet && tt != tkDigit && tt != tkSlash
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
                _pos++;

                if (isin(*_pos, "eEdD")) { // will all be changed to e btw
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

                if (tt != tkDigit && tt != tkPeriod && *_pos != 'i') break;
            }
            break;

        case tkMinus:
            tt_ret = isUnaryAfter(tt_lastNonSpace) ? tkUnaryMinus : tt;
            _pos++;
            break;

        case tkOpNotResults:
            // 3-char tokens
            _pos++;
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

            // 2-char tokens
            _pos++;
        default:
        defaultToken:
            tt_ret = tt;
            _pos++;
            break;
        }

        _matchlen = (uint32_t)(_pos - start);
        _pos = start;
        _kind = tt_ret;

        if (is(tkIdentifier)) tryKeywordMatch();

        if (is(tkSpaces) && _matchlen == 1) _kind = tkOneSpace;

        tt_last = _kind;
        if (tt_last != tkOneSpace && tt_last != tkSpaces)
            tt_lastNonSpace = tt_last;
    }

    // Advance to the next self->token (skip whitespace if `skipws` is set).
    void advance() {
        Lexer& lex = *this;
        switch (_kind) {
        case tkNullChar:
            unreachable("Advancing token at end of file! %p", _pos);
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
        case tkUnknown: // bcz start of the file is this
            //    case tkArrayOpen:
            //    case tkBraceOpen:
            break;
        default:
            *_pos = 0; // trample it so that idents etc. can be assigned
                       // in-situ
        }

        _pos += _matchlen;
        _col += _matchlen;
        _matchlen = 0;
        detect();

        if (_kind == tkNewline) {
            // WHY don't you do self->token advance here?
            // TODO: if ( _col>80) warning
            _line++;
            _col = 0; // position of the nl itself is 0
        }
        if (_skipWhiteSpace
            && (is(tkSpaces) || (_strictSpacing && _kind == tkOneSpace)))
            advance();
    }
    //
} Lexer;
