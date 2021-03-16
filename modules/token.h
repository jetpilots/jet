
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
#define Token_matchesKeyword(tok)                                              \
    if (sizeof(#tok) - 1 == l && !strncmp(#tok, s, l)) return true;

static bool doesKeywordMatch(const char* s, const int l) {
    //        const char* s = pos;
    //        const int l = matchlen;

    Token_matchesKeyword(and)
    Token_matchesKeyword(cheater)
    Token_matchesKeyword(for)
    Token_matchesKeyword(do)
    Token_matchesKeyword(while)
    Token_matchesKeyword(if)
    Token_matchesKeyword(then)
    Token_matchesKeyword(end)
    Token_matchesKeyword(function)
    Token_matchesKeyword(declare)
    Token_matchesKeyword(test)
    Token_matchesKeyword(not)
    Token_matchesKeyword(and)
    Token_matchesKeyword(or)
    Token_matchesKeyword(in)
    Token_matchesKeyword(else)
    Token_matchesKeyword(type)
    //    matchesen_compareKeyword(check)
    Token_matchesKeyword(extends)
    Token_matchesKeyword(var)
    Token_matchesKeyword(let)
    Token_matchesKeyword(import)
    Token_matchesKeyword(return)
    Token_matchesKeyword(result)
    Token_matchesKeyword(as)
    return false;
}

// Holds information about a syntax self->token.
typedef struct Token {

    char* pos;
    uint32_t matchlen : 24;
    struct {
        bool skipWhiteSpace : 1,
            mergeArrayDims : 1, // merge [:,:,:] into one self->token
            noKeywosrdDetect : 1, // leave keywords as idents
            strictSpacing : 1; // missing spacing around operators etc. is a
                               // compile error YES YOU HEARD IT RIGHT
                               // but why need this AND skipWhiteSpace?
    };
    uint16_t line;
    uint8_t col;
    TokenKind kind : 8;
} Token;

// Peek at the char after the current (complete) token
static char Token_peekCharAfter(Token* token) {
    char* s = token->pos + token->matchlen;
    if (token->skipWhiteSpace)
        while (*s == ' ') s++;
    return *s;
}

#define Token_compareKeyword(tok)                                              \
    if (sizeof(#tok) - 1 == l && !strncasecmp(#tok, s, l)) {                   \
        token->kind = tkKeyword_##tok;                                         \
        return;                                                                \
    }

// #define Token_compareKeyword(tok) Token_compareKeywordWith(tok,tok)

// Check if an (ident) self->token matches a keyword and return its type
// accordingly.
static void Token_tryKeywordMatch(Token* token) {
    // TODO: USE A DICT OR MPH FOR THIS!
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
    Token_compareKeyword(declare)
    Token_compareKeyword(test)
    Token_compareKeyword(and)
    Token_compareKeyword(or)
    Token_compareKeyword(in)
    // Token_compareKeyword(elif)
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

    // Token_compareKeyword(elif)

    //        Token_compareKeyword(print);
    //     if (sizeof("else if") - 1 == l and not strncmp("else if", s, l))
    // {
    //     self->kind = tkKeyword_elseif;
    //     return;
    // }
}

// Get the token kind based only on the char at the current position
// (or an offset).
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

static void Token_detect(Token* token) {
    TokenKind tt = Token_getType(token, 0);
    TokenKind tt_ret = tkUnknown; // = tt;
    static TokenKind tt_last = tkUnknown;
    // the previous self->token that was found
    static TokenKind tt_lastNonSpace = tkUnknown;
    // the last non-space self->token found
    TokenKind tmp;
    char* start = token->pos;
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
        if (tt_last == tkOneSpace) // if prev char was a space return
                                   // this as a run of spaces
            while (tt != tkNullChar) {
                // here we dont want to consume the end char, so break
                // before
                tt = Token_getType(token, 1);
                token->pos++;
                if (tt != tkSpaces) break;
            }
        else
            token->pos++;
        // else its a single space
        tt_ret = tkSpaces;
        break;

    case tkColEq:
        token->pos++; // 2-char token
        // fallthrough, since tkColEq is also a line continuation token
        // like , and ;
    case tkOpComma:
    case tkOpSemiColon:

        //        line continuation tokens
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
        // go on for array open [. here you need to identify [:,:,:] as a single
        // token
        //        self->pos++;

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
        // tt_ret = tt == tkArrayOpen ? tkListLiteral : tkDictLiteral;

        // mergearraydims should be set only when reading func args
        if (!token->mergeArrayDims) goto defaultToken;

        // during mergeDims [:,:] is considered 1 token.
        // hopefully nobody embeds spaces and comments here, but...
        // TODO: parse comments and spaces embedded in tkArrayDims
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
        // case tkPeriod:
    case tkUnderscore:
        while (tt != tkNullChar) {
            tt = Token_getType(token, 1);
            token->pos++;
            if (tt != tkAlphabet && tt != tkDigit && tt != tkUnderscore)
                // and tt != tkPeriod)
                break; /// validate in parser not here
        }
        tt_ret = tkIdentifier;
        break;

    case tkHash: // tkExclamation:
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

        while (tt != tkNullChar) // EOF, basically null char
        {
            tt = Token_getType(token, 1);
            // numbers such as 1234500.00 are allowed
            // very crude, error-checking is parser's job not tokenizer's
            token->pos++;

            if (*token->pos == 'e' || *token->pos == 'E' || *token->pos == 'd'
                || *token->pos == 'D') { // will all be changed to e btw
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

        switch (tt_lastNonSpace) {
        case tkParenClose:
        case tkIdentifier: // TODO: keywords too?
        case tkNumber:
        case tkArrayClose:
        case tkArrayDims:
        case tkMultiDotNumber:
            tt_ret = tt;
            break;
        default:
            tt_ret = tkUnaryMinus;
            break;
        }
        token->pos++;
        break;

    case tkOpNotResults:
        // 3-char tokens
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

        // 2-char tokens
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

// Advance to the next self->token (skip whitespace if `skipws` is set).
static void Token_advance(Token* token) {
    switch (token->kind) {
    case tkNullChar:
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
        *token->pos = 0; // trample it so that idents etc. can be assigned
                         // in-situ
    }

    token->pos += token->matchlen;
    token->col += token->matchlen;
    token->matchlen = 0;
    Token_detect(token);

    if (token->kind == tkNewline) {
        // WHY don't you do self->token advance here?
        token->line++;
        token->col = 0; // position of the nl itself is 0
    }
    if (token->skipWhiteSpace
        && (token->kind == tkSpaces
            || (token->strictSpacing && token->kind == tkOneSpace)))
        Token_advance(token);
}
//
