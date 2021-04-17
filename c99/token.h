
#include "tokenkind.h"
#include "tokenkind_funcs.h"

static const uint8_t tokenkind_eTable[256] = {
    /* 0 */ tk_nullChar, /* 1 */ tk_unknown, /* 2 */ tk_unknown,
    /* 3 */ tk_unknown, /* 4 */ tk_unknown, /* 5 */ tk_unknown,
    /* 6 */ tk_unknown, /* 7 */ tk_unknown, /* 8 */ tk_unknown,
    /* 9 */ tk_unknown, /* 10 */ tk_newline, /* 11 */ tk_unknown,
    /* 12 */ tk_unknown, /* 13 */ tk_unknown, /* 14 */ tk_unknown,
    /* 15 */ tk_unknown, /* 16 */ tk_unknown, /* 17 */ tk_unknown,
    /* 18 */ tk_unknown, /* 19 */ tk_unknown, /* 20 */ tk_unknown,
    /* 21 */ tk_unknown, /* 22 */ tk_unknown, /* 23 */ tk_unknown,
    /* 24 */ tk_unknown, /* 25 */ tk_unknown, /* 26 */ tk_unknown,
    /* 27 */ tk_unknown, /* 28 */ tk_unknown, /* 29 */ tk_unknown,
    /* 30 */ tk_unknown, /* 31 */ tk_unknown,
    /* 32   */ tk_spaces, /* 33 ! */ tk_exclamation,
    /* 34 " */ tk_stringBoundary, /* 35 # */ tk_hash, /* 36 $ */ tk_dollar,
    /* 37 % */ tk_opMod, /* 38 & */ tk_ampersand,
    /* 39 ' */ tk_rawStringBoundary,
    /* 40 ( */ tk_parenOpen, /* 41 ) */ tk_parenClose, /* 42 * */ tk_opTimes,
    /* 43 + */ tk_opPlus, /* 44 , */ tk_opComma, /* 45 - */ tk_opMinus,
    /* 46 . */ tk_period, /* 47 / */ tk_opSlash, /* 48 0 */ tk_digit,
    /* 49 1 */ tk_digit, /* 50 2 */ tk_digit, /* 51 3 */ tk_digit,
    /* 52 4 */ tk_digit, /* 53 5 */ tk_digit, /* 54 6 */ tk_digit,
    /* 55 7 */ tk_digit, /* 56 8 */ tk_digit, /* 57 9 */ tk_digit,
    /* 58 : */ tk_opColon, /* 59 ; */ tk_opSemiColon, /* 60 < */ tk_opLT,
    /* 61 = */ tk_opAssign, /* 62 > */ tk_opGT, /* 63 ? */ tk_opQuestion,
    /* 64 @ */ tk_at,
    /* 65 A */ tk_alphabet, /* 66 B */ tk_alphabet, /* 67 C */ tk_alphabet,
    /* 68 D */ tk_alphabet, /* 69 E */ tk_alphabet, /* 70 F */ tk_alphabet,
    /* 71 G */ tk_alphabet, /* 72 H */ tk_alphabet, /* 73 I */ tk_alphabet,
    /* 74 J */ tk_alphabet, /* 75 K */ tk_alphabet, /* 76 L */ tk_alphabet,
    /* 77 M */ tk_alphabet, /* 78 N */ tk_alphabet, /* 79 O */ tk_alphabet,
    /* 80 P */ tk_alphabet, /* 81 Q */ tk_alphabet, /* 82 R */ tk_alphabet,
    /* 83 S */ tk_alphabet, /* 84 T */ tk_alphabet, /* 85 U */ tk_alphabet,
    /* 86 V */ tk_alphabet, /* 87 W */ tk_alphabet, /* 88 X */ tk_alphabet,
    /* 89 Y */ tk_alphabet, /* 90 Z */ tk_alphabet,
    /* 91 [ */ tk_arrayOpen, /* 92 \ */ tk_opBackslash,
    /* 93 ] */ tk_arrayClose,
    /* 94 ^ */ tk_opPower, /* 95 _ */ tk_underscore,
    /* 96 ` */ tk_regexpBoundary,
    /* 97 a */ tk_alphabet, /* 98 b */ tk_alphabet, /* 99 c */ tk_alphabet,
    /* 100 d */ tk_alphabet, /* 101 e */ tk_alphabet, /* 102 f */ tk_alphabet,
    /* 103 g */ tk_alphabet, /* 104 h */ tk_alphabet, /* 105 i */ tk_alphabet,
    /* 106 j */ tk_alphabet, /* 107 k */ tk_alphabet, /* 108 l */ tk_alphabet,
    /* 109 m */ tk_alphabet, /* 110 n */ tk_alphabet, /* 111 o */ tk_alphabet,
    /* 112 p */ tk_alphabet, /* 113 q */ tk_alphabet, /* 114 r */ tk_alphabet,
    /* 115 s */ tk_alphabet, /* 116 t */ tk_alphabet, /* 117 u */ tk_alphabet,
    /* 118 v */ tk_alphabet, /* 119 w */ tk_alphabet, /* 120 x */ tk_alphabet,
    /* 121 y */ tk_alphabet, /* 122 z */ tk_alphabet,
    /* 123 { */ tk_braceOpen, /* 124 | */ tk_pipe, /* 125 } */ tk_braceClose,
    /* 126 ~ */ tk_tilde,
    /* 127 */ tk_unknown, /* 128 */ tk_unknown, /* 129 */ tk_unknown,
    /* 130 */ tk_unknown, /* 131 */ tk_unknown, /* 132 */ tk_unknown,
    /* 133 */ tk_unknown, /* 134 */ tk_unknown, /* 135 */ tk_unknown,
    /* 136 */ tk_unknown, /* 137 */ tk_unknown, /* 138 */ tk_unknown,
    /* 139 */ tk_unknown, /* 140 */ tk_unknown, /* 141 */ tk_unknown,
    /* 142 */ tk_unknown, /* 143 */ tk_unknown, /* 144 */ tk_unknown,
    /* 145 */ tk_unknown, /* 146 */ tk_unknown, /* 147 */ tk_unknown,
    /* 148 */ tk_unknown, /* 149 */ tk_unknown, /* 150 */ tk_unknown,
    /* 151 */ tk_unknown, /* 152 */ tk_unknown, /* 153 */ tk_unknown,
    /* 154 */ tk_unknown, /* 155 */ tk_unknown, /* 156 */ tk_unknown,
    /* 157 */ tk_unknown, /* 158 */ tk_unknown, /* 159 */ tk_unknown,
    /* 160 */ tk_unknown, /* 161 */ tk_unknown, /* 162 */ tk_unknown,
    /* 163 */ tk_unknown, /* 164 */ tk_unknown, /* 165 */ tk_unknown,
    /* 166 */ tk_unknown, /* 167 */ tk_unknown, /* 168 */ tk_unknown,
    /* 169 */ tk_unknown, /* 170 */ tk_unknown, /* 171 */ tk_unknown,
    /* 172 */ tk_unknown, /* 173 */ tk_unknown, /* 174 */ tk_unknown,
    /* 175 */ tk_unknown, /* 176 */ tk_unknown, /* 177 */ tk_unknown,
    /* 178 */ tk_unknown, /* 179 */ tk_unknown, /* 180 */ tk_unknown,
    /* 181 */ tk_unknown, /* 182 */ tk_unknown, /* 183 */ tk_unknown,
    /* 184 */ tk_unknown, /* 185 */ tk_unknown, /* 186 */ tk_unknown,
    /* 187 */ tk_unknown, /* 188 */ tk_unknown, /* 189 */ tk_unknown,
    /* 190 */ tk_unknown, /* 191 */ tk_unknown, /* 192 */ tk_unknown,
    /* 193 */ tk_unknown, /* 194 */ tk_unknown, /* 195 */ tk_unknown,
    /* 196 */ tk_unknown, /* 197 */ tk_unknown, /* 198 */ tk_unknown,
    /* 199 */ tk_unknown, /* 200 */ tk_unknown, /* 201 */ tk_unknown,
    /* 202 */ tk_unknown, /* 203 */ tk_unknown, /* 204 */ tk_unknown,
    /* 205 */ tk_unknown, /* 206 */ tk_unknown, /* 207 */ tk_unknown,
    /* 208 */ tk_unknown, /* 209 */ tk_unknown, /* 210 */ tk_unknown,
    /* 211 */ tk_unknown, /* 212 */ tk_unknown, /* 213 */ tk_unknown,
    /* 214 */ tk_unknown, /* 215 */ tk_unknown, /* 216 */ tk_unknown,
    /* 217 */ tk_unknown, /* 218 */ tk_unknown, /* 219 */ tk_unknown,
    /* 220 */ tk_unknown, /* 221 */ tk_unknown, /* 222 */ tk_unknown,
    /* 223 */ tk_unknown, /* 224 */ tk_unknown, /* 225 */ tk_unknown,
    /* 226 */ tk_unknown, /* 227 */ tk_unknown, /* 228 */ tk_unknown,
    /* 229 */ tk_unknown, /* 230 */ tk_unknown, /* 231 */ tk_unknown,
    /* 232 */ tk_unknown, /* 233 */ tk_unknown, /* 234 */ tk_unknown,
    /* 235 */ tk_unknown, /* 236 */ tk_unknown, /* 237 */ tk_unknown,
    /* 238 */ tk_unknown, /* 239 */ tk_unknown, /* 240 */ tk_unknown,
    /* 241 */ tk_unknown, /* 242 */ tk_unknown, /* 243 */ tk_unknown,
    /* 244 */ tk_unknown, /* 245 */ tk_unknown, /* 246 */ tk_unknown,
    /* 247 */ tk_unknown, /* 248 */ tk_unknown, /* 249 */ tk_unknown,
    /* 250 */ tk_unknown, /* 251 */ tk_unknown, /* 252 */ tk_unknown,
    /* 253 */ tk_unknown, /* 254 */ tk_unknown, /* 255 */ tk_unknown
};
// #define Token_matchesKeyword(tok)                                              \
//     if (sizeof(#tok) - 1 == l && !strncmp(#tok, s, l)) return true;

// static bool doesKeywordMatch(const char* s, const int l) {
//     //        const char* s = pos;
//     //        const int l = matchlen;

//     Token_matchesKeyword(and)
//     Token_matchesKeyword(cheater)
//     Token_matchesKeyword(for)
//     Token_matchesKeyword(do)
//     Token_matchesKeyword(while)
//     Token_matchesKeyword(if)
//     Token_matchesKeyword(then)
//     Token_matchesKeyword(end)
//     Token_matchesKeyword(function)
//     Token_matchesKeyword(yes)
//     Token_matchesKeyword(no)
//     Token_matchesKeyword(nil)
//     Token_matchesKeyword(declare)
//     Token_matchesKeyword(test)
//     Token_matchesKeyword(not)
//     Token_matchesKeyword(and)
//     Token_matchesKeyword(or)
//     Token_matchesKeyword(in)
//     Token_matchesKeyword(else)
//     Token_matchesKeyword(type)
//     //    matchesen_compareKeyword(check)
//     Token_matchesKeyword(extends)
//     Token_matchesKeyword(var)
//     Token_matchesKeyword(let)
//     Token_matchesKeyword(import)
//     Token_matchesKeyword(return)
//     Token_matchesKeyword(result)
//     Token_matchesKeyword(as)
//     return false;
// }

// Holds information about a syntax self->token.
typedef struct Token {
    char* pos;
    uint32_t matchlen : 24;
    struct {
        bool skipWhiteSpace : 1,
            mergedims : 1, // merge [:,:,:] into one self->token
            noKeywosrdDetect : 1, // leave keywords as idents
            strictSpacing : 1; // missing spacing around operators etc. is a
                               // compile error YES YOU HEARD IT RIGHT
                               // but why need this AND skipWhiteSpace?
    };
    uint16_t line;
    uint8_t col;
    tokenkind_e kind : 8;
} Token;

// Peek at the char after the current (complete) token
static char Token_peekCharAfter(Token* token) {
    char* s = token->pos + token->matchlen;
    if (token->skipWhiteSpace)
        while (*s == ' ') s++;
    return *s;
}

#define Token_compareKeywordAlt(tok, actual)                                   \
    if (sizeof(#tok) - 1 == l && !strncasecmp(#tok, s, l)) {                   \
        token->kind = tk_keyword_##actual;                                     \
        return;                                                                \
    }
#define Token_compareKeyword(tok) Token_compareKeywordAlt(tok, tok)

// #define Token_compareKeyword(tok) Token_compareKeywordWith(tok,tok)

// Check if an (ident) self->token matches a keyword and return its type
// accordingly.
static void Token_tryKeywordMatch(Token* token) {
    // TODO: USE A DICT OR MPH FOR THIS!
    if (token->kind != tk_identifier) return;

    const char* s = token->pos;
    const int l = token->matchlen;

    Token_compareKeyword(and)
    // Token_compareKeyword(cheater)
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
        token->kind = tk_keyword_elif;
        token->matchlen = 7;
        return;
    }
    if (!strncasecmp("not in ", s, 7)) {
        token->kind = tk_keyword_notin;
        token->matchlen = 6;
        return;
    }
    Token_compareKeyword(else) Token_compareKeyword(not )

    // Token_compareKeyword(elif)

    //        Token_compareKeyword(print);
    //     if (sizeof("else if") - 1 == l and not strncmp("else if", s, l))
    // {
    //     self->kind = tk_keyword_elseif;
    //     return;
    // }
}

// Get the token kind based only on the char at the current position
// (or an offset).
static tokenkind_e Token_getType(Token* token, const size_t offset) {
    const char c = token->pos[offset];
    const char cn = c ? token->pos[1 + offset] : 0;
    tokenkind_e ret = (tokenkind_e)tokenkind_eTable[(unsigned char)c];
    switch (c) {
    case '<':
        switch (cn) {
        case '=': return tk_opLE;
        default: return tk_opLT;
        }
    case '>':
        switch (cn) {
        case '=': return tk_opGE;
        default: return tk_opGT;
        }
    case '=':
        switch (cn) {
        case '=': return tk_opEQ;
        case '>': return tk_opResults;
        default: return tk_opAssign;
        }
    case '+':
        switch (cn) {
        case '=': return tk_opPlusEq;
        }
        return tk_opPlus;
    case '-':
        switch (cn) {
        case '=': return tk_opMinusEq;
        }
        return tk_opMinus;
    case '*':
        switch (cn) {
        case '=': return tk_opTimesEq;
        }
        return tk_opTimes;
    case '/':
        switch (cn) {
        case '=': return tk_opSlashEq;
        }
        return tk_opSlash;
    case '^':
        switch (cn) {
        case '=': return tk_opPowerEq;
        }
        return tk_opPower;
    case '%':
        switch (cn) {
        case '=': return tk_opModEq;
        }
        return tk_opMod;
    case '!':
        switch (cn) {
        case '=': return tk_opNE;
        }
        return tk_exclamation;
    case ':':
        switch (cn) {
        case '=': return tk_opColEq;
        default: return tk_opColon;
        }
    default: return ret;
    }
}

static bool Token_isUnaryAfter(tokenkind_e tk) {
    switch (tk) {
    case tk_parenClose:
    case tk_identifier: // TODO: keywords too?
    case tk_number:
    case tk_arrayClose:
    case tk_arrayDims:
    case tk_multiDotNumber: return false;
    default:;
    }
    return true;
}

static void Token_detect(Token* token) {
    tokenkind_e tt = Token_getType(token, 0);
    tokenkind_e tt_ret = tk_unknown; // = tt;
    static tokenkind_e tt_last = tk_unknown;
    // the previous self->token that was found
    static tokenkind_e tt_last_nonSpace = tk_unknown;
    // the last non-space self->token found
    tokenkind_e tmp;
    char* start = token->pos;
    bool found_e = false, found_dot = false; //, found_cmt = false;
    //    uint8_t found_spc = 0;

    switch (tt) {
    case tk_stringBoundary:
    case tk_regexpBoundary:
    case tk_rawStringBoundary:
        tmp = tt; // remember which it is exactly

        // Incrementing pos is a side effect of getTypeAtCurrentPos(...)
        while (tt != tk_nullChar) {
            // here we want to consume the ending " so we move next
            // before
            token->pos++;
            tt = Token_getType(token, 0);
            if (tt == tk_nullChar || tt == tmp) {
                *token->pos = 0;
                token->pos++;
                break;
            }
            if (tt == tk_opBackslash && Token_getType(token, 1) == tmp)
                token->pos++;
            if (tt == tk_newline) {
                token->line++;
                token->col = 0;
            }
        }
        switch (tmp) {
        case tk_stringBoundary: tt_ret = tk_string; break;
        case tk_regexpBoundary: tt_ret = tk_regexp; break;
        case tk_rawStringBoundary: tt_ret = tk_rawString; break;
        default:
            tt_ret = tk_unknown;
            printf("unreachable %s:%d\n", __FILE__, __LINE__);
        }
        break;

    case tk_spaces:
        if (tt_last == tk_oneSpace) // if prev char was a space return
                                    // this as a run of spaces
            while (tt != tk_nullChar) {
                // here we dont want to consume the end char, so break
                // before
                tt = Token_getType(token, 1);
                token->pos++;
                if (tt != tk_spaces) break;
            }
        else
            token->pos++;
        // else its a single space
        tt_ret = tk_spaces;
        break;

    case tk_opColEq:
        token->pos++; // 2-char token. fallthrough for line continuation
    case tk_opComma:
    case tk_opSemiColon:
    case tk_opAssign: // line continuation tokens
        tt_ret = tt;

        while (tt != tk_nullChar) {
            tt = Token_getType(token, 1);
            token->pos++;
            if (tt == tk_hash) {
                while (*token->pos != '\n' && *token->pos != '\0') token->pos++;
                tt = Token_getType(token, 0);
            }
            if (tt == tk_newline) {
                token->line++;
                token->col = 0;
            }
            if (tt != tk_spaces && tt != tk_newline && tt != tk_hash) break;
        }
        break;

    case tk_arrayOpen:
    case tk_braceOpen:
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
        // tt_ret = tt == tk_arrayOpen ? tk_listLiteral : tk_dictLiteral;

        // mergearraydims should be set only when reading func args
        if (!token->mergedims) goto defaultToken;

        // during mergeDims [:,:] is considered 1 token.
        // hopefully nobody embeds spaces and comments here, but...
        // TODO: parse comments and spaces embedded in tk_arrayDims
        while (tt != tk_nullChar) {
            tt = Token_getType(token, 1);
            token->pos++;
            if (tt != tk_opColon && tt != tk_opComma) break;
        }
        tt = Token_getType(token, 0);
        if (tt != tk_arrayClose) {
            unreachable(
                "expected a ']', found a '%c'. now what?\n", *token->pos);
        }
        token->pos++;
        tt_ret = tk_arrayDims;
        break;

    case tk_period:
        tt_ret = Token_isUnaryAfter(tt_last_nonSpace) ? tk_unaryDot : tt;
        token->pos++;
        break;

    case tk_alphabet:
    case tk_underscore:
        while (tt != tk_nullChar) {
            tt = Token_getType(token, 1);
            token->pos++;
            if (tt != tk_alphabet && tt != tk_digit && tt != tk_underscore)
                // and tt != tk_period)
                break; /// validate in parser not here
        }
        if (tt == tk_exclamation) token->pos++; // include it in ident
        tt_ret = tk_identifier;
        break;

    case tk_tilde: // tk_exclamation:
        while (tt != tk_nullChar) {
            tt = Token_getType(token, 1);
            token->pos++;
            if (tt == tk_newline) break;
        }
        tt_ret = tk_lineComment;
        break;

    case tk_pipe:
        while (tt != tk_nullChar) {
            tt = Token_getType(token, 1);
            token->pos++;
            if (tt != tk_alphabet && tt != tk_digit && tt != tk_opSlash
                && tt != tk_period)
                break;
        }
        tt_ret = tk_units;
        break;

    case tk_digit:
        tt_ret = tk_number;

        while (tt != tk_nullChar) // EOF, basically null char
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
            if (tt == tk_period) {
                found_dot = true;
                continue;
            }
            if (found_dot && tt == tk_period) tt_ret = tk_multiDotNumber;

            if (tt != tk_digit && tt != tk_period && *token->pos != 'i') break;
        }
        break;

    case tk_opMinus:
        tt_ret = Token_isUnaryAfter(tt_last_nonSpace) ? tk_opUnaryMinus : tt;
        token->pos++;
        break;

    // 3-char tokens
    case tk_opNotResults: token->pos++;

    // 2-char tokens
    case tk_opEQ:
    case tk_opGE:
    case tk_opLE:
    case tk_opNE:
    case tk_opResults:
    case tk_opBackslash:
    case tk_opPlusEq:
    case tk_opMinusEq:
    case tk_opTimesEq:
    case tk_opSlashEq:
    case tk_opPowerEq:
    case tk_opModEq: token->pos++;

    default:
    defaultToken:
        tt_ret = tt;
        token->pos++;
        break;
    }

    token->matchlen = (uint32_t)(token->pos - start);
    token->pos = start;
    token->kind = tt_ret;

    if (token->kind == tk_identifier) Token_tryKeywordMatch(token);

    if (token->kind == tk_spaces && token->matchlen == 1)
        token->kind = tk_oneSpace;

    tt_last = token->kind;
    if (tt_last != tk_oneSpace && tt_last != tk_spaces)
        tt_last_nonSpace = tt_last;
}

// Advance to the next self->token (skip whitespace if `skipws` is set).
static void Token_advance(Token* token) {
    switch (token->kind) {
    case tk_nullChar:
        unreachable("Advancing token at end of file! %p", token->pos);
        exit(1);
        return;
    case tk_identifier:
    case tk_string:
    case tk_number:
    case tk_multiDotNumber:
    case tk_functionCall:
    case tk_subscript:
    case tk_digit:
    case tk_alphabet:
    case tk_rawString:
    case tk_regexp:
    case tk_units:
    // case tk_keyword_cheater:
    case tk_keyword_for:
    case tk_keyword_while:
    case tk_keyword_if:
    case tk_keyword_end:
    case tk_keyword_function:
    case tk_keyword_test:
    case tk_keyword_not:
    case tk_keyword_and:
    case tk_keyword_or:
    case tk_keyword_in:
    case tk_keyword_do:
    case tk_keyword_then:
    case tk_keyword_as:
    case tk_keyword_else:
    case tk_keyword_type:
    case tk_keyword_return:
    case tk_keyword_extends:
    case tk_keyword_var:
    case tk_keyword_let:
    case tk_keyword_import:
    case tk_unknown: break;
    default:
        *token->pos = 0; // trample it so that idents etc. can be assigned
                         // in-situ
    }

    token->pos += token->matchlen;
    token->col += token->matchlen;
    token->matchlen = 0;
    Token_detect(token);

    if (token->kind == tk_newline) {
        // WHY don't you do self->token advance here?
        // TODO: if (token->col>80) warning
        token->line++;
        token->col = 0; // position of the nl itself is 0
    }
    if (token->skipWhiteSpace
        && (token->kind == tk_spaces
            || (token->strictSpacing && token->kind == tk_oneSpace)))
        Token_advance(token);
}
//
