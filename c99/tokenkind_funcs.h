
// Return the repr of a self->token kind (for debug)

// static const char* tokenkind_e_repr(const tokenkind_e kind, bool spacing) {
//     return spacing ? tokenkind_es_srepr[kind] : tokenkind_es_repr[kind];
// }

// alternative ascii repr of a token kind
// you don't really need this, pass the original repr to a macro as param
// and the macro will deal with it. usually 3way_lt_le(a,b,c) can be same as
// 3way(<, <=, a, b, c) even for string e.g. < implies cmp(a,b) < 0
// TODO: get rid of this func

static const char* tokenkind_e_ascrepr(const tokenkind_e kind, bool spacing) {
    switch (kind) {
    case tk_opGT: return "GT";
    case tk_opLT: return "LT";
    case tk_opGE: return "GE";
    case tk_opLE: return "LE";
    case tk_opNE: return "NE";
    case tk_opEQ: return "EQ";
    default: return tokenkind_e_repr[kind];
    }
}

static bool tokenkind_e_isUnary(tokenkind_e kind) {
    static const uint8_t unop[sizeof(tokenkind_e_names)] = { //
        [tk_keyword_not] = 1,
        [tk_opUnaryMinus] = 1,
        [tk_unaryDot] = 1,
        [tk_keyword_return] = 1,
        [tk_arrayOpen] = 1,
        [tk_keyword_check] = 1,
        [tk_braceOpen] = 1
    };
    return unop[kind];
}

static bool tokenkind_e_isRightAssociative(tokenkind_e kind) {
    static const uint8_t rassoc[sizeof(tokenkind_e_names)] = { //
        [tk_period] = 1,
        [tk_opPower] = 1,
        [tk_opComma] = 1,
        [tk_opSemiColon] = 1
    };
    return rassoc[kind];
}

static uint8_t tokenkind_e_getPrecedence(tokenkind_e kind) {
    static const uint8_t prec[sizeof(tokenkind_e_names)] = { //
        [tk_period] = 57,
        [tk_unaryDot] = 57,
        [tk_opUnaryMinus] = 55,
        [tk_pipe] = 53,
        [tk_opPower] = 51,

        [tk_opTimes] = 49,
        [tk_opSlash] = 49,
        [tk_opMod] = 49,

        [tk_opPlus] = 47,
        [tk_opMinus] = 47,

        [tk_opColon] = 45,

        [tk_opLE] = 41,
        [tk_opLT] = 41,
        [tk_opGT] = 41,
        [tk_opGE] = 41,
        [tk_keyword_in] = 41,
        [tk_keyword_notin] = 41,

        [tk_opEQ] = 40,
        // [tk_tilde] = 40,
        // regex match op e.g. sIdent ~ '[a-zA-Z_][a-zA-Z0-9_]'
        [tk_opNE] = 40,

        [tk_keyword_not] = 32,
        [tk_keyword_and] = 31,
        [tk_keyword_or] = 30,

        [tk_keyword_check] = 25,
        [tk_keyword_return] = 25,

        [tk_opAssign] = 22,

        [tk_opPlusEq] = 20,
        [tk_opColEq] = 20,
        [tk_opMinusEq] = 20,
        [tk_opTimesEq] = 20,
        [tk_opSlashEq] = 20,

        [tk_opComma] = 10, // list separator
        [tk_opSemiColon] = 9, // 2-D array / matrix row separator

        [tk_keyword_do] = 5, // for i in arr do ...
        // [tk_keyword_then] = 5, // if i == x then ...

        [tk_arrayOpen] = 1,
        [tk_braceOpen] = 1
    };

    return prec[kind];
}

static tokenkind_e tokenkind_e_reverseBracket(tokenkind_e kind) {
    switch (kind) {
    case tk_arrayOpen: return tk_arrayClose;
    case tk_parenOpen: return tk_parenClose;
    case tk_braceOpen: return tk_braceClose;
    case tk_arrayClose: return tk_arrayOpen;
    case tk_braceClose: return tk_braceOpen;
    case tk_parenClose: return tk_parenOpen;
    default:
        printf("unexpected at %s:%d\n", __FILE__, __LINE__);
        return tk_unknown;
    }
}
