#include "TokenKinds.h"

// Return the repr of a self->token kind (for debug)

static const char* TokenKind_repr(const TokenKind kind, bool spacing) {
    return spacing ? TokenKinds_srepr[kind] : TokenKinds_repr[kind];
}

// alternative ascii repr of a token kind
// you don't really need this, pass the original repr to a macro as param
// and the macro will deal with it. usually 3way_lt_le(a,b,c) can be same as
// 3way(<, <=, a, b, c) even for string e.g. < implies cmp(a,b) < 0
// TODO: get rid of this func

static const char* TokenKind_ascrepr(const TokenKind kind, bool spacing) {
    switch (kind) {
    case tkOpGT: return "GT";
    case tkOpLT: return "LT";
    case tkOpGE: return "GE";
    case tkOpLE: return "LE";
    case tkOpNE: return "NE";
    case tkOpEQ: return "EQ";
    default: return TokenKind_repr(kind, spacing);
    }
}

static bool TokenKind_isUnary(TokenKind kind) {
    static const uint8_t unop[sizeof(TokenKinds_names)] = { //
        [tkKeyword_not] = 1,
        [tkOpUnaryMinus] = 1,
        [tkUnaryDot] = 1,
        [tkKeyword_return] = 1,
        [tkArrayOpen] = 1,
        [tkKeyword_check] = 1,
        [tkBraceOpen] = 1
    };
    return unop[kind];
    // return kind == tkKeyword_not //
    //     || kind == tkUnaryMinus //
    //     || kind == tkKeyword_return //
    //     || kind == tkArrayOpen //
    //     || kind == tkKeyword_check //
    //     || kind == tkBraceOpen;
    // tkArrayOpen is "unary" because it's EXPR is unary i.e.
    // it has one field `->right`, a list/dict literal expr
    // same goes for tkDictLiteral
    // TODO: remove dict/arr literals from here and in switches treat them
    // separately (not in default block)
}

static bool TokenKind_isRightAssociative(TokenKind kind) {
    static const uint8_t rassoc[sizeof(TokenKinds_names)] = { //
        [tkPeriod] = 1,
        [tkOpPower] = 1,
        [tkOpComma] = 1,
        [tkOpSemiColon] = 1
    };
    return rassoc[kind];
    // kind == tkPeriod //
    // || kind == tkPower //
    // || kind == tkOpComma //
    // || kind == tkOpSemiColon;
}

static uint8_t TokenKind_getPrecedence(TokenKind kind) {
    static const uint8_t prec[sizeof(TokenKinds_names)] = { //
        [tkPeriod] = 57,
        [tkUnaryDot] = 57,
        [tkOpUnaryMinus] = 55,
        [tkPipe] = 53,
        [tkOpPower] = 51,

        [tkOpTimes] = 49,
        [tkOpSlash] = 49,
        [tkOpMod] = 49,

        [tkOpPlus] = 47,
        [tkOpMinus] = 47,

        [tkOpColon] = 45,

        [tkOpLE] = 41,
        [tkOpLT] = 41,
        [tkOpGT] = 41,
        [tkOpGE] = 41,
        [tkKeyword_in] = 41,
        [tkKeyword_notin] = 41,

        [tkOpEQ] = 40,
        // [tkTilde] = 40,
        // regex match op e.g. sIdent ~ '[a-zA-Z_][a-zA-Z0-9_]'
        [tkOpNE] = 40,

        [tkKeyword_not] = 32,
        [tkKeyword_and] = 31,
        [tkKeyword_or] = 30,

        [tkKeyword_check] = 25,
        [tkKeyword_return] = 25,

        [tkOpAssign] = 22,

        [tkOpPlusEq] = 20,
        [tkOpColEq] = 20,
        [tkOpMinusEq] = 20,
        [tkOpTimesEq] = 20,
        [tkOpSlashEq] = 20,

        [tkOpComma] = 10, // list separator
        [tkOpSemiColon] = 9, // 2-D array / matrix row separator

        [tkKeyword_do] = 5, // for i in arr do ...
        // [tkKeyword_then] = 5, // if i == x then ...

        [tkArrayOpen] = 1,
        [tkBraceOpen] = 1
    };

    return prec[kind];
    // if templateStr then precedence of < and > should be 0
    // functions and subscripts are set to 60, so stay below that
    // switch (kind) {
    // case tkUnaryMinus:
    //     return 57;
    // case tkPeriod:
    //     return 55;
    // case tkPipe:
    //     return 53;
    // case tkPower:
    //     return 51;
    // case tkTimes:
    // case tkSlash:
    // case tkOpMod:
    //     return 49;
    // case tkPlus:
    // case tkMinus:
    //     return 47;
    // case tkOpColon:
    //     return 45;
    // case tkOpLE:
    // case tkOpLT:
    // case tkOpGT:
    // case tkOpGE:
    // case tkKeyword_in:
    //     return 41;
    // case tkOpEQ:
    // case tkTilde: // regex match op e.g. sIdent ~ '[a-zA-Z_][a-zA-Z0-9_]'
    // case tkOpNE:
    //     return 40;
    // case tkKeyword_not:
    //     return 32;
    // case tkKeyword_and:
    //     return 31;
    // case tkKeyword_or:
    //     return 30;
    // case tkKeyword_check:
    // case tkKeyword_return:
    //     return 25;
    // case tkOpAssign:
    //     return 22;
    // case tkPlusEq:
    // case tkColEq:
    // case tkMinusEq:
    // case tkTimesEq:
    // case tkSlashEq:
    //     return 20;
    // case tkOpComma: // list separator
    //     return 10;
    // case tkOpSemiColon: // 2-D array / matrix row separator
    //     return 9;
    // case tkKeyword_do:
    //     return 5;
    // case tkArrayOpen:
    // case tkBraceOpen:
    //     return 1;
    // default:
    //     return 0;
    // }
}

static TokenKind TokenKind_reverseBracket(TokenKind kind) {
    switch (kind) {
    case tkArrayOpen: return tkArrayClose;
    case tkParenOpen: return tkParenClose;
    case tkBraceOpen: return tkBraceClose;
    case tkArrayClose: return tkArrayOpen;
    case tkBraceClose: return tkBraceOpen;
    case tkParenClose: return tkParenOpen;
    default:
        printf("unexpected at %s:%d\n", __FILE__, __LINE__);
        return tkUnknown;
    }
}
