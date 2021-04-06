#include "TokenKindDefs.hpp"

static const char* TokenKind_repr(const TokenKind kind, bool spacing) {
    return spacing ? tksrepr[kind] : tkrepr[kind];
}

static const char* TokenKind_ascrepr(const TokenKind kind, bool spacing) {
    switch (kind) {
    case tkOpGT:
        return "GT";
    case tkOpLT:
        return "LT";
    case tkOpGE:
        return "GE";
    case tkOpLE:
        return "LE";
    case tkOpNE:
        return "NE";
    case tkOpEQ:
        return "EQ";
    default:
        return TokenKind_repr(kind, spacing);
    }
}

static bool TokenKind_isUnary(TokenKind kind) {
    static const uint8_t unop[sizeof(TokenKind_str)] = { [tkKeyword_not] = 1,
        [tkUnaryMinus] = 1,
        [tkKeyword_return] = 1,
        [tkArrayOpen] = 1,
        [tkKeyword_check] = 1,
        [tkBraceOpen] = 1 };
    return unop[kind];
}

static bool TokenKind_isRightAssociative(TokenKind kind) {
    static const uint8_t rassoc[sizeof(TokenKind_str)] = {
        [tkPeriod] = 1, [tkPower] = 1, [tkOpComma] = 1, [tkOpSemiColon] = 1
    };
    return rassoc[kind];
}

static uint8_t TokenKind_getPrecedence(TokenKind kind) {
    static const uint8_t prec[sizeof(TokenKind_str)] = { [tkPeriod] = 57,
        [tkUnaryMinus] = 55,
        [tkPipe] = 53,
        [tkPower] = 51,

        [tkTimes] = 49,
        [tkSlash] = 49,
        [tkOpMod] = 49,

        [tkPlus] = 47,
        [tkMinus] = 47,

        [tkOpColon] = 45,

        [tkOpLE] = 41,
        [tkOpLT] = 41,
        [tkOpGT] = 41,
        [tkOpGE] = 41,
        [tkKeyword_in] = 41,

        [tkOpEQ] = 40,
        [tkTilde] = 40,
        [tkOpNE] = 40,

        [tkKeyword_not] = 32,
        [tkKeyword_and] = 31,
        [tkKeyword_or] = 30,

        [tkKeyword_check] = 25,
        [tkKeyword_return] = 25,

        [tkOpAssign] = 22,

        [tkPlusEq] = 20,
        [tkColEq] = 20,
        [tkMinusEq] = 20,
        [tkTimesEq] = 20,
        [tkSlashEq] = 20,

        [tkOpComma] = 10,
        [tkOpSemiColon] = 9,

        [tkKeyword_do] = 5,

        [tkArrayOpen] = 1,
        [tkBraceOpen] = 1 };

    return prec[kind];
}

static TokenKind TokenKind_reverseBracket(TokenKind kind) {
    switch (kind) {
    case tkArrayOpen:
        return tkArrayClose;
    case tkParenOpen:
        return tkParenClose;
    case tkBraceOpen:
        return tkBraceClose;
    case tkArrayClose:
        return tkArrayOpen;
    case tkBraceClose:
        return tkBraceOpen;
    case tkParenClose:
        return tkParenOpen;
    default:
        printf("unexpected at %s:%d\n", __FILE__, __LINE__);
        return tkUnknown;
    }
}
