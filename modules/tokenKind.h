#include "TokenKindDefs.h"

// Return the repr of a self->token kind (for debug)

static const char* TokenKind_repr(const TokenKind kind, bool spacing)
{
    return spacing ? tksrepr[kind] : tkrepr[kind];
}

// alternative ascii repr of a token kind
// you don't really need this, pass the original repr to a macro as param
// and the macro will deal with it. usually 3way_lt_le(a,b,c) can be same as
// 3way(<, <=, a, b, c) even for string e.g. < implies cmp(a,b) < 0
// TODO: get rid of this func

static const char* TokenKind_ascrepr(const TokenKind kind, bool spacing)
{
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

static bool TokenKind_isUnary(TokenKind kind)
{
    return kind == tkKeyword_not or kind == tkUnaryMinus
        or kind == tkKeyword_return or kind == tkArrayOpen
        or kind == tkKeyword_check or kind==tkBraceOpen;
    // tkArrayOpen is "unary" because it's EXPR is unary i.e.
    // it has one field `->right`, a list/dict literal expr
    // same goes for tkBraceOpen
}

static bool TokenKind_isRightAssociative(TokenKind kind)
{
    return kind == tkPeriod or kind == tkPower or kind == tkOpComma
        or kind == tkOpSemiColon;
}

static uint8_t TokenKind_getPrecedence(TokenKind kind)
{ // if templateStr then precedence of < and > should be 0
    // functions and subscripts are set to 60, so stay below that
    switch (kind) {
    case tkUnaryMinus:
        return 57;
    case tkPeriod:
        return 55;
    case tkPipe:
        return 53;
    case tkPower:
        return 51;
    case tkTimes:
    case tkSlash:
    case tkOpMod:
        return 49;
    case tkPlus:
    case tkMinus:
        return 47;
    case tkOpColon:
        return 45;
    case tkOpLE:
    case tkOpLT:
    case tkOpGT:
    case tkOpGE:
    case tkKeyword_in:
        return 41;
    case tkOpEQ:
    case tkTilde: // regex match op e.g. sIdent ~ '[a-zA-Z_][a-zA-Z0-9_]'
    case tkOpNE:
        return 40;
    case tkKeyword_not:
        return 32;
    case tkKeyword_and:
        return 31;
    case tkKeyword_or:
        return 30;
    case tkKeyword_check:
    case tkKeyword_return:
        return 25;
    case tkOpAssign:
        return 22;
    case tkPlusEq:
    case tkColEq:
    case tkMinusEq:
    case tkTimesEq:
    case tkSlashEq:
        return 20;
    case tkOpComma: // list separator
        return 10;
    case tkOpSemiColon: // 2-D array / matrix row separator
        return 9;
    case tkKeyword_do:
        return 5;
    case tkArrayOpen:
    case tkBraceOpen:
        return 1;
    default:
        return 0;
    }
}

static TokenKind TokenKind_reverseBracket(TokenKind kind)
{
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
