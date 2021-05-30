
// Return the repr of a self->token kind (for debug)

// static const char* TokenKind_repr(const TokenKind kind, bool spacing) {
//     return spacing ? TokenKinds_srepr[kind] : TokenKinds_repr[kind];
// }

// alternative ascii repr of a token kind
// you don't really need this, pass the original repr to a macro as param
// and the macro will deal with it. usually 3way_lt_le(a,b,c) can be same as
// 3way(<, <=, a, b, c) even for string e.g. < implies cmp(a,b) < 0
// TODO: get rid of this func

static const char* TokenKind_ascrepr(const TokenKind kind, bool spacing) {
  switch (kind) {
  case tkGT: return "GT";
  case tkLT: return "LT";
  case tkGE: return "GE";
  case tkLE: return "LE";
  case tkNE: return "NE";
  case tkEQ: return "EQ";
  default: return TokenKind_repr[kind];
  }
}

static bool TokenKind_isUnary(TokenKind kind) {
  static const uint8_t unop[sizeof(TokenKind_names)] = { //
    [tkNot] = 1,
    [tkUnaryMinus] = 1,
    [tkUnaryDot] = 1,
    [tkReturn] = 1,
    [tkYield] = 1,
    [tkBreak] = 1,
    [tkContinue] = 1,
    [tkThrow] = 1,
    [tkCatch] = 1,
    [tkArrayOpen] = 1,
    [tkArgAssign] = 1, // trick to ignore its left, which is an ident
    [tkCheck] = 1,
    [tkBraceOpen] = 1
  };
  return unop[kind];
}

static bool TokenKind_isRightAssociative(TokenKind kind) {
  static const uint8_t rassoc[sizeof(TokenKind_names)] = { //
    [tkPower] = 1,
    [tkComma] = 1,
    [tkSemiColon] = 1
  };
  return rassoc[kind];
}

static uint8_t TokenKind_getPrecedence(TokenKind kind) {
  static const uint8_t prec[sizeof(TokenKind_names)] = { //
    [tkPeriod] = 57,
    [tkUnaryDot] = 57,
    [tkUnaryMinus] = 55,
    [tkPipe] = 53,
    [tkPower] = 51,

    [tkTimes] = 49,
    [tkSlash] = 49,
    [tkMod] = 49,

    [tkPlus] = 47,
    [tkMinus] = 47,

    [tkColon] = 45,

    [tkLE] = 41,
    [tkLT] = 41,
    [tkGT] = 41,
    [tkGE] = 41,
    [tkIn] = 41,
    [tkNotin] = 41,

    [tkEQ] = 40,
    // [tkTilde] = 40,
    // regex match op e.g. sIdent ~ '[a-zA-Z_][a-zA-Z0-9_]'
    [tkNE] = 40,

    [tkNot] = 32,
    [tkAnd] = 31,
    [tkOr] = 30,

    [tkCheck] = 25,
    [tkReturn] = 25,
    [tkContinue] = 25,
    [tkBreak] = 25,
    [tkThrow] = 25,
    [tkYield] = 25,
    [tkCatch] = 25,

    [tkAssign] = 22,

    [tkPlusEq] = 20,
    [tkColEq] = 20,
    [tkMinusEq] = 20,
    [tkTimesEq] = 20,
    [tkSlashEq] = 20,

    [tkComma] = 10, // list separator
    [tkSemiColon] = 9, // 2-D array / matrix row separator

    [tkDo] = 5, // for i in arr do ...
    // [tkThen] = 5, // if i == x then ...

    [tkArrayOpen] = 1,
    [tkBraceOpen] = 1
  };

  return prec[kind];
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
