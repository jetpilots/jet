
// Autogenerated file, DO NOT EDIT.
typedef enum TokenKind {
    tkEOF,
    __tk__keywords__begin,
    __tk__ctrlflow__begin,
    tkFor,
    tkWhile,
    tkIf,
    tkMatch,
    tkCase,
    tkElse,
    tkElif,
    tkDo,
    tkThen,
    __tk__ctrlflow__end,
    tkEnd,
    tkEnum,
    tkFunc,
    tkDecl,
    tkTest,
    tkCheck,
    __tk__logicals__begin,
    tkNot,
    tkAnd,
    tkOr,
    tkIn,
    tkNotin,
    __tk__logicals__end,
    tkYes,
    tkNo,
    tkNil,
    tkAs,
    tkType,
    tkReturn,
    tkPrivate,
    tkBreak,
    tkContinue,
    tkThrow,
    tkCatch,
    tkYield,
    tkExtends,
    tkVar,
    tkLet,
    tkImport,
    __tk__keywords__end,
    tkIdent,
    tkFuncCall,
    tkSubscript,
    tkObjInit,
    tkNumber,
    tkIdentR,
    tkFuncCallR,
    tkSubscriptR,
    tkObjInitR,
    tkSpaces,
    tkOneSpace,
    tkTab,
    tkEOL,
    tkComment,
    tkAlphabet,
    tkAmpersand,
    tkArrayClose,
    tkArrayOpen,
    tkArrayDims,
    tkAt,
    tkBraceClose,
    tkBraceOpen,
    tkDigit,
    tkHash,
    tkUnaryDot,
    tkEnumMember,
    tkExcl,
    tkPipe,
    tkVarDefn,
    tkArgAssign,
    __tk__cmpOps__begin,
    tkEQ,
    tkNE,
    tkGE,
    tkGT,
    tkLE,
    tkLT,
    __tk__cmpOps__end,
    tkParenClose,
    tkParenOpen,
    tkPeriod,
    tkComma,
    tkSemiColon,
    tkColon,
    tkStringBoundary,
    tkString,
    tkRawStringBoundary,
    tkRawString,
    tkRegexpBoundary,
    tkRegexp,
    tkUnderscore,
    __tk__arithOps__begin,
    tkSlash,
    tkBackslash,
    tkPlus,
    tkMinus,
    tkUnaryMinus,
    tkTimes,
    tkPower,
    tkPowerEq,
    tkMod,
    __tk__arithOps__end,
    tkTilde,
    tkDollar,
    tkUnits,
    __tk__selfMutOps__begin,
    tkAssign,
    tkPlusEq,
    tkMinusEq,
    tkSlashEq,
    tkTimesEq,
    tkColEq,
    tkModEq,
    __tk__selfMutOps__end,
    tkQuestion,
    tkUnknown,
} TokenKind;
static const char* const TokenKind_names[] = {
    [tkEOF] = "tkEOF",
    [tkFor] = "tkFor",
    [tkWhile] = "tkWhile",
    [tkIf] = "tkIf",
    [tkMatch] = "tkMatch",
    [tkCase] = "tkCase",
    [tkElse] = "tkElse",
    [tkElif] = "tkElif",
    [tkDo] = "tkDo",
    [tkThen] = "tkThen",
    [tkEnd] = "tkEnd",
    [tkEnum] = "tkEnum",
    [tkFunc] = "tkFunc",
    [tkDecl] = "tkDecl",
    [tkTest] = "tkTest",
    [tkCheck] = "tkCheck",
    [tkNot] = "tkNot",
    [tkAnd] = "tkAnd",
    [tkOr] = "tkOr",
    [tkIn] = "tkIn",
    [tkNotin] = "tkNotin",
    [tkYes] = "tkYes",
    [tkNo] = "tkNo",
    [tkNil] = "tkNil",
    [tkAs] = "tkAs",
    [tkType] = "tkType",
    [tkReturn] = "tkReturn",
    [tkPrivate] = "tkPrivate",
    [tkBreak] = "tkBreak",
    [tkContinue] = "tkContinue",
    [tkThrow] = "tkThrow",
    [tkCatch] = "tkCatch",
    [tkYield] = "tkYield",
    [tkExtends] = "tkExtends",
    [tkVar] = "tkVar",
    [tkLet] = "tkLet",
    [tkImport] = "tkImport",
    [tkIdent] = "tkIdent",
    [tkFuncCall] = "tkFuncCall",
    [tkSubscript] = "tkSubscript",
    [tkObjInit] = "tkObjInit",
    [tkNumber] = "tkNumber",
    [tkIdentR] = "tkIdentR",
    [tkFuncCallR] = "tkFuncCallR",
    [tkSubscriptR] = "tkSubscriptR",
    [tkObjInitR] = "tkObjInitR",
    [tkSpaces] = "tkSpaces",
    [tkOneSpace] = "tkOneSpace",
    [tkTab] = "tkTab",
    [tkEOL] = "tkEOL",
    [tkComment] = "tkComment",
    [tkAlphabet] = "tkAlphabet",
    [tkAmpersand] = "tkAmpersand",
    [tkArrayClose] = "tkArrayClose",
    [tkArrayOpen] = "tkArrayOpen",
    [tkArrayDims] = "tkArrayDims",
    [tkAt] = "tkAt",
    [tkBraceClose] = "tkBraceClose",
    [tkBraceOpen] = "tkBraceOpen",
    [tkDigit] = "tkDigit",
    [tkHash] = "tkHash",
    [tkUnaryDot] = "tkUnaryDot",
    [tkEnumMember] = "tkEnumMember",
    [tkExcl] = "tkExcl",
    [tkPipe] = "tkPipe",
    [tkVarDefn] = "tkVarDefn",
    [tkArgAssign] = "tkArgAssign",
    [tkEQ] = "tkEQ",
    [tkNE] = "tkNE",
    [tkGE] = "tkGE",
    [tkGT] = "tkGT",
    [tkLE] = "tkLE",
    [tkLT] = "tkLT",
    [tkParenClose] = "tkParenClose",
    [tkParenOpen] = "tkParenOpen",
    [tkPeriod] = "tkPeriod",
    [tkComma] = "tkComma",
    [tkSemiColon] = "tkSemiColon",
    [tkColon] = "tkColon",
    [tkStringBoundary] = "tkStringBoundary",
    [tkString] = "tkString",
    [tkRawStringBoundary] = "tkRawStringBoundary",
    [tkRawString] = "tkRawString",
    [tkRegexpBoundary] = "tkRegexpBoundary",
    [tkRegexp] = "tkRegexp",
    [tkUnderscore] = "tkUnderscore",
    [tkSlash] = "tkSlash",
    [tkBackslash] = "tkBackslash",
    [tkPlus] = "tkPlus",
    [tkMinus] = "tkMinus",
    [tkUnaryMinus] = "tkUnaryMinus",
    [tkTimes] = "tkTimes",
    [tkPower] = "tkPower",
    [tkPowerEq] = "tkPowerEq",
    [tkMod] = "tkMod",
    [tkTilde] = "tkTilde",
    [tkDollar] = "tkDollar",
    [tkUnits] = "tkUnits",
    [tkAssign] = "tkAssign",
    [tkPlusEq] = "tkPlusEq",
    [tkMinusEq] = "tkMinusEq",
    [tkSlashEq] = "tkSlashEq",
    [tkTimesEq] = "tkTimesEq",
    [tkColEq] = "tkColEq",
    [tkModEq] = "tkModEq",
    [tkQuestion] = "tkQuestion",
    [tkUnknown] = "tkUnknown",
};
static const char* const TokenKind_repr[] = {
    [tkEOF] = "EOF",
    [tkFor] = "for",
    [tkWhile] = "while",
    [tkIf] = "if",
    [tkMatch] = "match",
    [tkCase] = "case",
    [tkElse] = "else",
    [tkElif] = "else if",
    [tkDo] = " do ",
    [tkThen] = " then ",
    [tkEnd] = "end",
    [tkEnum] = "enum",
    [tkFunc] = "func",
    [tkDecl] = "decl",
    [tkTest] = "test",
    [tkCheck] = "check",
    [tkNot] = "not",
    [tkAnd] = "and",
    [tkOr] = "or",
    [tkIn] = "in",
    [tkNotin] = "not in",
    [tkYes] = "yes",
    [tkNo] = "no",
    [tkNil] = "nil",
    [tkAs] = " as ",
    [tkType] = "type ",
    [tkReturn] = "return ",
    [tkPrivate] = "private",
    [tkBreak] = "break ",
    [tkContinue] = "continue ",
    [tkThrow] = "throw ",
    [tkCatch] = "catch ",
    [tkYield] = "yield ",
    [tkExtends] = " extends ",
    [tkVar] = "var ",
    [tkLet] = "let ",
    [tkImport] = "import ",
    [tkIdent] = "(id)",
    [tkFuncCall] = "(call)",
    [tkSubscript] = "(sub)",
    [tkObjInit] = "(obj)",
    [tkNumber] = "'123'",
    [tkIdentR] = "[id]",
    [tkFuncCallR] = "[call]",
    [tkSubscriptR] = "[sub]",
    [tkObjInitR] = "{obj}",
    [tkSpaces] = "(spc)",
    [tkOneSpace] = "(sp1)",
    [tkTab] = "(tab)",
    [tkEOL] = "(nl)",
    [tkComment] = "~",
    [tkAlphabet] = "a",
    [tkAmpersand] = "&",
    [tkArrayClose] = "]",
    [tkArrayOpen] = "[",
    [tkArrayDims] = "[:]",
    [tkAt] = "@",
    [tkBraceClose] = "}",
    [tkBraceOpen] = "{",
    [tkDigit] = "1",
    [tkHash] = "#",
    [tkUnaryDot] = ".",
    [tkEnumMember] = "#a",
    [tkExcl] = "!",
    [tkPipe] = "|",
    [tkVarDefn] = "v=",
    [tkArgAssign] = "a=",
    [tkEQ] = "==",
    [tkNE] = "!=",
    [tkGE] = ">=",
    [tkGT] = ">",
    [tkLE] = "<=",
    [tkLT] = "<",
    [tkParenClose] = ")",
    [tkParenOpen] = "(",
    [tkPeriod] = ".",
    [tkComma] = ", ",
    [tkSemiColon] = "; ",
    [tkColon] = ":",
    [tkStringBoundary] = "\"",
    [tkString] = "\"abc\"",
    [tkRawStringBoundary] = "'",
    [tkRawString] = "'abc'",
    [tkRegexpBoundary] = "`",
    [tkRegexp] = "`abc`",
    [tkUnderscore] = "_",
    [tkSlash] = "/",
    [tkBackslash] = "\\",
    [tkPlus] = "+",
    [tkMinus] = "-",
    [tkUnaryMinus] = "-",
    [tkTimes] = "*",
    [tkPower] = "^",
    [tkPowerEq] = " ^= ",
    [tkMod] = "%",
    [tkTilde] = "~",
    [tkDollar] = "$",
    [tkUnits] = "|N.m/s",
    [tkAssign] = "=",
    [tkPlusEq] = "+=",
    [tkMinusEq] = "-=",
    [tkSlashEq] = "/=",
    [tkTimesEq] = "*=",
    [tkColEq] = ":=",
    [tkModEq] = "%=",
    [tkQuestion] = "?",
    [tkUnknown] = "(unk)",
};
static const char* const TokenKind_srepr[] = {
    [tkEOF] = "EOF",
    [tkFor] = "for ",
    [tkWhile] = "while ",
    [tkIf] = "if ",
    [tkMatch] = "match ",
    [tkCase] = "case ",
    [tkElse] = "else",
    [tkElif] = "else if ",
    [tkDo] = " do ",
    [tkThen] = " then ",
    [tkEnd] = "end",
    [tkEnum] = "enum ",
    [tkFunc] = "func",
    [tkDecl] = "decl",
    [tkTest] = "test ",
    [tkCheck] = "check ",
    [tkNot] = "not ",
    [tkAnd] = " and ",
    [tkOr] = " or ",
    [tkIn] = " in ",
    [tkNotin] = " not in ",
    [tkYes] = "yes",
    [tkNo] = "no",
    [tkNil] = "nil",
    [tkAs] = " as ",
    [tkType] = "type ",
    [tkReturn] = "return ",
    [tkPrivate] = "private",
    [tkBreak] = "break ",
    [tkContinue] = "continue ",
    [tkThrow] = "throw ",
    [tkCatch] = "catch ",
    [tkYield] = "yield ",
    [tkExtends] = " extends ",
    [tkVar] = "var ",
    [tkLet] = "let ",
    [tkImport] = "import ",
    [tkIdent] = "(id)",
    [tkFuncCall] = "(call)",
    [tkSubscript] = "(sub)",
    [tkObjInit] = "(obj)",
    [tkNumber] = "'123'",
    [tkIdentR] = "[id]",
    [tkFuncCallR] = "[call]",
    [tkSubscriptR] = "[sub]",
    [tkObjInitR] = "{obj}",
    [tkSpaces] = "(spc)",
    [tkOneSpace] = "(sp1)",
    [tkTab] = "(tab)",
    [tkEOL] = "(nl)",
    [tkComment] = "~",
    [tkAlphabet] = "a",
    [tkAmpersand] = "&",
    [tkArrayClose] = "]",
    [tkArrayOpen] = "[",
    [tkArrayDims] = "[:]",
    [tkAt] = "@",
    [tkBraceClose] = "}",
    [tkBraceOpen] = "{",
    [tkDigit] = "1",
    [tkHash] = "#",
    [tkUnaryDot] = ".",
    [tkEnumMember] = "#a",
    [tkExcl] = "!",
    [tkPipe] = "|",
    [tkVarDefn] = "v=",
    [tkArgAssign] = "a=",
    [tkEQ] = " == ",
    [tkNE] = " != ",
    [tkGE] = " >= ",
    [tkGT] = " > ",
    [tkLE] = " <= ",
    [tkLT] = " < ",
    [tkParenClose] = ")",
    [tkParenOpen] = "(",
    [tkPeriod] = ".",
    [tkComma] = ", ",
    [tkSemiColon] = "; ",
    [tkColon] = ":",
    [tkStringBoundary] = "\"",
    [tkString] = "\"abc\"",
    [tkRawStringBoundary] = "'",
    [tkRawString] = "'abc'",
    [tkRegexpBoundary] = "`",
    [tkRegexp] = "`abc`",
    [tkUnderscore] = "_",
    [tkSlash] = " / ",
    [tkBackslash] = " \\ ",
    [tkPlus] = " + ",
    [tkMinus] = " - ",
    [tkUnaryMinus] = " -",
    [tkTimes] = " * ",
    [tkPower] = "^",
    [tkPowerEq] = " ^= ",
    [tkMod] = " % ",
    [tkTilde] = "~",
    [tkDollar] = "$",
    [tkUnits] = "|N.m/s",
    [tkAssign] = " = ",
    [tkPlusEq] = " += ",
    [tkMinusEq] = " -= ",
    [tkSlashEq] = " /= ",
    [tkTimesEq] = " *= ",
    [tkColEq] = " := ",
    [tkModEq] = " %= ",
    [tkQuestion] = "?",
    [tkUnknown] = "(unk)",
};
