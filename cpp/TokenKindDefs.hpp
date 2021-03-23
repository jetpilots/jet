

enum enum_TokenKind {
    tkNullChar,
    tkKeyword_cheater,
    tkKeyword_for,
    tkKeyword_while,
    tkKeyword_if,
    tkKeyword_end,
    tkKeyword_enum,
    tkKeyword_match,
    tkKeyword_case,
    tkKeyword_function,
    tkKeyword_declare,
    tkKeyword_test,
    tkKeyword_check,
    tkKeyword_not,
    tkKeyword_notin,
    tkKeyword_and,
    tkKeyword_yes,
    tkKeyword_no,
    tkKeyword_nil,
    tkKeyword_or,
    tkKeyword_in,
    tkKeyword_do,
    tkKeyword_then,
    tkKeyword_as,
    tkKeyword_else,
    tkKeyword_elif,
    tkKeyword_type,
    tkKeyword_return,
    tkKeyword_result,
    tkKeyword_extends,
    tkKeyword_var,
    tkKeyword_let,
    tkKeyword_import,
    tkIdentifier,
    tkArgumentLabel,
    tkFunctionCall,
    tkSubscript,
    tkObjectInit,
    tkNumber,
    tkIdentifierResolved,
    tkFunctionCallResolved,
    tkSubscriptResolved,
    tkObjectInitResolved,
    tkNumberAsInt,
    tkNumberAsDbl,
    tkNumberAsUInt,
    tkMultiDotNumber,
    tkSpaces,
    tkOneSpace,
    tkTab,
    tkNewline,
    tkLineComment,
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
    tkExclamation,
    tkPipe,
    tkOpAssign,
    tkVarAssign,
    tkOpEQ,
    tkOpNE,
    tkOpGE,
    tkOpGT,
    tkOpLE,
    tkOpLT,
    tkOpMod,
    tkOpModEq,
    tkOpResults,
    tkOpNotResults,
    tkParenClose,
    tkParenOpen,
    tkPeriod,
    tkOpComma,
    tkOpSemiColon,
    tkOpColon,
    tkStringBoundary,
    tkString,
    tkRawStringBoundary,
    tkRawString,
    tkRegexpBoundary,
    tkRegexp,
    tkUnderscore,
    tkSlash,
    tkBackslash,
    tkPlus,
    tkMinus,
    tkUnaryMinus,
    tkTimes,
    tkPower,
    tkPowerEq,
    tkTilde,
    tkDollar,
    tkUnits,
    tkUnknown,
    tkPlusEq,
    tkMinusEq,
    tkSlashEq,
    tkTimesEq,
    tkColEq,
    tkQuestion,
};

static const char* const TokenKind_str[] = {
    [tkNullChar] = "tkNullChar",
    [tkKeyword_cheater] = "tkKeyword_cheater",
    [tkKeyword_for] = "tkKeyword_for",
    [tkKeyword_while] = "tkKeyword_while",
    [tkKeyword_if] = "tkKeyword_if",
    [tkKeyword_end] = "tkKeyword_end",
    [tkKeyword_enum] = "tkKeyword_enum",
    [tkKeyword_match] = "tkKeyword_match",
    [tkKeyword_case] = "tkKeyword_case",
    [tkKeyword_function] = "tkKeyword_function",
    [tkKeyword_declare] = "tkKeyword_declare",
    [tkKeyword_test] = "tkKeyword_test",
    [tkKeyword_check] = "tkKeyword_check",
    [tkKeyword_not] = "tkKeyword_not",
    [tkKeyword_notin] = "tkKeyword_notin",
    [tkKeyword_and] = "tkKeyword_and",
    [tkKeyword_yes] = "tkKeyword_yes",
    [tkKeyword_no] = "tkKeyword_no",
    [tkKeyword_nil] = "tkKeyword_nil",
    [tkKeyword_or] = "tkKeyword_or",
    [tkKeyword_in] = "tkKeyword_in",
    [tkKeyword_do] = "tkKeyword_do",
    [tkKeyword_then] = "tkKeyword_then",
    [tkKeyword_as] = "tkKeyword_as",
    [tkKeyword_else] = "tkKeyword_else",
    [tkKeyword_elif] = "tkKeyword_elif",
    [tkKeyword_type] = "tkKeyword_type",
    [tkKeyword_return] = "tkKeyword_return",
    [tkKeyword_result] = "tkKeyword_result",
    [tkKeyword_extends] = "tkKeyword_extends",
    [tkKeyword_var] = "tkKeyword_var",
    [tkKeyword_let] = "tkKeyword_let",
    [tkKeyword_import] = "tkKeyword_import",
    [tkIdentifier] = "tkIdentifier",
    [tkArgumentLabel] = "tkArgumentLabel",
    [tkFunctionCall] = "tkFunctionCall",
    [tkSubscript] = "tkSubscript",
    [tkObjectInit] = "tkObjectInit",
    [tkNumber] = "tkNumber",
    [tkIdentifierResolved] = "tkIdentifierResolved",
    [tkFunctionCallResolved] = "tkFunctionCallResolved",
    [tkSubscriptResolved] = "tkSubscriptResolved",
    [tkObjectInitResolved] = "tkObjectInitResolved",
    [tkNumberAsInt] = "tkNumberAsInt",
    [tkNumberAsDbl] = "tkNumberAsDbl",
    [tkNumberAsUInt] = "tkNumberAsUInt",
    [tkMultiDotNumber] = "tkMultiDotNumber",
    [tkSpaces] = "tkSpaces",
    [tkOneSpace] = "tkOneSpace",
    [tkTab] = "tkTab",
    [tkNewline] = "tkNewline",
    [tkLineComment] = "tkLineComment",
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
    [tkExclamation] = "tkExclamation",
    [tkPipe] = "tkPipe",
    [tkOpAssign] = "tkOpAssign",
    [tkVarAssign] = "tkVarAssign",
    [tkOpEQ] = "tkOpEQ",
    [tkOpNE] = "tkOpNE",
    [tkOpGE] = "tkOpGE",
    [tkOpGT] = "tkOpGT",
    [tkOpLE] = "tkOpLE",
    [tkOpLT] = "tkOpLT",
    [tkOpMod] = "tkOpMod",
    [tkOpModEq] = "tkOpModEq",
    [tkOpResults] = "tkOpResults",
    [tkOpNotResults] = "tkOpNotResults",
    [tkParenClose] = "tkParenClose",
    [tkParenOpen] = "tkParenOpen",
    [tkPeriod] = "tkPeriod",
    [tkOpComma] = "tkOpComma",
    [tkOpSemiColon] = "tkOpSemiColon",
    [tkOpColon] = "tkOpColon",
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
    [tkTilde] = "tkTilde",
    [tkDollar] = "tkDollar",
    [tkUnits] = "tkUnits",
    [tkUnknown] = "tkUnknown",
    [tkPlusEq] = "tkPlusEq",
    [tkMinusEq] = "tkMinusEq",
    [tkSlashEq] = "tkSlashEq",
    [tkTimesEq] = "tkTimesEq",
    [tkColEq] = "tkColEq",
    [tkQuestion] = "tkQuestion",
};

static const char* const tkrepr[] = {
    [tkNullChar] = "EOF",
    [tkKeyword_cheater] = "cheater",
    [tkKeyword_for] = "for",
    [tkKeyword_while] = "while",
    [tkKeyword_if] = "if",
    [tkKeyword_end] = "end",
    [tkKeyword_enum] = "enum",
    [tkKeyword_match] = "match",
    [tkKeyword_case] = "case",
    [tkKeyword_function] = "function",
    [tkKeyword_declare] = "declare",
    [tkKeyword_test] = "test",
    [tkKeyword_check] = "check",
    [tkKeyword_not] = "not ",
    [tkKeyword_notin] = "not in ",
    [tkKeyword_and] = " and ",
    [tkKeyword_yes] = "yes",
    [tkKeyword_no] = "no",
    [tkKeyword_nil] = "nil",
    [tkKeyword_or] = " or ",
    [tkKeyword_in] = " in ",
    [tkKeyword_do] = " do ",
    [tkKeyword_then] = " then ",
    [tkKeyword_as] = " as ",
    [tkKeyword_else] = "else",
    [tkKeyword_elif] = "else if",
    [tkKeyword_type] = "type ",
    [tkKeyword_return] = "return ",
    [tkKeyword_result] = " result ",
    [tkKeyword_extends] = " extends ",
    [tkKeyword_var] = "var ",
    [tkKeyword_let] = "let ",
    [tkKeyword_import] = "import ",
    [tkIdentifier] = "(id)",
    [tkArgumentLabel] = "(lbl)",
    [tkFunctionCall] = "(call)",
    [tkSubscript] = "(sub)",
    [tkObjectInit] = "(obj)",
    [tkNumber] = "'123'",
    [tkIdentifierResolved] = "[id]",
    [tkFunctionCallResolved] = "[call]",
    [tkSubscriptResolved] = "[sub]",
    [tkObjectInitResolved] = "{obj}",
    [tkNumberAsInt] = "123",
    [tkNumberAsDbl] = "1.23",
    [tkNumberAsUInt] = "+123",
    [tkMultiDotNumber] = "1.2.3",
    [tkSpaces] = "(spc)",
    [tkOneSpace] = "(sp1)",
    [tkTab] = "(tab)",
    [tkNewline] = "(nl)",
    [tkLineComment] = "#",
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
    [tkExclamation] = "!",
    [tkPipe] = "|",
    [tkOpAssign] = "=",
    [tkVarAssign] = "v=",
    [tkOpEQ] = "==",
    [tkOpNE] = "!=",
    [tkOpGE] = ">=",
    [tkOpGT] = ">",
    [tkOpLE] = "<=",
    [tkOpLT] = "<",
    [tkOpMod] = "%",
    [tkOpModEq] = " %= ",
    [tkOpResults] = " -> ",
    [tkOpNotResults] = " -/> ",
    [tkParenClose] = ")",
    [tkParenOpen] = "(",
    [tkPeriod] = ".",
    [tkOpComma] = ", ",
    [tkOpSemiColon] = "; ",
    [tkOpColon] = ":",
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
    [tkTilde] = "~",
    [tkDollar] = "$",
    [tkUnits] = "|N.m/s",
    [tkUnknown] = "(unk)",
    [tkPlusEq] = " += ",
    [tkMinusEq] = " -= ",
    [tkSlashEq] = " /= ",
    [tkTimesEq] = " *= ",
    [tkColEq] = " := ",
    [tkQuestion] = "?",
};

static const char* const tksrepr[] = {
    [tkNullChar] = "EOF",
    [tkKeyword_cheater] = "cheater",
    [tkKeyword_for] = "for ",
    [tkKeyword_while] = "while ",
    [tkKeyword_if] = "if ",
    [tkKeyword_end] = "end",
    [tkKeyword_enum] = "enum ",
    [tkKeyword_match] = "match ",
    [tkKeyword_case] = "case ",
    [tkKeyword_function] = "function",
    [tkKeyword_declare] = "declare",
    [tkKeyword_test] = "test ",
    [tkKeyword_check] = "check ",
    [tkKeyword_not] = "not ",
    [tkKeyword_notin] = "not in ",
    [tkKeyword_and] = " and ",
    [tkKeyword_yes] = "yes",
    [tkKeyword_no] = "no",
    [tkKeyword_nil] = "nil",
    [tkKeyword_or] = " or ",
    [tkKeyword_in] = " in ",
    [tkKeyword_do] = " do ",
    [tkKeyword_then] = " then ",
    [tkKeyword_as] = " as ",
    [tkKeyword_else] = "else",
    [tkKeyword_elif] = "else if ",
    [tkKeyword_type] = "type ",
    [tkKeyword_return] = "return ",
    [tkKeyword_result] = " result ",
    [tkKeyword_extends] = " extends ",
    [tkKeyword_var] = "var ",
    [tkKeyword_let] = "let ",
    [tkKeyword_import] = "import ",
    [tkIdentifier] = "(id)",
    [tkArgumentLabel] = "(lbl)",
    [tkFunctionCall] = "(call)",
    [tkSubscript] = "(sub)",
    [tkObjectInit] = "(obj)",
    [tkNumber] = "'123'",
    [tkIdentifierResolved] = "[id]",
    [tkFunctionCallResolved] = "[call]",
    [tkSubscriptResolved] = "[sub]",
    [tkObjectInitResolved] = "{obj}",
    [tkNumberAsInt] = "123",
    [tkNumberAsDbl] = "1.23",
    [tkNumberAsUInt] = "+123",
    [tkMultiDotNumber] = "1.2.3",
    [tkSpaces] = "(spc)",
    [tkOneSpace] = "(sp1)",
    [tkTab] = "(tab)",
    [tkNewline] = "(nl)",
    [tkLineComment] = "#",
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
    [tkExclamation] = "!",
    [tkPipe] = "|",
    [tkOpAssign] = " = ",
    [tkVarAssign] = "v=",
    [tkOpEQ] = " == ",
    [tkOpNE] = " != ",
    [tkOpGE] = " >= ",
    [tkOpGT] = " > ",
    [tkOpLE] = " <= ",
    [tkOpLT] = " < ",
    [tkOpMod] = " % ",
    [tkOpModEq] = " %= ",
    [tkOpResults] = " -> ",
    [tkOpNotResults] = " -/> ",
    [tkParenClose] = ")",
    [tkParenOpen] = "(",
    [tkPeriod] = ".",
    [tkOpComma] = ", ",
    [tkOpSemiColon] = "; ",
    [tkOpColon] = ":",
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
    [tkTilde] = "~",
    [tkDollar] = "$",
    [tkUnits] = "|N.m/s",
    [tkUnknown] = "(unk)",
    [tkPlusEq] = " += ",
    [tkMinusEq] = " -= ",
    [tkSlashEq] = " /= ",
    [tkTimesEq] = " *= ",
    [tkColEq] = " := ",
    [tkQuestion] = "?",
};
