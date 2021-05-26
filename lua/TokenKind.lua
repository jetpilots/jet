 local tk = {
    EOF = {id=1, name="EOF", repr="EOF"}, 
    __tk__keywords__begin = {id=2, name="__tk__keywords__begin", repr=""}, 
    __tk__ctrlflow__begin = {id=3, name="__tk__ctrlflow__begin", repr=""}, 
    For = {id=4, name="For", repr="for "}, 
    While = {id=5, name="While", repr="while "}, 
    If = {id=6, name="If", repr="if "}, 
    Match = {id=7, name="Match", repr="match "}, 
    Case = {id=8, name="Case", repr="case "}, 
    Else = {id=9, name="Else", repr="else"}, 
    Elif = {id=10, name="Elif", repr="else if "}, 
    Do = {id=11, name="Do", repr=" do "}, 
    Then = {id=12, name="Then", repr=" then "}, 
    __tk__ctrlflow__end = {id=13, name="__tk__ctrlflow__end", repr=""}, 
    End = {id=14, name="End", repr="end"}, 
    Enum = {id=15, name="Enum", repr="enum "}, 
    Func = {id=16, name="Func", repr="func"}, 
    Decl = {id=17, name="Decl", repr="decl"}, 
    Test = {id=18, name="Test", repr="test "}, 
    Check = {id=19, name="Check", repr="check "}, 
    __tk__logicals__begin = {id=20, name="__tk__logicals__begin", repr=""}, 
    Not = {id=21, name="Not", repr="not "}, 
    And = {id=22, name="And", repr=" and "}, 
    Or = {id=23, name="Or", repr=" or "}, 
    In = {id=24, name="In", repr=" in "}, 
    Notin = {id=25, name="Notin", repr=" not in "}, 
    __tk__logicals__end = {id=26, name="__tk__logicals__end", repr=""}, 
    Yes = {id=27, name="Yes", repr="yes"}, 
    No = {id=28, name="No", repr="no"}, 
    Nil = {id=29, name="Nil", repr="nil"}, 
    As = {id=30, name="As", repr=" as "}, 
    Type = {id=31, name="Type", repr="type "}, 
    Return = {id=32, name="Return", repr="return "}, 
    Break = {id=33, name="Break", repr="break "}, 
    Continue = {id=34, name="Continue", repr="continue "}, 
    Throw = {id=35, name="Throw", repr="throw "}, 
    Catch = {id=36, name="Catch", repr="catch "}, 
    Yield = {id=37, name="Yield", repr="yield "}, 
    Extends = {id=38, name="Extends", repr=" extends "}, 
    Var = {id=39, name="Var", repr="var "}, 
    Let = {id=40, name="Let", repr="let "}, 
    Import = {id=41, name="Import", repr="import "}, 
    __tk__keywords__end = {id=42, name="__tk__keywords__end", repr=""}, 
    Ident = {id=43, name="Ident", repr="(id)"}, 
    FuncCall = {id=44, name="FuncCall", repr="(call)"}, 
    Subscript = {id=45, name="Subscript", repr="(sub)"}, 
    ObjInit = {id=46, name="ObjInit", repr="(obj)"}, 
    Number = {id=47, name="Number", repr="'123'"}, 
    IdentR = {id=48, name="IdentR", repr="[id]"}, 
    FuncCallR = {id=49, name="FuncCallR", repr="[call]"}, 
    SubscriptR = {id=50, name="SubscriptR", repr="[sub]"}, 
    ObjInitR = {id=51, name="ObjInitR", repr="{obj}"}, 
    Spaces = {id=52, name="Spaces", repr="(spc)"}, 
    OneSpace = {id=53, name="OneSpace", repr="(sp1)"}, 
    Tab = {id=54, name="Tab", repr="(tab)"}, 
    EOL = {id=55, name="EOL", repr="(nl)"}, 
    Comment = {id=56, name="Comment", repr="~"}, 
    Alphabet = {id=57, name="Alphabet", repr="a"}, 
    Ampersand = {id=58, name="Ampersand", repr="&"}, 
    ArrayClose = {id=59, name="ArrayClose", repr="]"}, 
    ArrayOpen = {id=60, name="ArrayOpen", repr="["}, 
    ArrayDims = {id=61, name="ArrayDims", repr="[:]"}, 
    At = {id=62, name="At", repr="@"}, 
    BraceClose = {id=63, name="BraceClose", repr="}"}, 
    BraceOpen = {id=64, name="BraceOpen", repr="{"}, 
    Digit = {id=65, name="Digit", repr="1"}, 
    Hash = {id=66, name="Hash", repr="#"}, 
    UnaryDot = {id=67, name="UnaryDot", repr="."}, 
    EnumMember = {id=68, name="EnumMember", repr="#a"}, 
    Excl = {id=69, name="Excl", repr="!"}, 
    Pipe = {id=70, name="Pipe", repr="|"}, 
    Assign = {id=71, name="Assign", repr=" = "}, 
    VarDefn = {id=72, name="VarDefn", repr="v="}, 
    __tk__cmpOps__begin = {id=73, name="__tk__cmpOps__begin", repr=""}, 
    EQ = {id=74, name="EQ", repr=" == "}, 
    NE = {id=75, name="NE", repr=" != "}, 
    GE = {id=76, name="GE", repr=" >= "}, 
    GT = {id=77, name="GT", repr=" > "}, 
    LE = {id=78, name="LE", repr=" <= "}, 
    LT = {id=79, name="LT", repr=" < "}, 
    __tk__cmpOps__end = {id=80, name="__tk__cmpOps__end", repr=""}, 
    ParenClose = {id=81, name="ParenClose", repr=")"}, 
    ParenOpen = {id=82, name="ParenOpen", repr="("}, 
    Period = {id=83, name="Period", repr="."}, 
    Comma = {id=84, name="Comma", repr=", "}, 
    SemiColon = {id=85, name="SemiColon", repr="; "}, 
    Colon = {id=86, name="Colon", repr=":"}, 
    StringBoundary = {id=87, name="StringBoundary", repr="\""}, 
    String = {id=88, name="String", repr="\"abc\""}, 
    RawStringBoundary = {id=89, name="RawStringBoundary", repr="'"}, 
    RawString = {id=90, name="RawString", repr="'abc'"}, 
    RegexpBoundary = {id=91, name="RegexpBoundary", repr="`"}, 
    Regexp = {id=92, name="Regexp", repr="`abc`"}, 
    Underscore = {id=93, name="Underscore", repr="_"}, 
    __tk__arithOps__begin = {id=94, name="__tk__arithOps__begin", repr=""}, 
    Slash = {id=95, name="Slash", repr=" / "}, 
    Backslash = {id=96, name="Backslash", repr=" \\ "}, 
    Plus = {id=97, name="Plus", repr=" + "}, 
    Minus = {id=98, name="Minus", repr=" - "}, 
    UnaryMinus = {id=99, name="UnaryMinus", repr=" -"}, 
    Times = {id=100, name="Times", repr=" * "}, 
    Power = {id=101, name="Power", repr="^"}, 
    PowerEq = {id=102, name="PowerEq", repr=" ^= "}, 
    Mod = {id=103, name="Mod", repr=" % "}, 
    __tk__arithOps__end = {id=104, name="__tk__arithOps__end", repr=""}, 
    Tilde = {id=105, name="Tilde", repr="~"}, 
    Dollar = {id=106, name="Dollar", repr="$"}, 
    Units = {id=107, name="Units", repr="|N.m/s"}, 
    __tk__selfMutOps__begin = {id=108, name="__tk__selfMutOps__begin", repr=""}, 
    PlusEq = {id=109, name="PlusEq", repr=" += "}, 
    MinusEq = {id=110, name="MinusEq", repr=" -= "}, 
    SlashEq = {id=111, name="SlashEq", repr=" /= "}, 
    TimesEq = {id=112, name="TimesEq", repr=" *= "}, 
    ColEq = {id=113, name="ColEq", repr=" := "}, 
    ModEq = {id=114, name="ModEq", repr=" %= "}, 
    __tk__selfMutOps__end = {id=115, name="__tk__selfMutOps__end", repr=""}, 
    Question = {id=116, name="Question", repr="?"}, 
    Unknown = {id=117, name="Unknown", repr="(unk)"}, 
}
return tk
