#!/bin/sh
count=0
start() {
    printf "\n// Autogenerated file, DO NOT EDIT.\n" > TokenKind.h
    echo "typedef enum TokenKind {" >> TokenKind.h
    echo "static const char* const TokenKind_repr[] = {" > TokenKind_repr.h
    echo "static const char* const TokenKind_srepr[] = {" > TokenKind_srepr.h
    echo "static const char* const TokenKind_names[] = {" > TokenKind_names.h
    echo " local tk = {" > ../lua/TokenKind.lua
}

finish() {
    printf "};\n" >> TokenKind_repr.h
    printf "};\n" >> TokenKind_srepr.h
    printf "};\n" >> TokenKind_names.h

    printf "}\nreturn tk\n" >> ../lua/TokenKind.lua

    printf "} TokenKind;\n" >> TokenKind.h
    cat TokenKind_names.h TokenKind_repr.h TokenKind_srepr.h >> TokenKind.h
    rm TokenKind_names.h TokenKind_repr.h TokenKind_srepr.h
}

add() {
    srepr=${2}
    repr=${3:-$2}
    echo "    $1," >> TokenKind.h
    if [ -n "$srepr" ]; then
        echo "    [$1] = \"$1\"," >> TokenKind_names.h
        echo "    [$1] = \"$srepr\"," >> TokenKind_srepr.h
        echo "    [$1] = \"$repr\"," >> TokenKind_repr.h
    fi
    count=$((count+1))
    echo "    ${1##tk} = {id=$count, name=\"${1##tk}\", repr=\"$srepr\"}, " >> ../lua/TokenKind.lua
}

start
add tkEOF "EOF"

add __tk__keywords__begin

    add __tk__ctrlflow__begin
        add tkFor "for "  "for"
        add tkWhile "while "  "while"
        add tkIf "if " "if"
        add tkMatch "match " "match"
        add tkCase "case " "case"
        add tkElse "else"
        add tkElif "else if " "else if"
        add tkDo " do "
        add tkThen " then "
    add __tk__ctrlflow__end

    add tkEnd "end"
    add tkEnum "enum " "enum"
    add tkFunc "func"
    add tkDecl "decl"
    add tkTest "test "  "test"
    add tkCheck "check "  "check"

    add __tk__logicals__begin
        add tkNot "not " "not"
        add tkAnd " and " "and"
        add tkOr " or " "or"
        add tkIn " in " "in"
        add tkNotin " not in " "not in"
    add __tk__logicals__end

    add tkYes "yes"
    add tkNo "no"
    add tkNil "nil"
    add tkAs " as "
    add tkType "type "
    add tkReturn "return "
    add tkPrivate "private"
    add tkBreak "break "
    add tkContinue "continue "
    add tkThrow "throw "
    add tkCatch "catch "
    add tkYield "yield "
    add tkExtends " extends "
    add tkVar "var "
    add tkLet "let "
    add tkImport "import "
add __tk__keywords__end ""

add tkIdent "(id)"
# add tkArgumentLabel "(lbl)"
add tkFuncCall "(call)"
add tkSubscript "(sub)"
add tkObjInit "(obj)"
add tkNumber "'123'"
add tkIdentR "[id]"
add tkFuncCallR  "[call]"
add tkSubscriptR  "[sub]"
add tkObjInitR  "{obj}"
# add tkNumberAsInt "123"
# add tkNumberAsDbl "1.23"
# add tkNumberAsUInt "+123"
# add tkMultiDotNumber "1.2.3"
add tkSpaces "(spc)"
add tkOneSpace "(sp1)"
add tkTab "(tab)"
add tkEOL "(nl)"
add tkComment "~"
add tkAlphabet "a"
add tkAmpersand "&"
add tkArrayClose "]"
add tkArrayOpen "["
add tkArrayDims "[:]"
add tkAt "@"
add tkBraceClose "}"
add tkBraceOpen "{"
add tkDigit "1"
add tkHash "#"
add tkUnaryDot "."
add tkEnumMember "#a"
add tkExcl "!"
add tkPipe "|"
add tkVarDefn "v="
add tkArgAssign "a="

add __tk__cmpOps__begin
    add tkEQ " == " "=="
    add tkNE " != " "!="
    add tkGE " >= " ">="
    add tkGT " > " ">"
    add tkLE " <= " "<="
    add tkLT " < " "<"
add __tk__cmpOps__end

# add tkResults " -> "
# add tkNotResults " -/> "
add tkParenClose ")"
add tkParenOpen "("
add tkPeriod "."
add tkComma ", "
add tkSemiColon "; "
add tkColon ":"
add tkStringBoundary "\\\""
add tkString "\\\"abc\\\""
add tkRawStringBoundary "'"
add tkRawString "'abc'"
add tkRegexpBoundary "\`"
add tkRegexp "\`abc\`"
add tkUnderscore "_"

add __tk__arithOps__begin
    add tkSlash " / " "/"
    add tkBackslash " \\\\\\ " "\\\\\\"
    add tkPlus " + " "+"
    add tkMinus " - " "-"
    add tkUnaryMinus " -" "-"
    add tkTimes " * " "*"
    add tkPower "^"
    add tkPowerEq " ^= "
    add tkMod " % " "%"
add __tk__arithOps__end

add tkTilde "~"
add tkDollar "$"
add tkUnits "|N.m/s"

add __tk__selfMutOps__begin
    add tkAssign " = " "="
    add tkPlusEq " += " "+="
    add tkMinusEq " -= " "-="
    add tkSlashEq " /= " "/="
    add tkTimesEq " *= " "*="
    add tkColEq " := " ":="
    add tkModEq " %= " "%="
add __tk__selfMutOps__end

add tkQuestion "?"

add tkUnknown "(unk)"
finish
