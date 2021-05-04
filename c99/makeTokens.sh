#!/bin/sh
count=0
start() {
    echo "\n// Autogenerated file, DO NOT EDIT.\n" > TokenKind.h
    echo "typedef enum TokenKind {" >> TokenKind.h
    echo "static const char* const TokenKind_repr[] = {" > TokenKind_repr.h
    echo "static const char* const TokenKind_srepr[] = {" > TokenKind_srepr.h
    echo "static const char* const TokenKind_names[] = {" > TokenKind_names.h
    echo " local tk = {" > ../lua/TokenKind.lua
}

finish() {
    echo "};\n" >> TokenKind_repr.h
    echo "};\n" >> TokenKind_srepr.h
    echo "};\n" >> TokenKind_names.h

    echo "}\nreturn tk\n" >> ../lua/TokenKind.lua

    echo "} TokenKind;\n" >> TokenKind.h
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
add tkNullChar "EOF"

add __tk__keywords__begin

    add __tk__ctrlflow__begin
        add tkKeyword_for "for "  "for"
        add tkKeyword_while "while "  "while"
        add tkKeyword_if "if " "if"
        add tkKeyword_match "match " "match"
        add tkKeyword_case "case " "case"
        add tkKeyword_else "else"
        add tkKeyword_elif "else if " "else if"
        add tkKeyword_do " do "
        add tkKeyword_then " then "
    add __tk__ctrlflow__end

    add tkKeyword_end "end"
    add tkKeyword_enum "enum " "enum"
    add tkKeyword_function "function"
    add tkKeyword_declare "declare"
    add tkKeyword_test "test "  "test"
    add tkKeyword_check "check "  "check"

    add __tk__logicals__begin
        add tkKeyword_not "not "
        add tkKeyword_and " and "
        add tkKeyword_or " or "
        add tkKeyword_in " in "
        add tkKeyword_notin " not in "
    add __tk__logicals__end

    add tkKeyword_yes "yes"
    add tkKeyword_no "no"
    add tkKeyword_nil "nil"
    add tkKeyword_as " as "
    add tkKeyword_type "type "
    add tkKeyword_return "return "
    add tkKeyword_result " result "
    add tkKeyword_extends " extends "
    add tkKeyword_var "var "
    add tkKeyword_let "let "
    add tkKeyword_import "import "
add __tk__keywords__end ""

add tkIdentifier "(id)"
add tkArgumentLabel "(lbl)"
add tkFunctionCall "(call)"
add tkSubscript "(sub)"
add tkObjectInit "(obj)"
add tkNumber "'123'"
add tkIdentifierResolved "[id]"
add tkFunctionCallResolved "[call]"
add tkSubscriptResolved "[sub]"
add tkObjectInitResolved "{obj}"
add tkNumberAsInt "123"
add tkNumberAsDbl "1.23"
add tkNumberAsUInt "+123"
add tkMultiDotNumber "1.2.3"
add tkSpaces "(spc)"
add tkOneSpace "(sp1)"
add tkTab "(tab)"
add tkNewline "(nl)"
add tkLineComment "#"
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
add tkExclamation "!"
add tkPipe "|"
add tkOpAssign " = " "="
add tkVarAssign "v=" "v="

add __tk__cmpOps__begin
    add tkOpEQ " == " "=="
    add tkOpNE " != " "!="
    add tkOpGE " >= " ">="
    add tkOpGT " > " ">"
    add tkOpLE " <= " "<="
    add tkOpLT " < " "<"
add __tk__cmpOps__end

add tkOpResults " -> "
add tkOpNotResults " -/> "
add tkParenClose ")"
add tkParenOpen "("
add tkPeriod "."
add tkOpComma ", "
add tkOpSemiColon "; "
add tkOpColon ":"
add tkStringBoundary "\\\""
add tkString "\\\"abc\\\""
add tkRawStringBoundary "'"
add tkRawString "'abc'"
add tkRegexpBoundary "\`"
add tkRegexp "\`abc\`"
add tkUnderscore "_"

add __tk__arithOps__begin
    add tkOpSlash " / " "/"
    add tkOpBackslash " \\\\\\ " "\\\\\\"
    add tkOpPlus " + " "+"
    add tkOpMinus " - " "-"
    add tkOpUnaryMinus " -" "-"
    add tkOpTimes " * " "*"
    add tkOpPower "^"
    add tkOpPowerEq " ^= "
    add tkOpMod " % " "%"
add __tk__arithOps__end

add tkTilde "~"
add tkDollar "$"
add tkUnits "|N.m/s"

add __tk__selfMutOps__begin
    add tkOpPlusEq " += "
    add tkOpMinusEq " -= "
    add tkOpSlashEq " /= "
    add tkOpTimesEq " *= "
    add tkOpColEq " := "
    add tkOpModEq " %= "
add __tk__selfMutOps__end

add tkOpQuestion "?"

add tkUnknown "(unk)"
finish
