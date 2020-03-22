

// LINTER should work only on TOKENS! No need to build a tree for that.
// Right now however the linter generates the AST and dumps it back. This is
// more to understand and debug the AST generation -- the production linter
// should not do AST gen.
// NOTE that you need to generate AST for linting to do things like:
// - auto annotate var types when not obvious and remove when obvious
// - auto add function argument names at call sites / remove the first
// - change |kg/s[:,:] or [:,:]|kg/s into Number[:,:]|kg/s
// - sort imports
// - remove extra parentheses in exprs
// - fix array ranges :: -> :, 1:-1:1 -> :, ::::7::::9:: -> error
// - move types and their member functions together
// since files are expected to be small this ast-based linter should be OK?
// keep modules limited to 2000 lines, or even 1000
// BUT if errors are found, no formatting can be done. in this case, after
// errors are reported, run the token-based linter (formatter).

#include <cassert>
#include <cctype>
#include <climits>
#include <cstdio>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <cmath>

#pragma mark - Heap Allocation Extras

// Generally this is not to be done at runtime. But for now we can use it as
// a lightweight and builtin alternative to valgrind. Later focus on getting
// codegen right so that no leaks are possible.

// Use the ch- functions, if you use plain malloc nothing will be tracked.
/*
#ifdef CHMALLOC
#define chfree(ptr) f_chfree(ptr, __FILE__, __LINE__, __func__)
#define chmalloc(size) f_chmalloc(size, __FILE__, __LINE__, __func__)
#define chcalloc(size, count) f_chcalloc(size,count, __FILE__, __LINE__,
__func__) #define chrealloc(ptr, size) f_chrealloc(ptr, size, __FILE__,
__LINE__, __func__) #else #define chfree(ptr) free(ptr) #define
chmalloc(size) malloc(size) #define chcalloc(size, count) calloc(size,count)
#define chrealloc(ptr, size) realloc(ptr, size)
#endif

void f_chfree(void* ptr, const char* file, int line, const char* func)
{
    if (!ad_size.has(ptr)) {
        fprintf(stderr,"freeing unknown ptr in '%s' at %s line %d\n", func,
file, line); return;
    }
    aD_file.del(ptr);
    ad_func.del(ptr);
    ad_line.del(ptr);
    ad_size.del(ptr);
    free(ptr);
}

void* f_chmalloc(size_t size, const char* file, int line, const char* func )
{
    void* ret = malloc(size);
    if (!ret) {
        fprintf(stderr,"malloc failed in '%s' at %s line %d\n", func, file,
line); return NULL;
    }
    aD_file[ptr] = fname;
    ad_func[ptr] = func;
    ad_line[ptr] = line;
    ad_size[ptr] = size;
}
*/

#pragma mark - String Functions

// operator char*() { return str; }
char* str_noext(char* str)
{
    char* s = strdup(str);
    const size_t len = strlen(s);
    char* sc = s + len;
    while (sc > s and *sc != '.')
        sc--;
    if (sc >= s) *sc = '\0';
    return s;
}

char* str_base(char* str, char sep = '/', size_t len = 0)
{
    char* s = str;
    if (!len) len = strlen(s);
    char* sc = s + len;
    while (sc > s and sc[-1] != sep)
        sc--;
    if (sc >= s) s = sc;
    return s;
}

char* str_dir(char* str)
{
    char* s = strdup(str);
    const size_t len = strlen(s);
    char* sc = s + len;
    while (sc > s and *sc != '/')
        sc--;
    if (sc >= s) *sc = '\0';
    return s;
}

char* str_upper(char* str)
{
    char* s = strdup(str);
    char* sc = s - 1;
    while (*++sc)
        if (*sc >= 'a' and *sc <= 'z') *sc -= 32;
    return s;
}

void str_tr_ip(
    char* str, const char oldc, const char newc, const size_t length = 0)
{

    char* sc = str - 1;
    char* end = length ? str + length : (char*)0xFFFFFFFFFFFFFFFF;
    while (*++sc && sc < end)
        if (*sc == oldc) *sc = newc;
}

char* str_tr(char* str, const char oldc, const char newc)
{
    char* s = strdup(str);
    str_tr_ip(s, oldc, newc);
    return s;
}

#pragma mark - Number Types

template <class T> class Dual {
    T x, dx;
};
template <class T> class Complex {
    T re, im;
};
template <class T> class Reciprocal {
    // this can be 4B/8B, all others r pairs will be 8B/16B
    T d;
};
template <class T> class Rational {
    T n, d;
};
template <class T> class Interval {
    T hi, lo;
};
template <class T> class Point {
    T x, y;
};
template <class T> class Size {
    T w, h;
};
union Number {
    float f;
    uint32_t u;
    int32_t d;
    Reciprocal<float> rf;
    Reciprocal<uint32_t> ru;
    Reciprocal<int32_t> rd;
};
static_assert(sizeof(Number) == 4, "");

union NumberL {
    double F;
    uint64_t U;
    int64_t D;
    Reciprocal<double> rF;
    Reciprocal<uint64_t> rU;
    Reciprocal<int64_t> rD;
};
static_assert(sizeof(NumberL) == 8, "");

#pragma mark - Stack

template <class T> class Stack {
    T* items = NULL;
    uint32_t cap = 0;

    public:
    uint32_t count = 0;
    T& operator[](int index) { return items[index]; }
    ~Stack<T>()
    {
        if (cap) free(items);
    }
    void push(T node)
    {
        if (count < cap) {
            items[count++] = node;
        } else {
            cap = cap ? 2 * cap : 8;
            items = (T*)realloc(items, sizeof(T) * cap);
            // !! realloc can NULL the ptr!
            items[count++] = node;
            for (int i = count; i < cap; i++)
                items[i] = NULL;
        }
    }

    T pop()
    {
        T ret = NULL;
        if (count) {
            ret = items[count - 1];
            items[count - 1] = NULL;
            count--;
        } else {
            printf("error: pop from empty list\n");
        }
        return ret;
    }

    T top() { return count ? items[count - 1] : NULL; }

    bool empty() { return count == 0; }
};

#pragma mark - Pool

static size_t globalMemAllocBytes = 0;

template <class T> class Pool {
    T* ref = NULL;
    uint32_t cap = 0, elementsPerBlock = 512, total = 0;
    Stack<T*> ptrs;

    public:
    uint32_t count = elementsPerBlock;

    T* alloc()
    {
        if (count >= elementsPerBlock) {
            if (ref) ptrs.push(ref);
            ref = (T*)calloc(elementsPerBlock, sizeof(T));
            count = 0;
        }
        total++;
        globalMemAllocBytes += sizeof(T);
        return &ref[count++];
    }
    ~Pool<T>()
    {
        for (int j = 0; j < count; j++) {
            T* obj = &(ref[j]);
            obj->~T();
        }
        free(ref);

        int k = 0;
        for (int i = 0; i < ptrs.count; i++) {
            for (int j = 0; j < elementsPerBlock; j++) {
                if (++k >= total) break;
                T* obj = &(ptrs[i][j]);
                obj->~T();
            }
            free(ptrs[i]);
        }
    }
    static const char* _typeName()
    {
        const char* a = "Pool<";
        char* ret = strcat(a, T::_typeName());
        ret = strcat(ret, ">");
        return ret;
    }
    void stat()
    {
        fprintf(stderr, "*** %-16s %ld B x %3d = %ld B\n", T::_typeName(),
            sizeof(T), total, total * sizeof(T));
    }
};

#pragma mark - List

template <class T> class List {
    public:
    static Pool<List<T> > pool;
    void* operator new(size_t size) { return pool.alloc(); }

    T item = NULL;

    List<T>* next = NULL;

    List<T>() {}
    List<T>(T item) { this->item = item; }

    void append(T item)
    {  // adds a single item, creating a wrapping list item holder.
        if (!this->item)
            this->item = item;
        else
            append(List<T>(item));
    }

    void append(List<T> listItem)
    {
        // adds a list item, including its next pointer, effectively
        // concatenating this with listItem
        auto li = new List<T>;
        *li = listItem;
        List<T>* last = this;
        while (last->next)
            last = last->next;
        last->next = li;
    }
};
template <class T> Pool<List<T> > List<T>::pool;

#define foreach(var, listp, listSrc)                                       \
    auto listp = &(listSrc);                                               \
    for (auto var = listp->item; listp && (var = listp->item);             \
         listp = listp->next)

#pragma mark - Token Kinds

enum TokenKind {
    TKNullChar,
    TKKeyword_cheater,
    TKKeyword_for,
    TKKeyword_while,
    TKKeyword_if,
    TKKeyword_end,
    TKKeyword_function,
    TKKeyword_test,
    TKKeyword_not,
    TKKeyword_and,
    TKKeyword_or,
    TKKeyword_in,
    TKKeyword_do,
    TKKeyword_then,
    TKKeyword_as,
    TKKeyword_else,
    TKKeyword_type,
//    TKKeyword_print,
    TKKeyword_return,
    TKKeyword_base,
    TKKeyword_var,
    TKKeyword_let,
    TKKeyword_import,
//    TKKeyword_check,
    TKIdentifier,
    TKFunctionCall,
    TKSubscript,
    TKNumber,
    TKMultiDotNumber,
    //    TKWs,
    TKSpaces,
    TKOneSpace, // exactly 1 space
    TKTab,
    TKNewline,
    TKLineComment,
    TKAlphabet,
    TKAmpersand,
    TKArrayClose, // ]
    // TKArray_empty, // []
    TKArrayOpen, //  [
    TKArrayDims, // [:,:,:]
    TKAt,
    TKBraceClose,
    //    TKBrace_empty,
    TKBraceOpen,
    TKDigit,
    TKHash,
    TKExclamation,
    TKPipe,
    TKOpAssign,
    TKOpEQ,
    TKOpNE,
    TKOpGE,
    TKOpGT,
    TKOpLE,
    //    TKOpLeftShift,
    TKOpLT,
    TKOpMod,
    //    TKOpRightShift,
    TKOpResults,
    TKOpNotResults,
    TKParenClose,
    //    TKParen_empty,
    TKParenOpen,
    TKPeriod,
    TKOpComma,
    TKOpSemiColon,
    TKOpColon,
    TKStringBoundary, // "
    //    TKStr_empty, // ""
    TKString, // "string"
    TKRegexBoundary, // '
    //    TKRgx_empty, // ''
    TKRegex, // '[a-zA-Z0-9]+'
    TKInlineBoundary, // `
    //    TKInl_empty,
    TKInline, // `something`
    TKUnderscore,
    TKSlash,
    TKBackslash,
    TKPlus,
    TKMinus,
    TKUnaryMinus,
    // TKUnaryPlus, don't have this because it will promote stuff like
    // +-++-+5
    TKTimes,
    TKPower,
    TKDollar,
    TKUnits,
    TKUnknown,
    TKPlusEq,
    TKMinusEq,
    TKSlashEq,
    TKTimesEq,
    TKColEq,
    TKQuestion
};

// Return the repr of a token kind (for debug)
const char* TokenKind_repr(const TokenKind kind, bool spacing = true)
{
    switch (kind) {
        //    case TK_source:
        //        return "(src)";
    case TKNullChar:
        return "EOF";
    case TKKeyword_cheater:
        return "cheater";
    case TKKeyword_for:
        return "for";
    case TKKeyword_while:
        return "while";
    case TKKeyword_if:
        return "if";
    case TKKeyword_then:
        return "then";
    case TKKeyword_as:
        return "as";
    case TKKeyword_end:
        return "end";
    case TKKeyword_function:
        return "function";
    case TKKeyword_test:
        return "test";
    case TKKeyword_not:
        return "not ";
    case TKKeyword_and:
        return " and ";
    case TKKeyword_or:
        return " or ";
    case TKKeyword_in:
        return " in ";
    case TKKeyword_else:
        return "else";
    case TKKeyword_type:
        return "type";
//    case TKKeyword_check:
//        return "check";
    case TKKeyword_base:
        return "base";
    case TKKeyword_var:
        return "var";
    case TKKeyword_let:
        return "let";
    case TKKeyword_import:
        return "import";
//    case TKKeyword_print:
//        return "print";
    case TKKeyword_return:
        return "return ";
    case TKIdentifier:
        return "id";
    case TKFunctionCall:
        return "f()";
    case TKSubscript:
        return "a[]";
    case TKNumber:
        return "#";
        //    case TKWs:
        //        return "(ws)";
    case TKMultiDotNumber:
        return "#.";
    case TKSpaces:
        return "(spc)";
    case TKTab:
        return "(tab)";
    case TKNewline:
        return "(nl)";
    case TKLineComment:
        return "(cmt)";
    case TKAmpersand:
        return "&";
    case TKDigit:
        return "1";
    case TKPower:
        return "^";
    case TKUnits:
        return "|kg";
    case TKAlphabet:
        return "a";
    case TKArrayClose:
        return "]";
        //    case TKArrayEmpty:
        //        return "[]";
    case TKArrayOpen:
        return "[";
    case TKArrayDims:
        return "[:]";
    case TKAt:
        return "@";
    case TKBraceClose:
        return "}";
        //    case TKBrace_empty:
        //        return "{}";
    case TKBraceOpen:
        return "{";
    case TKHash:
        return "#";
    case TKExclamation:
        return "!";
    case TKPipe:
        return "|";
    case TKOpAssign:
        return " = ";
    case TKOpEQ:
        return spacing ? " == " : "==";
    case TKOpNE:
        return spacing ? " =/ " : "=/";
    case TKOpGE:
        return spacing ? " >= " : ">=";
    case TKOpGT:
        return spacing ? " > " : ">";
    case TKOpLE:
        return spacing ? " <= " : "<=";
        //    case TKOp_lsh:
        //        return "<<";
    case TKOpLT:
        return spacing ? " < " : "<";
    case TKOpMod:
        return spacing ? " % " : "%";
        //    case TKOpRightShift:
        //        return ">>";
    case TKOpResults:
        return " => ";
    case TKOpNotResults:
        return " =/> ";
    case TKParenClose:
        return ")";
        //    case TKParen_empty:
        //        return "()";
    case TKParenOpen:
        return "(";
    case TKPeriod:
        return ".";
    case TKOpComma:
        return ", ";
    case TKOpColon:
        return ":";
    case TKOpSemiColon:
        return "; ";
    case TKStringBoundary:
        return "\"";
        //    case TKStringEmpty:
        //        return "\"\"";
    case TKString:
        return "\"..\"";
    case TKRegexBoundary:
        return "'";
        //    case TKRegexEmpty:
        //        return "''";
    case TKRegex:
        return "(rgx)";
    case TKInlineBoundary:
        return "`";
        //    case TKInlineEmpty:
        //        return "``";
    case TKInline:
        return "`..`";
    case TKUnderscore:
        return "_";
    case TKSlash:
        return spacing ? " / " : "/";
    case TKBackslash:
        return "\\";
    case TKPlus:
        return spacing ? " + " : "+";
    case TKMinus:
        return spacing ? " - " : "-";
    case TKTimes:
        return spacing ? " * " : "*";
    case TKDollar:
        return "$";
    case TKUnknown:
        return "(?)";
    case TKKeyword_do:
        return "do";
    case TKColEq:
        return spacing ? " := " : ":=";
    case TKPlusEq:
        return spacing ? " += " : "+=";
    case TKMinusEq:
        return spacing ? " -= " : "-=";
    case TKTimesEq:
        return spacing ? " *= " : "*=";
    case TKSlashEq:
        return spacing ? " /= " : "/=";
    case TKOneSpace:
        return "(sp1)";
    case TKQuestion:
        return "?";
    case TKUnaryMinus:
        return "-"; // the previous op will provide spacing
    }
    printf("unknown kind: %d\n", kind);
    return "(!unk)";
}

bool TokenKind_isUnary(TokenKind kind)
{
    return kind == TKKeyword_not or kind == TKUnaryMinus
        or kind == TKKeyword_return
        or kind == TKArrayOpen; // unary - is handled separately
}

bool TokenKind_isRightAssociative(TokenKind kind)
{
    return kind == TKPeriod or kind == TKPower;
}

uint8_t TokenKind_getPrecedence(TokenKind kind)
{ // if templateStr then precedence of < and > should be 0
    switch (kind) {
    case TKUnaryMinus:
        return 105;
    case TKArrayOpen:
        return 1;
    case TKKeyword_return:
        return 25;
    case TKPeriod:
        return 90;
    case TKPipe:
        return 80;
    case TKPower:
        return 70;
    case TKTimes:
    case TKSlash:
        return 60;
    case TKPlus:
    case TKMinus:
        return 50;
    case TKOpColon:
        return 45;
    case TKOpLE:
    case TKOpLT:
    case TKOpGT:
    case TKOpGE:
    case TKKeyword_in:
        // case TKKw_notin:
        return 41;
    case TKOpEQ:
    case TKOpNE:
        return 40;
    case TKKeyword_not:
        return 32;
    case TKKeyword_and:
        return 31;
    case TKKeyword_or:
        return 30;
    case TKOpAssign:
        return 22;
    case TKPlusEq:
    case TKColEq:
    case TKMinusEq:
    case TKTimesEq:
    case TKSlashEq:
        return 20;
    case TKOpComma:
        return 10;
    case TKOpSemiColon:
        return 9;
    case TKKeyword_do:
        return 5;
        //    case TKParenOpen:
        //    case TKParenClose:
        //        return 0;
        //    case TKBraceOpen:
        //    case TKBraceClose:
        //        return 0;
        //    case TKArrayOpen:
        //    case TKArrayClose:
        //        return 0;
    default:
        return 0;
    }
}

TokenKind TokenKind_reverseBracket(TokenKind kind)
{
    switch (kind) {
    case TKArrayOpen:
        return TKArrayClose;
    case TKParenOpen:
        return TKParenClose;
    case TKBraceOpen:
        return TKBraceClose;
    case TKArrayClose:
        return TKArrayOpen;
    case TKBraceClose:
        return TKBraceOpen;
    case TKParenClose:
        return TKParenOpen;
    default:
        printf("unexpected at %s:%d\n", __FILE__, __LINE__);
        return TKUnknown;
    }
}

#pragma mark - Token

// Holds information about a syntax token.
class Token {
    public:
    static Pool<Token> pool;
    void* operator new(size_t size) { return pool.alloc(); }
    static const char* _typeName() { return "Token"; }

    char* pos = NULL;
    uint32_t matchlen : 24;
    struct {
        bool //
            skipWhiteSpace : 1, // skip whitespace
            mergeArrayDims : 1, // merge [:,:,:] into one token
            noKeywordDetect : 1, // leave keywords as idents
            strictSpacing : 1; // missing spacing around operators etc. is a
                               // compile error YES YOU HEARD IT RIGHT
                               // but why need this AND skipWhiteSpace?
    } flags = { true, false, false };
    uint16_t line = 1;
    uint8_t col = 1;
    TokenKind kind : 8; //= TKUnknown;

    Token()
        : kind(TKUnknown)
        , matchlen(0)
    {
    }
    const char* repr() { return TokenKind_repr(kind); }
    char* dup() const // const Token* const token)
    {
        return strndup(pos, matchlen);
    }

    // Advance the token position by one char.
    void advance1()
    {
        // this is called after a token has been consumed and the pointer
        // needs to advance. If skipws is set, loop until the next
        // non-space. do {
        pos++;

        // } while (token.skipws and *(token.pos) == ' ');
    }

    // Peek at the char after the current (complete) token
    char peekCharAfter()
    {
        char* s = pos + matchlen;
        if (flags.skipWhiteSpace)
            while (*s == ' ')
                s++;
        return *s;
    }

#define Token_compareKeyword(tok)                                          \
    if (sizeof(#tok) - 1 == l and not strncmp(#tok, s, l)) {               \
        kind = TKKeyword_##tok;                                            \
        return;                                                            \
    }

    // Check if an (ident) token matches a keyword and return its type
    // accordingly.
    void tryKeywordMatch()
    {
        if (flags.noKeywordDetect or kind != TKIdentifier) return; // kind;

        const char* s = pos;
        const int l = matchlen;

        Token_compareKeyword(and)
        Token_compareKeyword(cheater)
        Token_compareKeyword(for)
        Token_compareKeyword(do)
        Token_compareKeyword(while)
        Token_compareKeyword(if)
        Token_compareKeyword(then)
        Token_compareKeyword(end)
        Token_compareKeyword(function)
        Token_compareKeyword(test)
        Token_compareKeyword(not)
        Token_compareKeyword(and)
        Token_compareKeyword(or)
        Token_compareKeyword(in)
        Token_compareKeyword(else)
        Token_compareKeyword(type)
//        Token_compareKeyword(check)
        Token_compareKeyword(base)
        Token_compareKeyword(var)
        Token_compareKeyword(let)
        Token_compareKeyword(import)
        Token_compareKeyword(return)
        Token_compareKeyword(as)
//        Token_compareKeyword(print);
        // return TKIdentifier;
    }

    // Get the type based on the char at the current position.
    inline TokenKind gettype() { return getTypeAtOffset(0); }

    // Get the type based on the char after the current position (don't
    // conflate this with char* Token_peekCharAfter(token))
    inline TokenKind peek_nextchar() { return getTypeAtOffset(1); }
    //    inline TokenKind peek_prevchar() { return getTypeAtOffset(-1); }

    // Get the token kind based only on the char at the current position (or
    // an offset).
    TokenKind getTypeAtOffset(const size_t offset)
    {
        const char c = pos[offset];
        const char cn = c ? pos[1 + offset] : 0;

        switch (c) {
        case 0:
            return TKNullChar;
        case '\n':
            return TKNewline;
        case ' ':
            return TKSpaces;
        case '\t':
            return TKTab;
        case ':':
            return TKOpColon;
        case ';':
            return TKOpSemiColon;
        case ',':
            return TKOpComma;
        case '?':
            return TKQuestion;
        case '"':
            return TKStringBoundary;
        case '`':
            return TKInlineBoundary;
        case '[':
            // switch (cn) {
            // default:
            return TKArrayOpen;
            // }
        case '$':
            return TKDollar;
        case '%':
            return TKOpMod;
        case '.':
            return TKPeriod;
        case '\'':
            return TKRegexBoundary;
        case '&':
            return TKAmpersand;
        case '^':
            return TKPower;
        case '@':
            return TKAt;
        case '#':
            return TKHash;
        case '|':
            return TKPipe;
        case '{':
            // switch (cn) {
            // case '}':
            //     return TKBraceEmpty;
            // default:
            return TKBraceOpen;
            // }
        case '(':
            // switch (cn) {
            // case ')':
            //     return TKParenEmpty;
            // default:
            return TKParenOpen;
            // }
        case ')':
            return TKParenClose;
        case '}':
            return TKBraceClose;
        case ']':
            return TKArrayClose;
        case '<':
            switch (cn) {
            case '=':
                return TKOpLE;
                //        case '<':
                //            return TKOpLeftShift;
            default:
                return TKOpLT;
            }
        case '>':
            switch (cn) {
            case '=':
                return TKOpGE;
                //        case '>':
                //            return TKOp_rsh;
            default:
                return TKOpGT;
            }
        case '=':
            switch (cn) {
            case '=':
                return TKOpEQ;
            case '/':
                return TKOpNE;
            case '>':
                return TKOpResults;
            default:
                return TKOpAssign;
            }
        case '!':
            return TKExclamation;
        case '/':
            return TKSlash;
        case '\\':
            return TKBackslash;
        case '+':
            return TKPlus;
        case '-':
            return TKMinus; // if prev token was +-*/^(<> (and some
                            // others?) then this is unary -
        case '*':
            return TKTimes;
        default:
            if (isalpha(c) or c == '_') {
                return TKAlphabet;
            } else if (isdigit(c)) {
                return TKDigit;
            } else {
                fprintf(stderr,
                    "Unknown token starting with '%c' at %d:%d\n", *pos,
                    line, col);
                return TKUnknown;
            }
            break;
        }
    }

    // Scans ahead from the current position until the actual end of the
    // token.
    void detect()
    {
        TokenKind tt = gettype();
        TokenKind tt_ret = TKUnknown; // = tt;
        static TokenKind tt_last
            = TKUnknown; // the previous token that was found
        static TokenKind tt_lastNonSpace
            = TKUnknown; // the last non-space token found
        TokenKind tmp;
        char* start = pos;
        bool found_e = false, found_dot = false, found_cmt = false;
        uint8_t found_spc = 0;

        switch (tt) {
        case TKStringBoundary:
        case TKInlineBoundary:
        case TKRegexBoundary:
            tmp = tt; // remember which it is exactly

            // Incrementing pos is a side effect of gettype(...)
            while (tt != TKNullChar) {
                // here we want to consume the ending " so we move next
                // before
                advance1();
                tt = gettype();
                if (tt == TKNullChar or tt == tmp) {
                    advance1();
                    break;
                }
                if (tt == TKBackslash)
                    if (peek_nextchar() == tmp) { // why if?
                        advance1();
                    }
            }
            switch (tmp) {
            case TKStringBoundary:
                tt_ret = TKString;
                break;
            case TKInlineBoundary:
                tt_ret = TKInline;
                break;
            case TKRegexBoundary:
                tt_ret = TKRegex;
                break;
            default:
                tt_ret = TKUnknown;
                printf("unreachable %s:%d\n", __FILE__, __LINE__);
            }
            break;

        case TKSpaces:
            //            tt=peek_prevchar();

            if (tt_last == TKOneSpace) // if prev char was a space return
                                       // this as a run of spaces
                while (tt != TKNullChar) {
                    // here we dont want to consume the end char, so break
                    // before
                    tt = peek_nextchar();
                    advance1();
                    if (tt != TKSpaces) break;
                }
            else
                advance1();
            // else its a single space
            tt_ret = TKSpaces;
            break;

        case TKOpComma:
        case TKOpSemiColon:
//        line continuation tokens
            tt_ret = tt;

            while (tt != TKNullChar) {
                tt = peek_nextchar();
                advance1();
                // line number should be incremented for line continuations
                if (tt == TKSpaces) {
                    found_spc++;
                }
                if (tt == TKExclamation) {
                    found_cmt = true;
                }
                if (tt == TKNewline) {
                    line++;
                    col = -found_spc - 1; // account for extra spaces
                                          // after , and for nl itself
                    found_spc = 0;
                }
                if (found_cmt and tt != TKNewline) {
                    found_spc++;
                    continue;
                }
                if (tt != TKSpaces and tt != TKNewline) break;
            }
            break;
//        case TKOpAssign:
//        case TKParenOpen:
//        case TKArrayOpen:
//        case TKBraceOpen:
//            while (tt != TKNullChar) {
//                tt = peek_nextchar();
////                advance1();
//                // line number should be incremented for line continuations
//                if (tt == TKSpaces) {
//                    found_spc++;
//                }
//                if (tt == TKExclamation) {
//                    found_cmt = true;
//                }
//                if (tt == TKNewline) {
//                    line++;
//                    col = -found_spc - 1; // account for extra spaces
//                                          // after , and for nl itself
//                    found_spc = 0;
//                }
//                if (found_cmt and tt != TKNewline) {
//                    found_spc++;
//                    continue;
//                }
//                if (tt != TKSpaces and tt != TKNewline) break;
//                advance1();
//            }
//            if (tt_ret!=TKArrayOpen) break;

        case TKArrayOpen:
            // mergearraydims should be set only when reading func args
            if (not flags.mergeArrayDims) goto defaultToken;

            while (tt != TKNullChar) {
                tt = peek_nextchar();
                advance1();
                if (tt != TKOpColon and tt != TKOpComma) break;
            }
            tt = gettype();
            if (tt != TKArrayClose) {
                fprintf(stderr, "expected a ']', found a '%c'. now what?\n",
                    *pos);
            }
            advance1();
            tt_ret = TKArrayDims;
            break;

        case TKAlphabet:
        case TKPeriod:
        case TKUnderscore:
            while (tt != TKNullChar) {
                tt = peek_nextchar();
                advance1();
                if (tt != TKAlphabet //
                    and tt != TKDigit //
                    and tt != TKUnderscore and tt != TKPeriod)
                    break; /// validate in parser not here
            }
            tt_ret = TKIdentifier;
            break;

        case TKExclamation:
            while (tt != TKNullChar) {
                tt = peek_nextchar();
                advance1();
                if (tt == TKNewline) break;
            }
            tt_ret = TKLineComment;
            break;

        case TKPipe:
            while (tt != TKNullChar) {
                tt = peek_nextchar();
                advance1();
                if (tt != TKAlphabet and tt != TKDigit and tt != TKSlash
                    and tt != TKPeriod)
                    break;
            }
            tt_ret = TKUnits;
            break;

        case TKDigit:
            tt_ret = TKNumber;

            while (tt != TKNullChar) // EOF, basically null char
            {
                tt = peek_nextchar();
                // numbers such as 1234500.00 are allowed
                // very crude, error-checking is parser's job
                //            if (tt==TKDigit //
                //                or tt==TKPlus //
                //                or tt==TKMinus //
                //                or tt==TKPeriod //
                //                or *token.pos == 'e' //
                //                or *token.pos == 'E' //
                //                or *token.pos == 'd'
                //                or *token.pos == 'D')
                advance1();

                if (*pos == 'e' //
                    or *pos == 'E' //
                    or *pos == 'd'
                    or *pos == 'D') { // will all be changed to e btw
                    found_e = true;
                    continue;
                }
                if (found_e) {
                    //}and (*token.pos == '-' or *token.pos ==
                    //'+'))
                               //{
                    found_e = false;
                    continue;
                }
                if (tt == TKPeriod) {
                    found_dot = true;
                    continue;
                }
                if (found_dot and tt == TKPeriod)
                    tt_ret = TKMultiDotNumber;

                if (tt != TKDigit and tt != TKPeriod) break;
            }
            break;

        case TKMinus:

            switch (tt_lastNonSpace) {
            case TKParenClose:
            case TKIdentifier: // keywords too?
            case TKNumber:
            case TKArrayClose:
            case TKArrayDims:
            case TKMultiDotNumber:
                tt_ret = tt;
                break;
            default:
                tt_ret = TKUnaryMinus;
                break;
            }
            advance1();
            break;

        case TKOpNotResults:
            // 3-char tokens
            advance1();
        case TKOpEQ:
        case TKOpGE:
        case TKOpLE:
        case TKOpNE:
        case TKOpResults:
        case TKBackslash:
            // 2-char tokens
            advance1();
        default:
        defaultToken:
            tt_ret = tt;
            advance1();
            break;
        }

        matchlen = (uint32_t)(pos - start);
        pos = start; // rewind. but why! then again advance rewind
                     // advance rewind
        kind = tt_ret;
        // keywords have their own token type
        if (kind == TKIdentifier) tryKeywordMatch();
        // exactly one space is TKOnespc, otherwise TKSpc.
        // the compiler needs to check one space frequently in strict mode.
        // FIXME figure it out later
        if (kind == TKSpaces and matchlen == 1) kind = TKOneSpace;

        tt_last = kind;
        if (tt_last != TKOneSpace and tt_last != TKSpaces)
            tt_lastNonSpace = tt_last;
    }

    // Advance to the next token (skip whitespace if `skipws` is set).
    void advance()
    {
        switch (kind) {
        case TKIdentifier:
        case TKString:
        case TKNumber:
        case TKMultiDotNumber:
            //        case TKNewline:
        case TKFunctionCall:
        case TKSubscript:
        case TKDigit:
        case TKAlphabet:
        case TKRegex:
        case TKInline:
        case TKUnits:
        case TKKeyword_cheater:
        case TKKeyword_for:
        case TKKeyword_while:
        case TKKeyword_if:
        case TKKeyword_end:
        case TKKeyword_function:
        case TKKeyword_test:
        case TKKeyword_not:
        case TKKeyword_and:
        case TKKeyword_or:
        case TKKeyword_in:
        case TKKeyword_do:
        case TKKeyword_then:
        case TKKeyword_as:
        case TKKeyword_else:
        case TKKeyword_type:
//        case TKKeyword_print:
        case TKKeyword_return:
        case TKKeyword_base:
        case TKKeyword_var:
        case TKKeyword_let:
        case TKKeyword_import:
//        case TKKeyword_check:
        case TKUnknown: // bcz start of the file is this
            break;
        default:
            *pos = 0; // trample it so that idents etc. can be assigned
                      // in-situ
        }

        pos += matchlen;
        col += matchlen;
        matchlen = 0;
        detect();

        if (flags.skipWhiteSpace
            and (kind == TKSpaces
                or (flags.strictSpacing and kind == TKOneSpace)))
            advance();
        if (kind == TKNewline) {
            line++;
            col = 0; // position of the nl itself is 0
        }
    }
};

const char* const spaces =
    "                                                                     ";
const char* const dashes =
    "\n_________________________________________________________________\n";
const char* const equaltos =
    "\n=================================================================\n";

#pragma mark - AST Import

struct ASTImport {
    char* importFile;
    uint32_t aliasOffset;
    // generally this is not changed (the alias), at least not by the
    // linter. So we can only store the offset from importFile. In general
    // it would be nice to allocate the file contents buffer with a little
    // extra space and use that as a string pool. This way we can make new
    // strings while still referring to them using a uint32.
    bool isPackage = false, hasAlias = false;
    static Pool<ASTImport> pool;
    void* operator new(size_t size) { return pool.alloc(); }
    static const char* _typeName() { return "ASTImport"; }
    void gen(int level)
    {
        printf("import %s%s%s%s\n", isPackage ? "@" : "", importFile,
            hasAlias ? " as " : "",
            hasAlias ? importFile + aliasOffset : "");
    }
};

#pragma mark - AST Units

struct ASTUnits {
    static Pool<ASTUnits> pool;
    void* operator new(size_t size) { return pool.alloc(); }
    static const char* _typeName() { return "ASTUnits"; }

    uint8_t powers[7], something;
    double factor, factors[7];
    char* label;
    void gen(int level) { printf("|%s", label); }
};

struct ASTTypeSpec;
struct ASTType;
struct ASTFunc;
struct ASTScope;
struct ASTVar;

#pragma mark - AST Expr

struct ASTExpr {
    static Pool<ASTExpr> pool;
    void* operator new(size_t size) { return pool.alloc(); }

    static const char* _typeName() { return "ASTExpr"; }
    struct {
        uint16_t line = 0;
        union {
            struct {
                bool opIsUnary, opIsRightAssociative;
            };
            uint16_t strLength;
        }; // for literals, the string length
        uint8_t opPrecedence;
        uint8_t col = 0;
        TokenKind kind : 8; // TokenKind must be updated to add
                            // TKFuncCallResolved TKSubscriptResolved etc.
                            // to indicate a resolved identifier. while you
                            // are at it add TKNumberAsString etc. then
                            // instead of name you will use the corr.
                            // object. OR instead of extra enum values add a
                            // bool bit resolved
    };

    // Expr : left right next
    // Literals: value next
    // FuncCall : value args next

    union {
        ASTExpr *left = NULL;
    }; // for Expr, FuncCall and Subscript respectively

    union {
        union {
            char* string;
            double real;
            int64_t integer;
            uint64_t uinteger;
        } value; // for terminals
        char* name; // for unresolved functioncall or subscript, or
                    // identifiers
        ASTFunc* func = NULL; // for functioncall
        ASTVar* var; // for array subscript, or to refer to a variable
        ASTScope* body; // for if/for/while
        ASTExpr* right;
    };
    //    };

    ASTExpr() {}
    ASTExpr(const Token* token)
    {
        kind = token->kind;
        line = token->line;
        col = token->col;

        opPrecedence = TokenKind_getPrecedence(kind);
        if (opPrecedence) {
            opIsRightAssociative = TokenKind_isRightAssociative(kind);
            opIsUnary = TokenKind_isUnary(kind);
        }
        switch (kind) {
        case TKIdentifier:
            strLength = (uint16_t)token->matchlen;
            name = token->pos; // dup();
            break;
        case TKString:
        case TKRegex:
        case TKNumber:
        case TKMultiDotNumber:
            strLength = (uint16_t)token->matchlen;
            value.string = token->pos; // dup();
            break;
        default:;
            // what else? errror
        }
        if (kind == TKNumber) {
            // turn all 1.0234[DdE]+01 into 1.0234e+01.
            // right now this is not null terminated. actually this should
            // be done in a later phase anyway. but if you want to do it
            // now, str_tr_ip should take the max length.
            str_tr_ip(value.string, 'd', 'e', strLength);
            str_tr_ip(value.string, 'D', 'e', strLength);
            str_tr_ip(value.string, 'E', 'e', strLength);
        }
    }

    void gen(int level = 0, bool spacing = true)
    {
        // generally an expr is not split over several lines (but maybe in
        // rare cases). so level is not passed on to recursive calls.
        printf("%.*s", level * 4, spaces);

        switch (kind) {
        case TKIdentifier:
        case TKNumber:
        case TKMultiDotNumber:
        case TKString:
            printf("%.*s", strLength, value.string);
            break;

        case TKFunctionCall:
            printf("%.*s(", strLength, name);
            if (left) left->gen(0, false);
            printf(")");
            break;

        case TKSubscript:
            printf("%.*s[", strLength, name);
            if (left) left->gen(0, false);
            printf("]");
            break;

        default:
            if (not opPrecedence) break;
            // not an operator, but this should be error if you reach here
            bool leftBr = left and left->opPrecedence
                and left->opPrecedence < this->opPrecedence;
            bool rightBr = right and right->opPrecedence
                and right->kind != TKKeyword_return // found in 'or return'
                and right->opPrecedence < this->opPrecedence;

            if (kind == TKOpColon) {
                // expressions like arr[a:x-3:2] should become
                // arr[a:(x-3):2]
                // or list literals [8, 9, 6, 77, sin(c)]
                if (left) switch (left->kind) {
                    case TKNumber:
                    case TKIdentifier:
                    case TKString:
                    case TKOpColon:
                    case TKMultiDotNumber:
                    case TKUnaryMinus:
                        break;
                    default:
                        leftBr = true;
                    }
                if (right) switch (right->kind) {
                    case TKNumber:
                    case TKIdentifier:
                    case TKString:
                    case TKOpColon:
                    case TKMultiDotNumber:
                    case TKUnaryMinus:
                        break;
                    default:
                        rightBr = true;
                    }
            }
            // the above should also happen for TKPower with primitives on
            // both sides && when spacing = false

            //            else if (kind==TKPower) {
            //                rightBr = right;
            //                leftBr = left;
            //            }
            if (false and kind == TKKeyword_return and right) {
                switch (right->kind) {
                case TKString:
                case TKNumber:
                case TKIdentifier:
                case TKFunctionCall:
                case TKSubscript:
                case TKRegex:
                case TKMultiDotNumber:
                    break;
                default:
                    rightBr = true;
                    break;
                }
            } // right may be null if its empty return
            //             {
            if (kind == TKPower and not spacing) putc('(', stdout);
            //            if (level and (kind == TKOpComma or
            //            kind==TKOpSemiColon)) putc('[', stdout);

            char lpo = leftBr and left->kind == TKOpColon ? '[' : '(';
            char lpc = leftBr and left->kind == TKOpColon ? ']' : ')';
            if (leftBr) putc(lpo, stdout);
            if (left)
                left->gen(0, spacing and !leftBr and kind != TKOpColon);
            if (leftBr) putc(lpc, stdout);
            //            }
            // TODO: need a way to have compact and full repr for the same
            // token e.g. (a + b) but x[a+b:-1]
            printf("%s", TokenKind_repr(kind, spacing));
            //             {
            char rpo = rightBr and right->kind == TKOpColon ? '[' : '(';
            char rpc = rightBr and right->kind == TKOpColon ? ']' : ')';
            if (rightBr) putc(rpo, stdout);
            if (right)
                right->gen(0, spacing and !rightBr and kind != TKOpColon);
            if (rightBr) putc(rpc, stdout);

            if (kind == TKPower and not spacing) putc(')', stdout);
            if (kind == TKArrayOpen)
                putc(']', stdout); // close a list comprehension / literal
                                   // [1, 2, 3] etc.
            //            if (level and (kind == TKOpComma or
            //            kind==TKOpSemiColon)) putc(']', stdout);

            //            }
        }
    }
}; // how about if, for, etc. all impl using ASTExpr?

#pragma mark - AST TypeSpec

struct ASTTypeSpec {
    static Pool<ASTTypeSpec> pool;
    void* operator new(size_t size) { return pool.alloc(); }
    static const char* _typeName() { return "ASTTypeSpec"; }

    // char* typename; // use name
    union {
        ASTType* type;
        char* name = NULL;
        ASTUnits* units;
        // you know that if this is set, then type can only be Number anyway
    };

    uint32_t dims = 0;
    enum TypeSpecStatus {
        TSUnresolved, // name ptr is set
        TSResolved, // type ptr is set and points to the type
        TSDimensionedNumber // type can only be Number, units ptr is set
        // if more (predefined) number types can use units, add them here
    };
    TypeSpecStatus status = TSUnresolved;
    // if false, name is set, else type is set

    void gen(int level = 0)
    {
        if (status == TSUnresolved) printf("%s", name);
        //        if (status==TSResolved) printf("%s", type->name);
        if (dims) printf("%s", dimsGenStr(dims));
        if (status == TSDimensionedNumber) units->gen(level);
    }
    //
    // bool dimsvalid(char* dimsstr) {
    //    bool valid = true;
    //    char* str=dimsstr;
    //    valid = valid and (*str++ == '[');
    //    while (*str != ']' and *str != 0) {
    //        valid = valid and (*str++ == ':');
    //        if (*str==',') {
    //            valid = valid and (*str++ == ',');
    //            valid = valid and (*str++ == ' ');
    //        }
    //    }
    //    return valid;
    //}
    //    int32_t dimsCount(char* dimsstr)
    //    {
    //        int32_t count = 0;
    //        char* str = dimsstr;
    //        while (*str)
    //            if (*str++ == ':' or *str == '[') count++;
    //        return count;
    //    }

    const char* dimsGenStr(int32_t dims)
    {
        switch (dims) {
        case 0:
            return "";
        case 1:
            return "[]";
        case 2:
            return "[:,:]";
        case 3:
            return "[:,:,:]";
        case 4:
            return "[:,:,:,:]";
        case 5:
            return "[:,:,:,:,:]";
        case 6:
            return "[:,:,:,:,:,:]";
        default:
            int32_t i;
            int32_t sz = 2 + dims + (dims ? (dims - 1) : 0) + 1;
            char* str = (char*)malloc(sz * sizeof(char));
            str[sz * 0] = '[';
            str[sz - 1] = 0;
            for (i = 0; i < dims; i++) {
                str[i * 2 + 1] = ':';
                str[i * 2 + 2] = ',';
            }
            str[sz - 2] = ']';
            return str;
        }
    }
};

#pragma mark - AST Var

struct ASTVar {
    static Pool<ASTVar> pool;
    void* operator new(size_t size) { return pool.alloc(); }
    static const char* _typeName() { return "ASTVar"; }

    ASTTypeSpec* typeSpec = NULL;
    ASTExpr* init = NULL;
    char* name = NULL;

    struct {
        bool unused : 1, //
            unset : 1, //
            isLet : 1, //
            isVar : 1, //
            isTarget : 1;
        /* x = f(x,y) */ //
    } flags;

    void gen(int level = 0)
    {
        printf("%.*s%s%s", level * 4, spaces,
            flags.isVar ? "var " : flags.isLet ? "let " : "", name);
        if (typeSpec) {
            printf(" as ");
            typeSpec->gen(level + 1);
        } else
            printf(" as Unknown");
        if (init) {
            printf(" = ");
            init->gen(0);
        }
        //        puts("");
    }
};

#pragma mark - AST NodeRef

class ASTNodeRef {
    union {
        struct {
            uint64_t typ : 3, _danger : 45, _unused : 16;
        };
        uintptr_t uiptr;
        void* ptr = NULL;
    };

    public:
    enum NodeKind { NKVar, NKExpr, NKFunc, NKType, NKModule, NKTest };

    ASTNodeRef() {}

    ASTNodeRef(ASTVar* varNode)
    {
        ptr = varNode;
        typ = NKVar;
    }

    ASTNodeRef(ASTExpr* exprNode)
    {
        ptr = exprNode;
        typ = NKExpr;
    }
    inline uintptr_t getptr() { return (uiptr & 0xFFFFFFFFFFFFFFF8); }
    uint8_t kind() { return typ; } // this only takes values 0-7, 3 bits
    //--
    ASTVar* var() { return (ASTVar*)getptr(); }
    ASTExpr* expr() { return (ASTExpr*)getptr(); }
};

#pragma mark - AST Type

struct ASTType {
    static Pool<ASTType> pool;
    void* operator new(size_t size) { return pool.alloc(); }
    static const char* _typeName() { return "ASTType"; }

    List<ASTVar*> vars; // vars contained in this type
    ASTTypeSpec* super = NULL;
    char* name = NULL;

    List<ASTExpr*>
        checks; // invariants
                // make sure each Expr has null next. If not, split the
                // Expr into two and add one after the other. If you
                // will add automatic checks based on expressions
                // elsewhere, clone them and set null next, because we
                // need next to build this list here.
    List<ASTVar*>
        params; // params of this type (if generic type / template)

    void gen(int level = 0)
    {
        printf("type %s\n", name);
        // gen params
        if (super) {
            printf("    base ");
            super->gen(level);
            puts("");
        } // %s\n", node->type.super);
        //        ASTNode* member = node->type.members;
        //        auto vars = &(this->vars);
        //        if (vars)
        //            for (auto var = vars->item;
        //                 vars;
        //                 vars = vars->next,
        //                 var = vars->item)
        //        if (this->vars.item) {
        //            auto vars = &(this->vars);
        ////            if (vars)
        //                for (auto var = vars->item;
        //                     vars && (var = vars->item);
        //                     vars = vars->next
        //                     )

        foreach (var, vars, this->vars) {
            if (!var) continue;
            var->gen(level + 1);
            puts("");
        }
        //        }
        //        if (this->checks.item) {
        foreach (check, checks, this->checks) {
            if (!check) continue;
            check->gen(level + 1);
            puts("");
        }
        //        }
        puts("end type\n");
    }
};

// typedef struct {
//    union {
//        ASTExpr* expr;
//        ASTVar* var;
//    };
//    struct ASTStmt* next; // Expr has its own next... so clean this up
//} ASTStmt;

#pragma mark - AST Scope

struct ASTScope {
    static Pool<ASTScope> pool;
    void* operator new(size_t size) { return pool.alloc(); }
    static const char* _typeName() { return "ASTScope"; }

    List<ASTExpr*> stmts;
    List<ASTVar*> locals;
    ASTScope* parent = NULL; // fixme: can be type, func, or scope

    void gen(int level)
    {
        // List<ASTExpr*> stmts = this->stmts;
        // ASTExpr* stmt;
        foreach (local, locals, this->locals) {
            //            if (!stmt) continue;
            local->gen(level);
            puts("");
        }
        foreach (stmt, stmts, this->stmts) {
            //            if (!stmt) continue;
            stmt->gen(level);
            puts("");

        } // while((stmts = *(stmts.next)));
    }
};

#pragma mark - AST Func

struct ASTFunc {
    static Pool<ASTFunc> pool;
    void* operator new(size_t size) { return pool.alloc(); }
    static const char* _typeName() { return "ASTFunc"; }

    ASTScope* body;
    List<ASTVar*> args;
    ASTTypeSpec* returnType;
    char* mangledName;
    char* owner; // if method of a type
    char* name;
    struct {
        uint16_t line;
        struct {
            uint16_t prints : 1, //
                throws : 1, //
                recurs : 1, //
                net : 1, //
                gui : 1, //
                file : 1, //
                refl : 1, //
                nodispatch : 1;
        } flags;
        uint8_t col;
    };

    void gen(int level = 0)
    {
        printf("function %s(", name);
        //        List<ASTVar*> args = this->args;
        //        ASTVar* arg = NULL;
        //        do {
        //            if (!(arg = args.item)) continue; // embedded nulls
        //            arg->gen(level);
        //            printf(args.next ? ", " : ")");
        //            args = *(args.next);
        //        } while (args.next);

        //        List<ASTVar*>* args = this->args;
        //        ASTVar* arg = NULL;
        //        do {
        //            if (!(arg = args.item)) continue; // embedded nulls
        //            arg->gen(level);
        //            printf(args.next ? ", " : ")");
        //            args = (args.next);
        //        } while (args.next);

        foreach (arg, args, this->args) {
            //            if (!arg) continue;
            arg->gen(level);
            printf(args->next ? ", " : ")");
        }

        if (returnType) {
            printf(" as ");
            returnType->gen(level);
        }
        puts("");
        body->gen(level + 1);
        puts("end function\n");
    }
};

#pragma mark - AST Module

struct ASTModule {
    static Pool<ASTModule> pool;
    void* operator new(size_t size) { return pool.alloc(); }
    static const char* _typeName() { return "ASTModule"; }

    List<ASTFunc*> funcs;
    List<ASTExpr*> exprs;
    List<ASTType*> types;
    List<ASTVar*> globals;
    List<ASTImport*> imports;
    List<ASTFunc*> tests;
    char* name;
    char* moduleName = NULL; // mod.submod.xyz.mycode
    char* mangledName = NULL; // mod_submod_xyz_mycode
    char* capsMangledName = NULL; // MOD_SUBMOD_XYZ_MYCODE
    void gen(int level = 0)
    {
        printf("! module %s\n", name);

        foreach (import, imports, this->imports) {
            import->gen(level);
        }
        puts("");

        // nope you will mess up the order
        //        List<ASTType*> types = this->types;
        //        ASTType* type;
        //        do
        //        if (this->types.item) {
        foreach (type, types, this->types) {
            //                if (!type) continue;
            type->gen(level);
        } // puts("");
        //        } // while((types = types.next));

        //        List<ASTFunc*> funcs = this->funcs;
        //        ASTFunc* func;
        //        do
        //        if (this->funcs.item) {
        foreach (func, funcs, this->funcs) {
            //                if (!func) continue;
            func->gen(level);
        }
        //        } // while (    (funcs =funcs.next));
    }
};

static void print_sizes()
{
    printf("ASTImport %lu\n", sizeof(ASTImport));
    printf("ASTUnits %lu\n", sizeof(ASTUnits));
    printf("ASTExpr %lu\n", sizeof(ASTExpr));
    printf("ASTVar %lu\n", sizeof(ASTVar));
    printf("ASTType %lu\n", sizeof(ASTType));
    //    printf("ASTStmt %lu\n", sizeof(ASTStmt));
    printf("ASTScope %lu\n", sizeof(ASTScope));
    printf("ASTTypeSpec %lu\n", sizeof(ASTTypeSpec));
    printf("ASTFunc %lu\n", sizeof(ASTFunc));
    printf("ASTModule %lu\n", sizeof(ASTModule));
}
// POOLS
// 1-way pools based on type (no freeing)
// rename this to node pool

void alloc_stat()
{
    ASTImport::pool.stat();
    //    ASTUnits::pool.stat();
    ASTExpr::pool.stat();
    ASTVar::pool.stat();
    ASTType::pool.stat();
    //    ASTStmt::pool.stat();
    ASTScope::pool.stat();
    ASTTypeSpec::pool.stat();
    ASTFunc::pool.stat();
    ASTModule::pool.stat();
//        List<ASTExpr*>::pool.stat();
    fprintf(stderr, "Total %lu B\n", globalMemAllocBytes);
}

#pragma mark - Parser

class Parser {
    char* filename = NULL; // mod/submod/xyz/mycode.ch
    char* moduleName = NULL; // mod.submod.xyz.mycode
    char* mangledName = NULL; // mod_submod_xyz_mycode
    char* capsMangledName = NULL; // MOD_SUBMOD_XYZ_MYCODE
    char* basename = NULL; // mycode
    char* dirname = NULL; // mod/submod/xyz
    char *data = NULL, *end = NULL;
    char* noext = NULL;
    Token token; // current
    List<ASTModule*> modules; // module node of the AST
    Stack<ASTScope*> scopes; // a stack that keeps track of scope nesting

    public:
    static Pool<Parser> pool;
    void* operator new(size_t size) { return pool.alloc(); }
    static const char* _typeName() { return "Parser"; }

#define STR(x) STR_(x)
#define STR_(x) #x

    ~Parser()
    {
        free(data);
        free(noext);
        free(moduleName);
        free(mangledName);
        free(capsMangledName);
        free(dirname);
    }
    uint32_t errCount = 0, warnCount = 0;
    Parser(char* filename, bool skipws = true)
    {
        static const char* _funcSig
            = "%s:%d: Parser::Parser(filename = \"%s\", skipws = %s)";
        // the file and line should be of the call site, not the definition!
        // ie every func must return errcode in check generated source. if
        // err code is unhandled then backtrace. func prints its string?
        // caller needs to supply file,line as arg then. or else caller
        // prints, needs str ptr then. all str ptrs could be made const
        // static globals.

        static const auto FILE_SIZE_MAX = 1 << 24;
        FILE* file = fopen(filename, "r");
        fprintf(stdout, "compiling %s\n", filename);
        this->filename = filename;
        noext = str_noext(filename); // will leak
        fseek(file, 0, SEEK_END);
        const size_t size
            = ftell(file) + 2; // 2 null chars, so we can always lookahead
        if (size < FILE_SIZE_MAX) { // 16MB max
            data = (char*)malloc(size);
            fseek(file, 0, SEEK_SET);
            fread(data, size, 1, file);
            data[size - 1] = 0;
            data[size - 2] = 0;
            moduleName = str_tr(noext, '/', '.');
            mangledName = str_tr(noext, '/', '_');
            capsMangledName = str_upper(mangledName);
            basename = str_base(noext);
            dirname = str_dir(noext);
            end = data + size;
            token.pos = data;
            token.flags.skipWhiteSpace = skipws;
        }

        fclose(file);
        return;

    _printbt:
        fprintf(stderr, _funcSig, __LINE__, filename, skipws);
    }

#pragma mark - Error Reporting

    static const auto errLimit = 10;
#define fatal(str, ...)                                                    \
    {                                                                      \
        fprintf(stderr, str, __VA_ARGS__);                                 \
        exit(1);                                                           \
    }

    void errorIncrement()
    {
        if (++errCount >= errLimit)
            fatal("too many errors (%d), quitting\n", errLimit);
    }

#define errorExpectedToken(a) errorExpectedToken_(a, __func__)
    void errorExpectedToken_(TokenKind expected, const char* funcName)
    {
        fputs(dashes, stderr);
        fprintf(stderr,
            "(%d) error: %s at %s:%d:%d\n      expected '%s' found '%s'\n",
            errCount + 1, funcName, filename, token.line, token.col,
            TokenKind_repr(expected),
            token.repr() /*matchlen, token.pos */);
        errorIncrement();
    }

#define errorParsingExpr() errorParsingExpr_(__func__)
    void errorParsingExpr_(const char* funcName)
    {
        fputs(dashes, stderr);
        fprintf(stderr,
            "(%d) error: %s at %s:%d\n      failed to parse expr",
            errCount + 1, funcName, filename,
            token.line - 1); // parseExpr will move to next line
        errorIncrement();
    }

#define errorUnexpectedToken() errorUnexpectedToken_(__func__)
    void errorUnexpectedToken_(const char* funcName)
    {
        fputs(dashes, stderr);
        fprintf(stderr,
            "(%d) error: %s at %s:%d:%d\n      unexpected token '%.*s'\n",
            errCount + 1, funcName, filename, token.line, token.col,
            token.matchlen, token.pos);
        errorIncrement();
    }

#define errorUnexpectedExpr(e) errorUnexpectedExpr_(e, __func__)
    void errorUnexpectedExpr_(const ASTExpr* expr, const char* funcName)
    {
        fputs(dashes, stderr);
        fprintf(stderr,
            "(%d) error: %s at %s:%d:%d\n      unexpected expr '%.*s'",
            errCount + 1, funcName, filename, expr->line, expr->col,
            expr->opPrecedence ? 100 : expr->strLength,
            expr->opPrecedence ? TokenKind_repr(expr->kind) : expr->name);
        errorIncrement();
    }

#pragma mark -

    ASTExpr* exprFromCurrentToken()
    {
        auto expr = new ASTExpr(&token);
        token.advance();
        return expr;
    }

    ASTExpr* next_token_node(TokenKind expected, const bool ignore_error)
    {
        if (token.kind == expected) {
            return exprFromCurrentToken();
        } else {
            if (not ignore_error) errorExpectedToken(expected);
            return NULL;
        }
    }

    // in the match case, token should be advanced on error
    ASTExpr* match(TokenKind expected)
    {
        return next_token_node(expected, false);
    }

    // this returns the match node or null
    ASTExpr* trymatch(TokenKind expected)
    {
        return next_token_node(expected, true);
    }

    // just yes or no, simple
    bool matches(TokenKind expected) { return (token.kind == expected); }

    bool ignore(TokenKind expected)
    {
        bool ret;
        if ((ret = matches(expected))) token.advance();
        return ret;
    }

    // this is same as match without return
    void discard(TokenKind expected)
    {
        if (not ignore(expected)) errorExpectedToken(expected);
    }


    char* parseIdent()
    {
        if (token.kind != TKIdentifier) errorExpectedToken(TKIdentifier);
        char* p = token.pos; // dup();
        token.advance();
        return p;
    }

#pragma mark -
    ASTExpr* parseExpr()
    {

        // there are 2 steps to this madness.
        // 1. parse a sequence of tokens into RPN using shunting-yard.
        // 2. walk the rpn stack as a sequence and copy it into a result
        // stack, collapsing the stack when you find nonterminals (ops, func
        // calls, array index, ...)

        // we could make this static and set len to 0 upon func exit
        Stack<ASTExpr*> rpn, ops, result;
        int prec_top = 0;
        ASTExpr* p = NULL;
        TokenKind revBrkt = TKUnknown;
        //        bool errExpr = false;

        //        token.flags.noKeywordDetect = true;
        // ******* STEP 1 CONVERT TOKENS INTO RPN

        while (token.kind != TKNullChar and token.kind != TKNewline
            and token.kind != TKLineComment) { // build RPN

            // you have to ensure that ops have a space around them, etc.
            // so don't just skip the one spaces like you do now.
            if (token.kind == TKOneSpace) token.advance();

            //            TokenKind exprKind = token.kind;

            ASTExpr* expr = new ASTExpr(&token); // exprFromCurrentToken();
            int prec = expr->opPrecedence;
            bool rassoc = expr->opIsRightAssociative;
            char lookAheadChar = token.peekCharAfter();
            switch (expr->kind) {
            case TKIdentifier:
                switch (lookAheadChar) {
                case '(':
                    expr->kind = TKFunctionCall;
                    expr->opPrecedence = 100;
                    ops.push(expr);
                    break;
                case '[':
                    expr->kind = TKSubscript;
                    expr->opPrecedence = 100;
                    ops.push(expr);
                    break;
                default:
                    rpn.push(expr);
                }
                break;
            case TKParenOpen:
                ops.push(expr);
                if (not ops.empty()
                    and ops.top()->kind == TKFunctionCall)
                    rpn.push(expr);
                // instead of marking with (, could consider pushing a NULL.
                // (not for func calls & array indexes -- for grouping)
                break;

            case TKArrayOpen:
                ops.push(expr);
                if (not ops.empty()
                    and ops.top()->kind == TKSubscript)
                    rpn.push(expr);
                break;

            case TKParenClose:
            case TKArrayClose:
            case TKBraceClose:

                revBrkt = TokenKind_reverseBracket(expr->kind);
                while (not ops.empty()) {
                    p = ops.pop();
                    if (p->kind == revBrkt) break;
                    rpn.push(p);
                }
                // we'll push the TKArrayOpen as well to indicate a list
                // literal or comprehension
                // TKArrayOpen is a unary op.
                if (p->kind == TKArrayOpen
                    and (ops.empty()
                        or ops.top()->kind
                            != TKSubscript) // don't do this if its part of
                                            // a subscript
                    and (rpn.empty()
                        or rpn.top()->kind
                            != TKOpColon)) // or aa range. range exprs are
                                           // handled separately. by
                                           // themselves they don't need a
                                           // surrounding [], but for
                                           // grouping like 2+[8:66] you do.
                    rpn.push(p);

                // result.push( expr); // for funcs/arrays only.
                // or we could not use these as seps but mark the func ident
                // as TKIdentf and set nargs. NOPE u need to know nargs
                // before starting to parse args.
                //            }
                break;
            case TKKeyword_return:
                // to treat empty return, push a NULL if there is no expr
                // coming.
                ops.push(expr);
                if (lookAheadChar == '!' or lookAheadChar == '\n')
                    rpn.push(NULL);
                break;
            default:
                if (prec) { // general operators

                    if (expr->kind == TKOpColon) {
                        if (rpn.empty()
                            or (!rpn.top() and !ops.empty()
                                and ops.top()->kind != TKOpColon)
                            or (rpn.top() and rpn.top()->kind == TKOpColon
                                and !ops.empty()
                                and (ops.top()->kind == TKOpComma
                                    or ops.top()->kind
                                        == TKArrayOpen)) // <<-----
                                                         // yesssssssssss
                        )

                            rpn.push(NULL); // indicates empty operand
                        ;
                    }
                    while (not ops.empty())
                    {
                        prec_top = ops.top()->opPrecedence;
                        if (not prec_top) break; // left parenthesis
                        if (prec > prec_top) break;
                        if (prec == prec_top and rassoc) break;
                        p = ops.pop();

                        if (p->kind != TKOpComma
                            and p->kind != TKOpSemiColon
                            and p->kind != TKFunctionCall
                            and p->kind != TKSubscript and rpn.top()
                            and rpn.top()->kind == TKOpComma) {
                            errorUnexpectedToken();
                            goto error;
                        }

                        if (!p->opIsUnary
                            and p->kind != TKFunctionCall
                            and p->kind != TKOpColon
                            // in case of ::, second colon will add null later
                            and p->kind != TKSubscript
                            and rpn.count < 2) {
                            errorUnexpectedToken();
                            goto error;
                            // TODO: even if you have more than two, neither
                            // of the top two should be a comma
                        }

                        rpn.push(p);
                    }
                    //                    assert(!rpn.empty());
                    // when the first error is found in an expression, seek
                    // to the next newline and return NULL.
                    if (rpn.empty()) {
                        errorUnexpectedToken();
                        goto error;
                    }
                    if (expr->kind == TKOpColon
                        and (lookAheadChar == ','
                             or lookAheadChar == ':'
                             or lookAheadChar == ']'
                             or lookAheadChar == ')'))
                        rpn.push(NULL);

                    ops.push(expr);
                } else {
                    rpn.push(expr);
                }
            }
            token.advance();
            if (token.kind == TKOneSpace) token.advance();
        }
    exitloop:

        while (not ops.empty()) //
        {
            p = ops.pop();

            if (p->kind != TKOpComma
                and p->kind != TKFunctionCall
                and p->kind != TKSubscript
                and p->kind != TKArrayOpen
                and rpn.top()
                and rpn.top()->kind == TKOpComma) {
                errorUnexpectedExpr(rpn.top());
                goto error;
            }

            if (!p->opIsUnary
                and (p->kind != TKFunctionCall
                     and p->kind != TKSubscript)
                and rpn.count < 2) {
                errorParsingExpr();
                goto error;
                // TODO: even if you have more than two, neither of the top
                // two should be a comma
            }

            rpn.push(p);
        }

        // *** STEP 2 CONVERT RPN INTO EXPR TREE

        ASTExpr* arg;
        for (int i = 0; i < rpn.count; i++) {
            if (!(p = rpn[i])) goto justpush;
            switch (p->kind) {
            case TKFunctionCall:
            case TKSubscript:
                assert(result.count > 0);
                arg = result.pop();
                p->left = arg;
            case TKNumber:
            case TKString:
            case TKRegex:
            case TKMultiDotNumber:
            case TKIdentifier:
            case TKParenOpen:
//            case TKArrayOpen:
                break;
            default:
                // careful now, assuming everything except those above is a
                // nonterminpal and needs left/right
                assert(p->opPrecedence); //if (not p->opPrecedence) continue;
                // operator must have some precedence, right?

                if (result.empty()) {
                    errorParsingExpr();
                    goto error;
                }

                p->right = result.pop();

                if (not p->opIsUnary) {
                    if (result.empty()) {
                        errorParsingExpr();
                        goto error;
                    }
                    p->left = result.pop();
                }
            }
        justpush:
            result.push(p);
        }
        if (result.count != 1) {
            errorParsingExpr();
            goto error;
        }

        return result[0];

    error:

        while (token.kind != TKNewline
               and token.pos < this->end)
            token.advance();

        if (ops.count) printf("\n      ops: ");
        for (int i = 0; i < ops.count; i++)
            printf("%s ", TokenKind_repr(ops[i]->kind));

        if (rpn.count) printf("\n      rpn: ");
        for (int i = 0; i < rpn.count; i++)
            if (!rpn[i])
                printf("NUL ");
            else
                printf("%.*s ",
                    rpn[i]->opPrecedence ? 100 : rpn[i]->strLength,
                    rpn[i]->opPrecedence ? TokenKind_repr(rpn[i]->kind)
                                         : rpn[i]->name);
        if (p)
            printf("\n      p: %.*s ", p->opPrecedence ? 100 : p->strLength,
                p->opPrecedence ? TokenKind_repr(p->kind) : p->name);

        if (rpn.count or ops.count) puts("");

        return NULL;
    }

#pragma mark -
    ASTTypeSpec* parseTypeSpec()
    { // must have ident(U), then may have "[:,:]" i.e. '[\]\[\:, ]+' , then
      // may have units. note: after ident may have params <T, S>
        token.flags.mergeArrayDims = true;

        auto typeSpec = new ASTTypeSpec;

        typeSpec->name = parseIdent();
        //        typeSpec->params = parseParams();
        // have a loop and switch here so you can parse both
        // Number[:,:]|kg/s and Number|kg/s[:,:]. linter will fix.

        if (matches(TKArrayDims)) {
            for (int i = 0; i < token.matchlen; i++)
                if (token.pos[i] == ':') typeSpec->dims++;
            if (!typeSpec->dims) typeSpec->dims = 1; // [] is 1 dim
            token.advance();
        }

        if (matches(TKUnits)) {
            // assert that the type is Number, nothing else can have units.
            // read the units and assign it to typeSpec->units. this will
            // overwrite name -- you don't need it, it's 'Number' when valid.
            ignore(TKUnits);
        }
        // fixme: node->type = lookupType;

        assert(token.kind != TKUnits);
        assert(token.kind != TKArrayDims);

        token.flags.mergeArrayDims = false;
        return typeSpec;
    }

    List<ASTVar*> parseArgs()
    {
        token.flags.mergeArrayDims = true;
        discard(TKParenOpen);

        List<ASTVar*> args;
        ASTVar* arg;
        do {
            arg = parseVar();
            args.append(arg);
        } while (ignore(TKOpComma));

        discard(TKParenClose);
        token.flags.mergeArrayDims = false;

        return args;
    }

#pragma mark -
    ASTVar* parseVar()
    {
        auto var = new ASTVar;
        var->flags.isVar = (token.kind == TKKeyword_var);
        var->flags.isLet = (token.kind == TKKeyword_let);

        if (var->flags.isVar) discard(TKKeyword_var);
        if (var->flags.isLet) discard(TKKeyword_var);
        if (var->flags.isVar or var->flags.isLet) discard(TKOneSpace);

//        token.flags.noKeywordDetect = true;
        var->name = parseIdent();
//        token.flags.noKeywordDetect = false;

        if (ignore(TKOneSpace)) {
            if (ignore(TKKeyword_as)) {
            discard(TKOneSpace);
            var->typeSpec = parseTypeSpec();

            }
        }
//        token.flags.noKeywordDetect = false;
        ignore(TKOneSpace);
        if (ignore(TKOpAssign)) {
//            discard(TKOpAssign);
//            discard(TKOneSpace);
            var->init = parseExpr();
        }
        return var;
    }

#pragma mark -
    ASTScope* parseScope()
    {
        auto scope = new ASTScope;

        union {
            ASTScope* subScope;
            ASTVar* var = NULL;
        }; // last variable encountered

        // don't conflate this with the while in parse(): it checks against
        // file end, this checks against the keyword 'end'.
        while (token.kind != TKKeyword_end) {
            switch (token.kind) {
            case TKNullChar:
                errorExpectedToken(TKUnknown);
                goto exitloop;
            case TKKeyword_var:
                var = parseVar();
                if (!var) continue;
                scope->locals.append(var);
                break;
                //            case TKKeyword_check:
                //            case TKKeyword_print:
                //            case TKKeyword_return:

                break;
                //            case TKIdentifier:
                //            case TKFunctionCall:
                //            case TKSubscript:
                //                expr = parseExpr();
                //                break;
            case TKKeyword_if:
            case TKKeyword_for:
            case TKKeyword_while:
                // not quite
                //                subScope = parseScope();
                //                subScope->parent = scope;
                break;
            case TKNewline:
            case TKLineComment:
            case TKOneSpace: // found at beginning of line
                break;
            default:
                scope->stmts.append(parseExpr());
                break;
            }
            token.advance();
        }
    exitloop:
        return scope;
    }

    List<ASTVar*> parseParams()
    {
        discard(TKOpLT);
        List<ASTVar*> params;
        ASTVar* param;
        do {
            param = new ASTVar;
            param->name = parseIdent();
            if (ignore(TKOpColon)) param->typeSpec = parseTypeSpec();
            if (ignore(TKOpAssign)) param->init = parseExpr();
            params.append(param);
        } while (ignore(TKOpComma));

        discard(TKOpGT);
        return params;
    }

#pragma mark -
    ASTFunc* parseFunc()
    {
        discard(TKKeyword_function);
        discard(TKOneSpace);
        auto func = new ASTFunc;

//        token.flags.noKeywordDetect = true;

        func->name = parseIdent();
        func->args = parseArgs();
        if (ignore(TKOneSpace) and ignore(TKKeyword_as)) {

            discard(TKOneSpace);
            func->returnType = parseTypeSpec();
        }
//        token.flags.noKeywordDetect = false;

        discard(TKNewline);

        func->body = parseScope();

        discard(TKKeyword_end);
        discard(TKOneSpace);
        discard(TKKeyword_function);

        return func;
    }

#pragma mark -
    ASTFunc* parseTest() { return NULL; }

#pragma mark -
    ASTUnits* parseUnits() { return NULL; }

#pragma mark -
    ASTType* parseType()
    {
        auto type = new ASTType; // exprFromCurrentToken();
        union {
            ASTExpr* expr;
            ASTVar* var;
        };

        discard(TKKeyword_type);
        discard(TKOneSpace);
        type->name = parseIdent();
        if (matches(TKOpLT)) type->params = parseParams();

        while (token.kind != TKKeyword_end) {
            switch (token.kind) {
            case TKNullChar:
                errorExpectedToken(TKUnknown); // return NULL;
                goto exitloop;
            case TKKeyword_var:
                var = parseVar();
                if (!var) continue;
                type->vars.append(var);

                break;
            case TKKeyword_base:
                discard(TKKeyword_base);
                discard(TKOneSpace);
                type->super = parseTypeSpec();
            case TKIdentifier:
                break;
//            case TKKeyword_check:
//                break;
            case TKNewline:
            case TKLineComment:
            case TKOneSpace:
                break;
            default:
                errorUnexpectedToken();// errorExpectedToken(TKUnknown);
                break;
            }
            token.advance();
        }
    exitloop:

        discard(TKKeyword_end);
        discard(TKOneSpace);
        discard(TKKeyword_type);

        return type;
    }

#pragma mark -
    ASTImport* parseImport()
    {
        auto import = new ASTImport;
        char* tmp;
        discard(TKKeyword_import);
        discard(TKOneSpace);
//        token.flags.noKeywordDetect = true;
        import->isPackage = ignore(TKAt);
        import->importFile = parseIdent();
//        token.flags.noKeywordDetect = false;
        ignore(TKOneSpace);
        if (ignore(TKKeyword_as)) {
//            token.flags.noKeywordDetect = true;
            ignore(TKOneSpace);
            import->hasAlias = true;
            tmp = parseIdent();
            if (tmp)
                import->aliasOffset = (uint32_t)(tmp - import->importFile);
//            token.flags.noKeywordDetect = false;
        } else {
            import->aliasOffset = (uint32_t)(
                str_base(import->importFile, '.') - import->importFile);
        }
        return import;
    }

#pragma mark -
    List<ASTModule*> parse()
    {
        auto root = new ASTModule;
        root->name = moduleName;
        const bool onlyPrintTokens = false;
        token.advance(); // maybe put this in parser ctor
        ASTImport* import = NULL;

        while (token.kind != TKNullChar) {
            if (onlyPrintTokens) {
                printf("%s %2d %3d %3d %-6s\t%.*s\n", basename, token.line,
                    token.col, token.matchlen, TokenKind_repr(token.kind),
                    token.kind == TKNewline ? 0 : token.matchlen,
                    token.pos);
                token.advance();
                continue;
            }
            switch (token.kind) {
            case TKKeyword_function:
                root->funcs.append(parseFunc());
                break;
            case TKKeyword_type:
                root->types.append(parseType());
                break;
            case TKKeyword_import:
                import = parseImport();
                if (import) {
                    root->imports.append(import);
                    //                    auto subParser = new
                    //                    Parser(import->importFile);
                    //                    List<ASTModule*> subMods =
                    //                    subParser->parse();
                    //                    modules.append(subMods);
                }
                break;
            case TKKeyword_test:
                root->tests.append(parseTest());
                break;
            case TKKeyword_var:
            case TKKeyword_let:
                root->globals.append(parseVar());
                break;
            case TKNewline:
            case TKLineComment:
            case TKOneSpace:
                token.advance();
                break;
            default:
                printf("other token: %s at %d:%d len %d\n", token.repr(),
                    token.line, token.col, token.matchlen);
                token.advance();
            }
        }
        modules.append(root);
        return modules;
    }
};

Pool<ASTTypeSpec> ASTTypeSpec::pool;
Pool<ASTVar> ASTVar::pool;
Pool<ASTImport> ASTImport::pool;
Pool<ASTModule> ASTModule::pool;
Pool<ASTFunc> ASTFunc::pool;
Pool<ASTType> ASTType::pool;
Pool<ASTExpr> ASTExpr::pool;
Pool<ASTScope> ASTScope::pool;
Pool<Parser> Parser::pool;

#pragma mark - main
int main(int argc, char* argv[])
{
    if (argc == 1) return 1;

    auto parser = new Parser(argv[1]);

    List<ASTModule*> modules = parser->parse();

    if (parser->errCount or parser->warnCount) {
        fputs(equaltos, stderr);
        if (parser->errCount)
            fprintf(stderr, "*** %d errors\n", parser->errCount);
        if (parser->warnCount)
            fprintf(stderr, "*** %d warnings\n", parser->warnCount);
        fprintf(stderr, "    *Reading* the code helps, sometimes.");
        fputs(equaltos, stderr);
        return 1;
    };

    foreach(mod, mods, modules) mod->gen();

//    alloc_stat();
    return 0;
}