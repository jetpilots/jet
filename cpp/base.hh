class ASTTypeSpec;
class ASTModule;
class ASTType;
class ASTFunc;
class ASTScope;
class ASTExpr;
class ASTTest;
class ASTVar;
class ASTImport;
class Parser;
enum _TokenKind__wrapped {};
class TokenKind {
    unsigned char kind;
};
static const unsigned long as = sizeof(TokenKind);

class ASTNode {
    union {
        TokenKind kind;
        struct {
            unsigned short _kind : 8, col : 8;
        };
    };
    unsigned short line;
};
template <class T>
class List;
static const unsigned long asn = sizeof(ASTNode);

#include "List.hh"