#include "ASTTypeSpec.hh"
#include "ASTModule.hh"
#include "ASTType.hh"
#include "ASTFunc.hh"
#include "ASTScope.hh"
#include "ASTExpr.hh"
#include "ASTTest.hh"
#include "ASTVar.hh"
#include "ASTImport.hh"

#include "TokenKindDefs.hpp"
#include "tokenKind.hpp"
#include "types.hpp"

class Parser;
// enum _TokenKind__wrapped {};
// struct TokenKind {
//     unsigned char kind;
// };
// static const unsigned long as = sizeof(TokenKind);

struct ASTNode {
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

bool operator^=(ASTVar& a, const char* s) { return !strcasecmp(a.name, s); }
bool operator^=(ASTType& a, const char* s) { return !strcasecmp(a.name, s); }
bool operator^=(ASTFunc& a, const char* s) {
    return !strcasecmp(a.selector, s);
}
bool operator^=(ASTImport& a, const char* s) { return !strcasecmp(a.alias, s); }

#include "List.hh"