
struct Module {
    // Diagnostics* errs;
    List<Func&> funcs;
    List<Type&> types;
    List<Import&> imports;
    Scope* scope;

    // ~Module() { delete errs; }

#include "Module_loadsave.hpp"

    Func* getFuncByTypeMatch(Expr& funcCallExpr) {
        for (Func& func : funcs) {
            if (!strcasecmp(funcCallExpr.string, func.name)
                && funcCallExpr.left->countCommaList() == func.args.count()) {
                // check all argument types to see if they match.
                Expr* carg = funcCallExpr.left;
                for (Var& farg : func.args) {
                    if (!carg) break;
                    Expr* arg = (carg->kind == tkOpComma) ? carg->left : carg;
                    // __ this is why you need typeType and typeSubType so that
                    // compatible types can be checked by equality ignoring
                    // subType. The way it is now, CString and String wont match
                    // because they arent strictly equal, although they are
                    // perfectly compatible for argument passing.
                    if (arg->typeInfo.typeType == farg.typeInfo.typeType
                        and arg->typeInfo.collectionType
                            == farg.typeInfo.collectionType) {
                        if (arg->typeInfo.typeType == TYObject
                            and arg->getTypeOrEnum() != farg.type)
                            goto nextfunc;
                    }

                    carg = (carg->kind == tkOpComma) ? carg->right : nullptr;
                }

                return &func;
            }
        nextfunc:;
        }
        return nullptr;
    }
    Var* getVar(const char* name) {
        if (not scope) return nullptr;
        for (Var& var : scope->vars)
            if (!strcasecmp(var.name, name)) return &var;
        return nullptr;
    }
    Func* getFunc(const char* selector) {
        for (Func& func : funcs)
            if (!strcasecmp(func.selector, selector)) return &func;
        return nullptr;
    }
    Type* getType(const char* name) {
        for (Type& type : types)
            if (!strcasecmp(type.name, name)) return &type;
        return nullptr;
    }
    Import* getImport(const char* alias) {
        for (Import& imp : imports)
            if (!strcmp(imp.alias, alias)) return &imp;
        return nullptr;
    }

    Func& newFunc(const char* name, const char* selector,
        Type* retType = nullptr, Type* argType = nullptr,
        bool intrinsic = false, bool external = false) {
        // push new func on list & return it
    }
    Type& newType(const char* name) { }
    Var& newVar(const char* name, TypeInfo typeInfo) {
        // if type, set it, else set typeType
    }
    /// Create a new import with the given name and alias, and load the module.
    Import& newImport(List<Module&>& modules, const char* name,
        const char* alias = nullptr) { }
    /// Create a new import with an already loaded module.
    Import& newImport(Module& module) { }
};