
struct Module {
    // Diagnostics* errs;
    List<Func&> funcs;
    List<Type&> types;
    List<Import&> imports;
    Scope scope;

    // ~Module() { delete errs; }

#include "Module_loadsave.hpp"

    Var* getVar(const char* name) { return nullptr; }
    Func* getFunc(const char* selector) { return nullptr; }
    Type* getType(const char* name) { return nullptr; }
    Import* getImport(const char* alias) { return nullptr; }

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