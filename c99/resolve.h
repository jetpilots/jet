
// isSelfMutOp(expr as Expr) := expr.kind in [
//     .plusEq, .minusEq, .slashEq, .timesEq, .opModEq, .opAssign
// ]

// function resolve(spec as TypeSpec, mod as Module, par as Parser)
//     if spec.typeType != .unresolved then return
//     if spec.name == "" then return
//     var tyty = typeTypeByName(spec.name)
//     if tyty != .unresolved
//         spec.typeType = tyty
//     else
//         type = lookupType(spec.name, module = mod)
//         spec.typeType = .object
//         spec.type = type
//     end if
//     on error . itemNotFound
//         errorUnrecognized(spec, parser = parser)
// end function

static void resolveTypeSpec(Parser* parser, TypeSpec* spec, Module* mod) {
  // TODO: disallow a type that derives from itself!
  if (spec->typeType != TYUnknown) return;
  if (!*spec->name) return;

  // TODO: DO THIS IN PARSE... stuff!!

  TypeTypes tyty = Typetype_byName(spec->name);
  if (tyty) { // can be member of TypeSpec!
    spec->typeType = tyty;
  } else {
    Type* type = mod_getType(mod, spec->name);
    if (type) {
      spec->typeType = TYObject;
      spec->type = type;
      type->used++;
      return;
    }
    err_unrecognizedType(parser, spec);
    return;
  }
  if (spec->dims) {
    // set collection type, etc.
    // for this we will need info about the var and its usage
    // patterns. so this will probably be a separate func that is
    // called during such analysis.
  }
}
// function checkUnusedVars(scope as Scope, parser as Parser)
//     for var = scope.locals
//
//     end for
// end function

static void scope_checkUnusedVars(Parser* parser, Scope* scope) {
  foreach (Var*, var, scope->locals)
    if (!var->used) warn_unusedVar(parser, var);

  foreach (Expr*, stmt, scope->stmts)
    if (isCtrlExpr(stmt) && stmt->body)
      scope_checkUnusedVars(parser, stmt->body);
}

static void func_checkUnusedVars(Parser* parser, Func* func) {
  foreach (Var*, arg, func->args)
    if (!arg->used) warn_unusedArg(parser, arg);

  scope_checkUnusedVars(parser, func->body);
}

static void JetTest_checkUnusedVars(Parser* parser, JetTest* test) {
  scope_checkUnusedVars(parser, test->body);
}

// TODO: Btw there should be a module level scope to hold lets (and
// comments). That will be the root scope which has parent==NULL.

// This function is called in one pass, during the line-by-line parsing.
// (since variables cannot be "forward-declared").
static void resolveVars(Parser* parser, Expr* expr, Scope* scope,
    bool inFuncCall) { // TODO: this could be done on rpn in parseExpr,
                       // making it iterative instead of recursive = behaves
                       // differently inside a func call: the ->left is not
                       // resolved to a var, but to an argument label of the
                       // called func. it would make sense to just skip
                       // checking it here for now, and let resolveFuncs use
                       // it to construct the func selector for lookup. At
                       // some point though it would be nice if the compiler
                       // could tell the user 'you missed the arg label
                       // "xyz"' for which the basename of the func could be
                       // used to get a list of all selectors having that
                       // basename as a prefix.

  if (!expr) return;
  switch (expr->kind) {
  case tkIdentR:
  case tkSubscriptR:
    expr->var->lastUsage = expr->line;
    // ^ TODO: actually the line to set is not expr->line, but the line
    // number of the toplevel expr. In the new code you pass parent expr
    // into analyse & resolve, use that to walk up to the toplevel and find
    // its line number. This matters for statements like:
    // var g =
    //    1 + call(2,
    //             4,
    //             h)
    // when you later want to insert drop calls, you have to find the expr
    // with the corresponding line (==, not >= since order can be changed)
    // The stmt list will have the line number corresponding to the varDecl
    // line and not the actual line on which h appears.
    break;

  case tkIdent:
  case tkSubscript: {
    TokenKind ret = (expr->kind == tkIdent) ? tkIdentR : tkSubscriptR;
    Var* found = scope_getVar(scope, expr->str);
    if (found) {
      expr->kind = ret;
      expr->var = found;
      expr->var->used++;
      expr->var->lastUsage = expr->line; // see above for a TODO and info
    } else {
      // Import* import = mod_getImportByAlias(mod,
      // expr->str); if (import) {
      //     expr->kind = tkImport;
      //     expr->import = import;
      //     import->used = true;
      // } else {
      err_unrecognizedVar(parser, expr);
      // }
    }
    if (expr->kind == tkSubscriptR || expr->kind == tkSubscript) {
      resolveVars(parser, expr->left, scope, inFuncCall);
      // check subscript argument count
      // recheck kind since the var may have failed resolution
      // TODO: handle dims 0 as dims 1 because arr[] is the same as arr[:]
      //            if (expr->kind == tkSubscriptR
      //                and expr_countCommaList(expr->left)
      //                    != expr->var->spec->dims)
      //                err_indexDimsMismatch(parser, expr);
      // do it in analysis after type/dims inference
    }
    break;
  }
  case tkFuncCall:
    if (expr->left) resolveVars(parser, expr->left, scope, true);
    break;

  case tkPeriod:
    if (expr->left) resolveVars(parser, expr->left, scope, inFuncCall);
    // expr->right is not to be resolved in the same scope, but in
    // the type body of the type of expr->left. So you cannot call
    // resolveVars on expr->right from here. Neither can you assume
    // that types have been resolved, because name resolution
    // happens as the file is read, while type resolution happens
    // only after the tree is fully built. The way to fix it is to
    // have analyseExpr call the name resolution (as it already does for
    // exprs like a.b but not a.b.c) for expr->right, AFTER the type
    // for expr->left has been resolved.
    //        if (expr->right->kind==tkPeriod) resolveVars(this,
    //        expr->right, scope, inFuncCall);
    // besides an ident, the ->right of a . can be either another
    // dot, a subscript, or a func call if we allow member funcs
    if (expr->right)
      if (expr->right->kind == tkSubscript
          || expr->right->kind == tkSubscriptR)
        resolveVars(parser, expr->right->left, scope, inFuncCall);

    break;

  case tkRawString:
  case tkString: {
    // strings may have embedded variable names of the form $name or
    // $(name), so resolve them. by the time this func is called, the string
    // will be null-terminated
    char* pos = strchr(expr->str, '$');
    while (pos) {
      if (pos[-1] != '\\') {
        if (pos[1] == '(') pos++;
        size_t len = strspn(pos + 1,
            "abcdefghijklmnopqrstuvwxyz"
            "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            "1234567890");
        if (len) {
          if (len > 31) len = 31;
          char buf[32];
          strncpy(buf, pos + 1, len);
          buf[len] = 0;
          buf[31] = 0;
          //            eprintf("lookup '%s'\n", buf);
          Var* var = scope_getVar(scope, buf);
          if (!var) {
            char* orig = expr->str;
            expr->str = buf;
            expr->col += (pos - orig) + 1;
            err_unrecognizedVar(parser, expr);
            expr->col -= (pos - orig) + 1;
            expr->str = orig;
          } else {
            // we're not going to actually "resolve" the embedded
            // var, just lint the name here so it is in the correct
            // case
            memcpy(pos + 1, var->name, len);
            var->used++;
          }

        } else {
          err_stringInterp(parser, expr, pos);
        }
      }
      pos = strchr(pos + 1, '$');
    }
  } break;

  default:
    if (expr->prec) {
      if (!expr->unary) {
        if (inFuncCall && expr->kind == tkAssign) {
          // expr->left->kind = tkArgumentLabel;
          // Here you only set the type of the expr to
          // tkArgumentLabel. It will be resolved later in analyse(),
          // when the functions have been resolved. Maybe this should
          // also be moved there then.
        } else {
          resolveVars(parser, expr->left, scope, inFuncCall);
        }
      }
      resolveVars(parser, expr->right, scope, inFuncCall);

      if (isSelfMutOp(expr)) {
        Var* var = NULL;
        Expr* varExpr = expr->left;
        if (varExpr) {
          if (varExpr->kind == tkIdentR || //
              varExpr->kind == tkSubscriptR) {
            var = varExpr->var;
          } else if (varExpr->kind == tkPeriod && //
              varExpr->left && varExpr->left->kind == tkIdentR) {
            varExpr = varExpr->left;
            var = varExpr->var;
          }
        }
        if (var) {
          // TODO: If you will allow changing the first arg of a
          // function, using an & op or whatever, check for those
          // mutations here BTW this marks entire arrays as used
          // for a.x = ... it marks all of a as used. Instead should
          // traverse the . sequence left to right and check which the
          // first read-only variable in that sequence
          var->changed++;
          if (expr->kind == tkAssign) var->reassigned = true;
          if (!var->isVar) err_readOnlyVar(parser, varExpr);
        }
      }
    }
  }
}
