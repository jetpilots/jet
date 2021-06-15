
monostatic Func* mod_makeFunc(Module* module, Expr* funcCallExpr) {
  foreach (Func*, tfunc, module->tfuncs) {
    if (strcasecmp(funcCallExpr->str, tfunc->name)) continue;
    if (expr_countCommaList(funcCallExpr->left) != tfunc->argCount)
      continue;
    zip(cArg, ExprP, arg, VarP, funcCallExpr->left, ExprP, tfunc->args,
        PtrListP) {
      if (cArg->kind == tkArgAssign) {
        if (strcasecmp(arg->name, cArg->left->str)) goto nextfunc;
        cArg = cArg->right;
      }
      if (ISIN(3, cArg->typeType, TYUnknown, TYError, TYVoid)) return NULL;

      if (arg->spec->typeType != TYUnknown
          && !(cArg->typeType == arg->spec->typeType
              && cArg->collType == arg->spec->collType))
        goto nextfunc;
    }
    // at this point we have a match. instntiate and prepare it
    Func* ret = func_clone(tfunc);
    zip(cArg, ExprP, arg, VarP, funcCallExpr->left, ExprP, tfunc->args,
        PtrListP) {
      if (cArg->kind == tkArgAssign) { cArg = cArg->right; }
      // concretize the arg types
      arg->spec->typeType = cArg->typeType;
      arg->spec->collType = cArg->collType;
      if (arg->spec->typeType == TYObject)
        arg->spec->type = expr_getTypeOrEnum(cArg);
      getSelector(ret);
    }

    li_shift(&module->funcs, ret);
    module->modified
        = true; // it must be recompiled if in non-monolithic mode
    return ret;
  nextfunc:;
  }
  return NULL;
}
