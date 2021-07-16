
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
    }
    getSelector(ret);
    ret->intrinsic = true;
    li_shift(&module->funcs, ret);
    // module->modified = true;
    // FIXME: it must be recompiled if in non-monolithic mode!!
    // just turned it off to check. Turn it back on!
    // WAIT it must be set only when the request for the template instn
    // comes from another module. If it is from the same module, and
    // that module has already been compiled then it is good to go, but if
    // you set modified it will be recompiled needlessly.
    return ret;
  nextfunc:;
  }
  return NULL;
}
