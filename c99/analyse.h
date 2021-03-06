
static void setStmtFuncTypeInfo(Parser* parser, Func* func) {
  // this assumes that setExprTypeInfo has been called on the func body
  Expr* stmt = func->body->stmts->item;
  if (!func->spec->typeType)
    func->spec->typeType = stmt->typeType;
  else if (func->spec->typeType != stmt->typeType)
    err_typeMismatchBinOp(parser, stmt);
}

// TODO make sempassModule -> same as mod_analyse now
static void type_analyse(Parser* parser, Type* type, Module* mod);
static void func_analyse(Parser* parser, Func* func, Module* mod);
static void scope_analyse(
  Parser* parser, Scope* scope, Func* ownerFunc, Module* mod);

static void analyseDictLiteral(Parser* parser, Expr* expr, Module* mod) {
  // check that
  // - dict keys in the dict literal are of the same type.
  // - values are of the same type.
  // - all exprs within the commas are tkAssign.
  // assign the spec pair somehow to the tkDictLiteral expr.
  // maybe alloc twice the size and set the pointer so you can access
  // spec[0] and spec[1].
}

// TODO: you should have a flag that tells
// you whether there is any string
// interpolation at all, and if yes what the expected size might be, and
// if it can be put on the stack

// actually the string may have embedded %s, so you need to process it
// in any case, unless you plan on doing puts.
// there are 3 things to do.
// 1. compute the format string
// 2. compute a guess for the size
// 3. keep a stack of vars that are in the string in the right order
static void expr_prepareInterp(Parser* parser, Expr* expr, Scope* scope) {
  // static Array(VPtr) vars;
  assert(expr->kind == tkString || expr->kind == tkRawString);
  PtrList** exprvars = &expr->vars;
  // at some point you should make it so that only strings with a
  // preceding $ get scanned for vars. e.g. $"this is a $kindof day" gets
  // processed but "this is a $kindof day" remains as is.

  char* pos = expr->str + 1; // starts with '"'
  int line = expr->line, col = expr->col;

  // ******
  // The first var should already have been added during resolveVars!!!!
  // So check if there is any var in there, only then you should bother.
  // Otherwise an error will have been shown already in resolveVars.
  while (*pos) {
    // in this loop you print the text preceding a $, then text
    // suceeding a $, then loop. Here's the text before the $
    char* dollar = pos; // strchr(pos, '$');
    while (*dollar && *dollar != '$') {
      dollar++;
      col++;
      if (*dollar == '\n') {
        col = 0;
        line++;
      }
    }
    if (!*dollar) break; // no dollars
    // not using $..., using {...}
    // long lentxt = dollar - pos;
    // printf(",\"%.*s\"", lentxt, pos);
    // after the $ is a variable, so look it up etc.
    char *varname = dollar + 1, *varend;
    bool wasbracket = varname[0] == '(';
    if (wasbracket) varname++;
    // char endchar = 0;
    // if () {
    //     // $(varName)
    //     endchar = ')';
    //     varend = strchrnul(varname, ')');

    // } else {
    // $varName
    varend = varname;
    while (*varend && isalnum(*varend)) ++varend;
    // }
    col += varend - varname;
    if (*varend == ')') *varend++ = 0;
    char endchar = *varend; // store the char in a temp
    *varend = 0;
    Var* var = scope_getVar(scope, varname);

    if (var) strcpy(varname, var->name); // fix case
    *varend = endchar;
    // ^ TODO: this should be getQualVar which looks up a.b.c and
    // returns c
    if (!var) {
      err_unrecognizedVar(
        parser, &(Expr) { .line = line, .col = col, .str = varname });
      // unreachable("undefined var found: '%s' at %d:%d", varname, line,
      // col);
      return;
    }
    // You should have checked for all var refs to be valid in the
    // analysis phase!

    Expr* exdot = NULL;
    Expr* ex = NEWW(Expr, //
      .kind = tkIdentR,   //
      .line = line,       //
      .col = col,         //
      .var = var);
    // var->used++;

    exdot = ex;
    if (!wasbracket)
      while (endchar == '.') {
        if (!var || var->spec->typeType != TYObject) {
          unreachable("using a . for a non-object type (string "
                      "interp: $%.*s)",
            (int)(varend - varname), varname);
          break;
        }

        // varend++;
        varname = ++varend;
        while (*varend && isalnum(*varend)) ++varend;
        col += varend - varname;
        endchar = *varend;
        *varend = 0;
        Type* type = var->spec->type;
        var = type_getVar(type, varname);
        if (!var) {
          exdot = NULL; // reset it if was set along the chain
          err_unrecognizedMember(parser, type,
            &(Expr) {
              .str = varname, .line = line, .col = col, .kind = tkIdent });
        } else {
          // var->used++;
          strcpy(varname, var->name); // fix case
          exdot = NEWW(Expr,          //
            .kind = tkPeriod,
            .left = ex,                      //
            .right = NEWW(Expr,              //
              .kind = tkIdentR,              //
              .line = line,                  //
              .col = col + varend - varname, //
              .var = var));
          ex = exdot;
        }
        *varend = endchar;
      }

    // Here var is the final one ie c in a.b.c

    // const char* fmtstr;
    // if (var->spec->typeType == TYObject) {
    //     fmtstr = "%s";
    //     // you'll have to call type_str(...) for it. Or actually why
    //     // not just do type_print?
    // } else {
    //     fmtstr = Typetype_format(var->spec->typeType, false);
    // }
    // printf(",%s", var->name);
    // ^ this should be qualified name or the cgen version
    // of accessing the actual member for a.b.c

    if (exdot) exprvars = li_push(exprvars, exdot);
    pos = varend;
  }
}
//     break;
// case tkRawString:
// case tkNumber:
// case tkIdentR:
// case tkIdent:
// case tkArgumentLabel:
//     break;
// case tkFuncCallR:
// case tkFuncCall:
// case tkSubscript:
// case tkObjectInit:
// case tkObjectInitResolved:
// case tkSubscriptR:
//     if (expr->left) expr_prepareInterp(expr->left, scope);
//     break;
// case tkVarDefn:
//     expr_prepareInterp(expr->var->init, scope);
//     break;
// case tkFor:
// case tkIf:
// case tkElse:
// case tkMatch:
// case tkCase:
// case tkElif:
// case tkWhile:
//     expr_prepareInterp(expr->left, scope);
//     foreach (Expr*, subexpr, expr->body->stmts)
//         expr_prepareInterp(subexpr, expr->body);
//     break;
// default:
//     if (expr->prec) {
//         if (!expr->unary) expr_prepareInterp(expr->left, scope);
//         if (expr->right) expr_prepareInterp(expr->right, scope);
//     } else {
//         unreachable("unknown token kind %s",
//         TokenKind_str[expr->kind]);
//     }
// }

// #this string
//     "The quick brown fox $jumps over $the lazy dog.";
// #becomes
//     `"%s%s%s%s%s", "The quick brown fox ", jumps, " over ", the, "lazy
//     dog.",
//         ""`;

static void expr_setEnumBase(
  Parser* parser, Expr* expr, TypeSpec* spec, Module* mod) {
  switch (expr->kind) {
  case tkPeriod: // replace with tkEnumMember
    if (expr->left) return;
    expr->left = NEW(Expr); // instead just set type in expr->elemType
    expr->left->kind = tkIdent;
    expr->left->str
      = spec->typeType == TYObject ? spec->type->name : spec->name;
    expr->left->line = expr->line;
    expr->left->col = expr->col;
    resolveVars(parser, expr, mod->scope, false);
  case tkPlus:
  case tkComma:
  case tkEQ:
  case tkNE: expr_setEnumBase(parser, expr->left, spec, mod); fallthrough;
  case tkAssign:
  case tkArrayOpen: expr_setEnumBase(parser, expr->right, spec, mod);
  default:;
  }
}

static void expr_reduceVarUsage(Expr* expr) {
  // vars and subscripts, but also function calls have their usage counts
  // reduced when being involved in an expr that inits a known unused
  // variable.
  switch (expr->kind) {
  case tkString:
    foreach (Expr*, ex, expr->vars)
      expr_reduceVarUsage(ex);
    // if (!--var->used) {
    //     if (expr->var->init) expr_reduceVarUsage(expr->var->init);
    // };
    break;
  case tkSubscript:
  case tkFuncCall:
    if (expr->left) expr_reduceVarUsage(expr->left);
    break;
  case tkSubscriptR:
    if (expr->left) expr_reduceVarUsage(expr->left); // fallthru
  case tkVarDefn:
  case tkIdentR:
    // reduce this var's usage, and if it drops to zero reduce the usage
    // of all vars in this var's init. Sort of like a compile time ref
    // count.
    if (!--expr->var->used) {
      if (expr->var->spec->typeType == TYObject)
        expr->var->spec->type->used--;
      // type_reduceUsage will be called later after
      // whole-func analysis to get rid of type's contents
      if (expr->var->init) expr_reduceVarUsage(expr->var->init);
      // if (expr->var->spec->typeType == TYObject)
      //     --expr->var->spec->type->used;
    }
    break;
  case tkFuncCallR:
    if (expr->left) expr_reduceVarUsage(expr->left);
    if (expr->func->used) expr->func->used--;
    // func_reduce will bedone later
    break;
  case tkPeriod:
    if (expr->left) expr_reduceVarUsage(expr->left);
    break;
  default:
    if (expr->prec) {
      if (!expr->unary && expr->left) expr_reduceVarUsage(expr->left);
      if (expr->right) expr_reduceVarUsage(expr->right);
    };
  }
}

static bool func_calls(Func* func, Func* target) {
  bool ret = false;
  if (!func->visited) {
    func->visited = true;
    foreach (Func*, fn, func->callees) {
      // FIXME: what if intrinsics are recursive? json is. also template
      // instantiations may be (they are also marked intrinsic)
      if (!fn->intrinsic && (fn == target || func_calls(fn, target))) {
        ret = true;
        break;
      }
    }
    func->visited = false;
  }
  return ret;
}

static void func_checkRecursion(Func* func) {
  if (!func->recursivity) func->recursivity = 1 + func_calls(func, func);
}

#define FN_ANALYSE(name)                                                   \
  void expr_analyse##name(Parser* parser, Expr* expr, Scope* scope,        \
    Module* mod, Func* ownerFunc, bool inFuncArgs)
#define SFN_ANALYSE(name) static FN_ANALYSE(name)
#define ANALYSE_(name, expr, scope, inFuncArgs)                            \
  expr_analyse##name(parser, expr, scope, mod, ownerFunc, inFuncArgs)
#define ANALYSE(expr, scope, inFuncArgs) ANALYSE_(, expr, scope, inFuncArgs)

monostatic FN_ANALYSE();

// static void expr_analyse_keyword_match(Parser* parser, Expr* expr,
// Scope* scope,
//     Module* mod, Func* ownerFunc, bool inFuncArgs)
void expr_analyse_keyword_match(Parser* parser, Expr* expr, Scope* scope,
  Module* mod, Func* ownerFunc, bool inFuncArgs) {
  Expr* cond = expr->left;
  if (cond) { // check the match condition. Plus, if it is an enum, go
              // through the case conditions and set enum base on them.
    expr_analyse(parser, cond, scope, mod, ownerFunc, false);
    TypeSpec* tsp = expr_getObjectTypeSpec(cond);
    if (expr->body && tsp && tsp->type && tsp->type->isEnum) {
      foreach (Expr*, cas, expr->body->stmts)
        if (cas->left) expr_setEnumBase(parser, cas->left, tsp, mod);
    }
  }

  // each stmt in the body of a match is a case stmt. Analyse that and
  // check type consistency.
  foreach (Expr*, stmt, expr->body->stmts) {
    if (stmt->kind != tkCase) err_unexpectedExpr(parser, stmt);

    expr_analyse(parser, stmt, expr->body, mod, ownerFunc, false);
    // ^ Case bodies will be analyzed inside (via scope_analyse).

    if (!cond || !stmt->left) {
      // ignore. a syntax error will have been reported before.
    } else if (cond->typeType == TYString
      && stmt->left->typeType == TYRegex) {
    } else if (cond->typeType == TYRegex
      && stmt->left->typeType == TYString) {
    } else if (cond->typeType == TYObject
      && !upcastable(
        expr_getTypeOrEnum(stmt->left), expr_getTypeOrEnum(cond))) {
      err_typeMismatch(parser, cond, stmt->left);
    } else if (stmt->left->typeType != cond->typeType) {
      err_typeMismatch(parser, cond, stmt->left);
    }
  }
}

static void expr_analyse_functionCall(Parser* parser, Expr* expr,
  Scope* scope, Module* mod, Func* ownerFunc, bool inFuncArgs) {
  char buf[256] = {}, sbuf[256] = {};
  char* bufp = buf;
  char* collName = "";
  if (expr->left)
    expr_analyse(parser, expr->left, scope, mod, ownerFunc, true);

  // TODO: need a function to make and return selector
  Expr* arg1 = expr->left;
  if (arg1 && arg1->kind == tkComma) arg1 = arg1->left;
  Type* type = expr_getObjectType(arg1);

  if (arg1 && arg1->collType)
    collName
      = cstr_interp_s(256, "_%s", Collectiontype_name(arg1->collType));
  if (arg1) *bufp++ = '_'; // += sprintf(bufp, "_", typeName);

  int l = cstr_len(expr->str);
  if (expr->str[l - 1] == '!') l--;
  bufp += sprintf(bufp, "%.*s", l, expr->str);
  if (expr->left)
    expr_strarglabels(expr->left, bufp, 256 - ((int)(bufp - buf)));
  Func* found = NULL;
  // if (arg1->typeType == TYObject) {
  do { // fast path when arg labels given
    sprintf(sbuf, "%s%s%s", expr_typeName(arg1), collName, buf);
    sbuf[255] = 0;
    found = mod_getFunc(mod, sbuf);
    if (type) type = type->super ? type->super->type : NULL;
    // arg1->upcast++;//TODO
  } while (type && !found);
  // }
  if (!found && (found = mod_getFuncByTypeMatch(mod, expr))) {
    // take the closest function by type match instead, for now. tell
    // the user this may not be what they expected
    warn_unrecognizedSelector(parser, expr, sbuf, found);
  }
  if (!found && (found = mod_makeFunc(mod, expr))) {
    // TODO: if you are looking in another module for the definition, this
    // will make a new func in that module. In this case that mod should be
    // marked modified. If the module containing the template is the same as
    // the module requesting it then it should not set modified.
    warn_templateHit(parser, expr, sbuf, found);
  }

  if (found) {
    expr->kind = tkFuncCallR;
    expr->func = found;
    expr->func->used++;
    func_analyse(parser, found, mod);
    expr_analyse(parser, expr, scope, mod, ownerFunc, false);
    return;
  }
  if (!strncmp(buf, "Void_", 5))
    err_callingFuncWithVoid(parser, expr, arg1);
  else {
    err_unrecognizedFunc(parser, expr, sbuf);
    expr->typeType = TYError;

    if (*buf != '<') // not invalid type
    {
      int sugg = 0;
      foreach (Func*, func, mod->funcs) {
        if (!strcasecmp(expr->str, func->name)) {
          eprintf("not viable: %s with %d arguments at %s:%d;;", func->psel,
            func->argCount, parser->filename, func->line);
          sugg++;
        }
        if (!func->intrinsic && strcasecmp(expr->str, func->name)
          && leven(expr->str, func->name, expr->slen, func->nameLen) < 3
          && func->argCount == li_count(func->args)) {
          eprintf("did you mean: '%s' (%s at %s:%d);;\n", func->name,
            func->psel, parser->filename, func->line);
          sugg++;
        }
      }
      // if (sugg)
      eputs("\n");
    }
  }
}
static void expr_analyse_functionCallResolved(Parser* parser, Expr* expr,
  Scope* scope, Module* mod, Func* ownerFunc, bool inFuncArgs) {
  // TODO: ownerFunc should not be NULL, even top-level and type implicit
  // ctors should have a func created
  if (ownerFunc) {
    li_shift(&ownerFunc->callees, expr->func);
    li_shift(&expr->func->callers, ownerFunc);
  }
  if (expr_countCommaList(expr->left) != expr->func->argCount)
    err_argsCountMismatch(parser, expr);
  expr->typeType = expr->func->spec ? expr->func->spec->typeType
                                    : TYVoid; // should actually be TYVoid
  expr->collType = expr->func->spec ? expr->func->spec->collType
                                    : CTYNone; // should actually be TYVoid
  expr->elemental = expr->func->elemental;
  expr->canEval = expr->func->canEval;
  if (expr->func->spec) expr->dims = expr->func->spec->dims;
  // isElementalFunc means the func is only defined for (all)
  // Number arguments and another definition for vector args
  // doesn't exist. Basically during typecheck this should see
  // if a type mismatch is only in terms of collType.
  if (!expr->left) return;
  expr->elemental = expr->elemental && expr->left->elemental;
  expr->throws = expr->left->throws || expr->func->throws;
  expr->throws = 1; // any func can throw stack overflow. better way?
  Expr* currArg = expr->left;
  foreach (Var*, arg, expr->func->args) {
    Expr* cArg = (currArg->kind == tkComma) ? currArg->left : currArg;
    if (cArg->kind == tkAssign) {
      if (strcasecmp(cArg->left->str, arg->name))
        err_argLabelMismatch(parser, cArg->left, arg);
      cArg->left->str = arg->name;
      cArg = cArg->right;
    } else {
      // warn_labelMissing(...)
    }
    if (cArg->typeType == TYUnknown //
      && arg->spec->typeType == TYObject && arg->spec->type->isEnum) {
      expr_setEnumBase(parser, cArg, arg->spec, mod);
      expr_analyse(parser, cArg, scope, mod, ownerFunc, false);
    }
    if (cArg->typeType != arg->spec->typeType)
      err_argTypeMismatch(parser, cArg, arg);
    // TODO: check dims mismatch
    // TODO: check units mismatch
    // TODO: set enum base
    if (!(currArg = currArg->right)) break;
  }

  currArg = expr->left;
  while (currArg) {
    Expr* cArg = (currArg->kind == tkComma) ? currArg->left : currArg;
    if (cArg->kind == tkAssign) {
      // LHS will be a tkIdent. You should resolve it to one
      // of the function's arguments and set it to tkArgumentLabel.
      assert(cArg->left->kind == tkIdent);
      Var* theArg = NULL;
      foreach (Var*, arg, expr->func->args) {
        if (!strcasecmp(cArg->left->str, arg->name)) theArg = arg;
      }
      if (!theArg) {
        unreachable("unresolved argument %s!", cArg->left->str);
        // cArg->left->kind = tkIdent;
        // change it back to identifier
      } // TODO: change this to parser error
      else {
        cArg->left->var = theArg;
        cArg->left->kind = tkIdentR;
      }
    }

    currArg = currArg->kind == tkComma ? currArg->right : NULL;
  }
}
static void expr_analyse_arrayOpen(Parser* parser, Expr* expr, Scope* scope,
  Module* mod, Func* ownerFunc, bool inFuncArgs) {
  expr->collType = CTYArray;
  if (expr->right) {
    expr_analyse(parser, expr->right, scope, mod, ownerFunc, false);
    expr->typeType = expr->right->typeType;
    expr->collType
      = expr->right->kind == tkSemiColon ? CTYTensor : CTYArray;
    expr->dims = expr->right->kind == tkSemiColon ? 2 : 1;
    // using array literals you can only init 1D or 2D
    if (expr->typeType == TYObject) {
      // you need to save the exact type of the elements, it's not a
      // primitive type. You'll find it in the first element.
      Expr* first = expr->right;
      while (ISIN(2, first->kind, tkComma, tkSemiColon))
        first = first->left;
      switch (first->kind) {
      case tkIdentR:
        expr->elementType = first->var->spec->type;
        if (first->var->spec->dims || first->var->spec->collType != CTYNone)
          unreachable("trying to make array of arrays %d", expr->line);
        break;
      case tkFuncCallR:
        expr->elementType = first->func->spec->type;
        if (first->func->spec->dims
          || first->var->spec->collType != CTYNone)
          unreachable("trying to make array of arrays line %d", expr->line);
        break;
      default:
        break;
        // TODO: object init literals
        // case tkObjectInitResolved:
        // expr->elementType = first->var->spec->type;break;
      }
      // expr->elementType =
    }
  }
}
static void expr_analyse_braceOpen(Parser* parser, Expr* expr, Scope* scope,
  Module* mod, Func* ownerFunc, bool inFuncArgs) {
  expr_analyse(parser, expr->right, scope, mod, ownerFunc, true);
  // TODO: you told expr_analyse to not care about what's on the
  // LHS of tkAssign exprs. Now you handle it yourself. Ensure that
  // they're all of the same type and set that type to the expr
  // somehow.
  analyseDictLiteral(parser, expr->right, mod);
  expr->typeType = expr->right->typeType;

  if (expr->typeType == TYObject) {
    // you need to save the exact type of the elements, it's not a
    // primitive type. You'll find it in the first element.
    Expr* first = expr->right;
    while (first->kind == tkComma) first = first->left;
    if (first->kind == tkAssign) first = first->right;
    // we care about the value in the key-value pair. We'll figure
    // out the key type later or not, whatever.
    switch (first->kind) {
    case tkIdentR: expr->elementType = first->var->spec->type; break;
    case tkFuncCallR: expr->elementType = first->func->spec->type; break;
    default:
      expr->elementType = NULL;
      break;
      // TODO: object init literals
      // case tkObjectInitResolved:
      // expr->elementType = first->var->spec->type;break;
    }
  }
  expr->collType = CTYDict;
  expr->dims = 1;

  // these are only Dicts! Sets are normal [] when you detect they are
  // only used for querying membership.
  // TODO: what were the gazillion Dict subtypes for?
}
static void expr_analyse_varAssign(Parser* parser, Expr* expr, Scope* scope,
  Module* mod, Func* ownerFunc, bool inFuncArgs) {

  Expr* const init = expr->var->init;
  TypeSpec* const spec = expr->var->spec;

  if (spec->typeType == TYUnknown)
    resolveTypeSpec(parser, spec, mod,
      ownerFunc ? ownerFunc->params : NULL); // what if its an owner type?

  if (!init) {
    // if the typespec is given, generate the init expr yourself
    // this is only for basic types like primitives.
    // and arrays of anything can be left without an init.
    if (!spec->dims) // goto errorMissingInit;
      switch (spec->typeType) {
      case TYReal64: expr->var->init = expr_const_0; break;
      case TYString: expr->var->init = expr_const_empty; break;
      case TYBool: expr->var->init = expr_const_no; break;
      case TYObject:
        if (!spec->type->isEnum) {
          expr->var->init = expr_const_nil;
          break;
        }
      default:
        // errorMissingInit:
        // this is an error, no way to find out what you want
        err_missingInit(parser, expr);
      }
  } else {

    // first try to set enum base if applicable.
    if (spec->typeType == TYObject && spec->type->isEnum)
      expr_setEnumBase(parser, init, spec, mod);

    expr_analyse(parser, init, scope, mod, ownerFunc, false);

    if (!expr->var->used) { expr_reduceVarUsage(init); }
    // TODO: this should also be done for += -= *= etc.

    if (init->typeType != TYNil) {
      // if not nil, get type info from init expr.
      expr->typeType = init->typeType;
      expr->collType = init->collType;
      expr->nullable = init->nullable;
      expr->elemental = init->elemental;
      expr->throws = init->throws;
      expr->impure = init->impure;
      expr->dims = init->dims;
    } else if (spec->typeType == TYObject) {
      // if nil, and typespec given and resolved, set type info from
      // typespec.
      expr->typeType = spec->typeType;

    } else if (spec->typeType == TYUnknown) {
      // this will already have caused an error while resolution.
      // since specified type is unresolved and init is nil, there
      // is nothing to do except to mark it an error type.
      spec->typeType = expr->typeType = TYError;

    } else {
      // this would be something like init'ing primitives with nil.
      err_initMismatch(parser, expr);
      spec->typeType = expr->typeType = TYError;
    }

    if (spec->typeType == TYUnknown) {
      spec->typeType = init->typeType;
      if (init->typeType == TYObject) {
        if (init->kind == tkFuncCallR) {
          expr->var->spec = init->func->spec;
          // ^ TODO: DROP old expr->var->spec!!
          spec->type = init->func->spec->type;
          spec->dims = init->func->spec->dims;
        } else if (init->kind == tkIdentR) {
          expr->var->spec = init->var->spec;
          spec->type = init->var->spec->type;
          spec->dims = init->var->spec->dims;
        } else if (init->kind == tkArrayOpen)
          spec->type = init->elementType;
        else if (init->kind == tkBraceOpen)
          spec->type = init->elementType;
        else if (init->kind == tkPeriod) {
          Expr* e = init;
          if (init->left->var->spec->type->isEnum) {
            spec->type = init->left->var->spec->type;
          } else {
            while (e->kind == tkPeriod) e = e->right;
            // at this point, it must be a resolved ident or
            // subscript
            spec->type = e->var->spec->type;
            spec->dims = e->var->spec->dims;
          }

        } else {
          unreachable("%s", "var type inference failed");
        }
        type_analyse(parser, spec->type, mod);
      }
    } else if (spec->typeType != init->typeType
      // && init->typeType != TYNil
    ) {
      // init can be nil, which is a TYNil
      err_initMismatch(parser, expr);
      expr->typeType = TYError;
    }

    if (spec->dims == 0) {
      spec->collType = init->collType;
      spec->dims = init->collType == CTYTensor ? init->dims
        : init->collType == CTYArray           ? 1
        : init->collType == CTYDict            ? 1
                                               : 0;
    } else if (spec->dims != 1 && init->collType == CTYArray) {
      err_initDimsMismatch(parser, expr, 1);
      expr->typeType = TYError;
    } else if (spec->dims != 2 && init->collType == CTYTensor) {
      err_initDimsMismatch(parser, expr, 2);
      expr->typeType = TYError;

    } else if (spec->dims != 0 && init->collType == CTYNone) {
      err_initDimsMismatch(parser, expr, 0);
      expr->typeType = TYError;
    }
  }
}

monostatic void expr_analyse(Parser* parser, Expr* expr, Scope* scope,
  Module* mod, Func* ownerFunc, bool inFuncArgs) {
  switch (expr->kind) {
  case tkFuncCallR:
    expr_analyse_functionCallResolved(
      parser, expr, scope, mod, ownerFunc, inFuncArgs);
    break;

  case tkFuncCall:
    expr_analyse_functionCall(
      parser, expr, scope, mod, ownerFunc, inFuncArgs);
    break;

  case tkVarDefn:
    expr_analyse_varAssign(parser, expr, scope, mod, ownerFunc, inFuncArgs);
    break;

  case tkMatch:
    expr_analyse_keyword_match(
      parser, expr, scope, mod, ownerFunc, inFuncArgs);
    break;

  case tkFor: {
    if (expr->left)
      expr_analyse(parser, expr->left->right, scope, mod, ownerFunc, false);
    scope_analyse(parser, expr->body, ownerFunc, mod);
  } break;

  case tkElse:
  case tkIf:
  case tkElif:
  case tkWhile:
  case tkCase: {
    if (expr->left)
      expr_analyse(parser, expr->left, scope, mod, ownerFunc, false);

    scope_analyse(parser, expr->body, ownerFunc, mod);
    // foreach (Expr*, stmt, expr->body->stmts)
    //   expr_analyse(parser, stmt, expr->body, mod, ownerFunc, false);
  } break;

    // -------------------------------------------------- //
  case tkSubscriptR: {
    // assert(expr->left->kind == tkArrayOpen);
    int nhave = expr_countCommaList(expr->left);
    if (nhave != expr->var->spec->dims)
      err_indexDimsMismatch(parser, expr, nhave);
    // }   if (expr->kind == tkSubscriptR) {
    expr->typeType = expr->var->spec->typeType;
    if (expr->left->elemental) {
      // expr->collType =CTYSlice;// expr->var->spec->collType;
      expr->dims = 1; // FIXME: this will be dims of the array - num of
                      // singular (non-range) indices
    } else {
      expr->collType = CTYNone; // expr->var->spec->collType;
      expr->dims = 0;
    }
    // TODO: since it is a subscript, if it has no left (i.e. arr[])
    // i'm guessing it is a full array, and therefore can be an
    // elemental op
    expr->elemental = expr->left ? expr->left->elemental : true;

    // eval is possible only when the index AND the var can be eval'd
    if (expr->var->init && expr->left)
      expr->canEval = expr->var->init->canEval & expr->left->canEval;

    // TODO: check args in the same way as for funcs below, not
    // directly checking expr->left.}
    // TODO: dims is actually a bit complicated here. For each dim
    // indexed with a scalar (not a range), you reduce the dim of the
    // subscript result by 1. For now the default is to keep the dims at
    // 0, which is what it would be if you indexed something with a
    // scalar index in each dimension.
    // expr_analyse for a : op should set dims to 1. Then you just
    // walk the comma op in expr->right here and check which index exprs
    // have dims=0. Special case is when the index expr is a logical,
    // meaning you are filtering the array, in this case the result's
    // dims is always 1.
  } break; // index expr has already been analyzed before resolution
           // fallthru
  case tkSubscript:
    expr->typeType = TYError; // must have been resolved
    if (expr->left)
      expr_analyse(parser, expr->left, scope, mod, ownerFunc, false);
    break;

  case tkString:
  case tkRawString:
    expr->typeType = TYString;
    expr->canEval = true;
    expr_prepareInterp(parser, expr, scope);
    break;

  case tkRegexp:
    expr->typeType = TYRegex;
    expr->canEval = true;
    expr->extract = true;
    break;

  case tkNumber:
    expr->typeType = TYReal64;
    expr->canEval = true;
    break;

  case tkYes:
  case tkNo:
    expr->typeType = TYBool;
    expr->canEval = true;
    break;

  case tkNil: expr->typeType = TYNil; break;

  case tkIdent:
    // by the time analysis is called, all vars must have been resolved
    // err_unrecognizedVar(parser, expr);
    // func call labels, enum names etc remain identifiers
    expr->typeType = TYError;
    break;

  case tkIdentR:
    expr->typeType = expr->var->spec->typeType;
    expr->collType = expr->var->spec->collType;
    expr->elemental = expr->collType != CTYNone;
    expr->dims = expr->var->spec->dims;
    if (expr->var->init) expr->canEval = expr->var->init->canEval;

    // this was done just to ensure enums are caught and analysed in
    // non-lint mode. In lint mode they are done anyway.
    // TODO: remove this, you shouldnt be doing it on each var use
    // it will be too intensive. let it just be done on each var
    // decl and you figure out a way to set the enum type on var
    // decls.
    if (expr->typeType == TYObject)
      type_analyse(parser, expr->var->spec->type, mod);
    break;

  case tkArrayOpen:
    if (expr->right) {
      expr_analyse_arrayOpen(
        parser, expr, scope, mod, ownerFunc, inFuncArgs);
      expr->canEval = expr->right->canEval;
    }
    break;

  case tkBraceOpen:
    if (expr->right) {
      expr_analyse_braceOpen(
        parser, expr, scope, mod, ownerFunc, inFuncArgs);
      expr->canEval = expr->right->canEval;
    }
    break;

  case tkWhen: {
    // the var that was passed in has an init and maybe marked isVar.
    if (expr->func->argCount) {
      Var *event, *selfv = expr->func->args->item; // its the first arg.
      if (!(event = type_getVar(selfv->spec->type, expr->func->name)))
        err_unrecognizedMember(parser, selfv->spec->type,
          &(Expr) { .kind = tkIdent,
            .str = expr->func->name,
            .line = expr->func->line,
            .col = expr->func->col });
      else
        event->used++;
      // selfv = NEWW(
      //   Var, .name = selfv->name, .spec = selfv->spec, .isVar = false);
      // ^ the var's type will have been inferred by now.
      expr->func->args->item = selfv;
      expr->func->sel = expr->func->psel = cstr_pclone(cstr_interp_s(128, //
        "%s_%s_%d", selfv->name, expr->func->name, expr->func->line));
      // ^ selectors are not important (these are not callable funcs) but
      // thing is getSelectors() wont have been called on the handler since
      // it wasnt in the list when analyse started.
    } else {
      expr->func->sel = expr->func->psel = expr->func->name;
    }

    li_shift(&mod->funcs, expr->func);
    // ^ it can't be added to the module during parsing, because it would
    // precede its containing function in the list. Before the containing
    // func is analysed the type of the target var may not be known & usages
    // of it in the handler cannot be analysed (will find type invalid).

    func_analyse(parser, expr->func, mod);
    // TODO: need a func->isGenerated flag so that it doesnt show up in
    // linter output.
  } break;

  case tkPeriod: {
    Expr *member = expr->right, *base = expr->left;

    if (!base && !inFuncArgs) {
      err_noEnumInferred(parser, member);
      break;
    }
    if (!base) break;

    // if (expr->left->kind == tkPeriod)
    //   expr_analyse(parser, expr->left, scope, mod, ownerFunc, false);

    assert(base->kind == tkPeriod //
      || base->kind == tkIdentR   //
      || base->kind == tkIdent);
    // if (expr->left->kind == tkIdentR || expr->left->kind == tkIdent)
    expr_analyse(parser, base, scope, mod, ownerFunc, false);

    // The name/type resolution of expr->left may have failed.
    if (!base->typeType) break;

    if (base->kind == tkPeriod) { base = base->right; }

    if (!member) break; // fixme: is this possible?

    if (!ISIN(3, member->kind, tkIdent, tkSubscript, tkFuncCall)) {
      err_unexpectedExpr(parser, member);
      break;
    }

    if (ISIN(2, member->kind, tkSubscript, tkFuncCall) && member->left)
      expr_analyse(parser, member->left, scope, mod, ownerFunc, false);

    if (base->kind != tkIdentR) { break; }
    // error will have been shown for it already
    // err_unexpectedExpr(parser, member);

    if (ISIN(2, base->var->spec->typeType, TYError, TYVoid)) {
      expr->typeType = TYError;
      break;
    }

    // left of . must be object; unless right is a func call
    if (!(member
          && (base->var->spec->typeType == TYObject
            || ISIN(2, member->kind, tkFuncCall, tkFuncCallR)))) {
      unreachable("invalid operands for . operator at %d:%d: %s and %s\n",
        expr->line, expr->col,
        expr->left ? TokenKind_names[expr->left->kind] : "(null)",
        expr->right ? TokenKind_names[expr->right->kind] : "(null)");
      expr->typeType = TYError;
      break;
    }
    // || member->kind == tkFuncCall || member->kind == tkFuncCallR);

    Type* type = base->var->spec->type;
    if (!type) {
      expr->typeType = TYError;
      break;
    }

    // Resolve the member in the scope of the type definition.
    switch (member->kind) {
    case tkIdent:
    case tkSubscript: {
      TokenKind ret = (member->kind == tkIdent) ? tkIdentR : tkSubscriptR;
      Var* found = NULL;
      if (type->body) found = scope_getVar(type->body, member->str);
      if (found) {
        member->kind = ret;
        member->var = found;
        member->var->used++;
        expr_analyse(parser, member, scope, mod, ownerFunc, false);
      } else {
        err_unrecognizedMember(parser, type, member);
        expr->typeType = TYError;
      }
    } break;
    case tkFuncCall: {
      Expr* args = member->left;
      Expr* dummy = base;
      if (args) {
        dummy = &(Expr) { //
          .kind = tkComma,
          .left = base,
          .right = args
        };
      }
      member->left = dummy;
      expr_analyse(parser, member, scope, mod, ownerFunc, inFuncArgs);
      member->left = args;
    } break;
    default:
      err_syntax(parser, member, "invalid member");
      eputs("NYI\n");
      expr->typeType = TYError;
    }

    // Name resolution may fail...
    if (expr->typeType == TYError) break;

    expr->typeType = type->isEnum ? TYObject : expr->right->typeType;
    expr->collType = expr->right->collType;
    expr->elemental = expr->right->elemental;
    expr->canEval = expr->right->canEval;
    expr->dims = expr->right->dims;

  } break;

  case tkComment:
    break;

    // case tkArgumentLabel: break;

  case tkYield:
    if (expr->right)
      expr_analyse(parser, expr->right, scope, mod, ownerFunc, false);

    // how to deal with multiple return values?
    ownerFunc->yields = true;
    ownerFunc->spec->collType = CTYIterator;
    break;

  default:
    if (expr->prec) {
      if (!expr->unary && expr->left)
        expr_analyse(parser, expr->left, scope, mod, ownerFunc,
          inFuncArgs
            && ISIN(3, expr->left->kind, tkComma, tkAssign, tkPeriod));
      // some exprs like return can be used without any args

      if (ISIN(5, expr->kind, tkIn, tkNotin, tkAssign, tkEQ, tkNE)) {
        TypeSpec* spec = expr_getObjectTypeSpec(expr->left);
        if (spec && spec->typeType == TYObject && spec->type->isEnum) {
          expr_setEnumBase(parser, expr->right, spec, mod);
        } else {
          // FIXME FIXME FIXME why is this repeated
          spec = expr_getObjectTypeSpec(expr->left);
          if (spec && spec->typeType == TYObject && spec->type->isEnum) {
            expr_setEnumBase(parser, expr->right, spec, mod);
          }
        }
      }

      if (expr->right)
        expr_analyse(parser, expr->right, scope, mod, ownerFunc,
          inFuncArgs
            && ISIN(3, expr->right->kind, tkComma, tkAssign, tkPeriod));

      if (expr->kind == tkCheck && expr->right->typeType != TYBool) {
        err_typeWrong(parser, expr->right, TYBool);
      } else if (expr->kind == tkUnaryMinus
        && expr->right->typeType < TYInt8) {
        if (!(parser->godMode && expr->right->typeType == TYString))
          err_typeWrong(parser, expr->right, TYReal64);
      } else if (expr->kind == tkOr && expr->left->typeType != TYBool) {
        // Handle the special 'or' keyword used to provide alternatives
        // for a nullable expression.

        TypeTypes leftType = expr->left ? expr->left->typeType : TYError;
        TypeTypes rightType = expr->right ? expr->right->typeType : TYError;
        switch (expr->right->kind) {
        case tkReturn: break;
        default:
          if (leftType != rightType) {
            err_typeMismatchBinOp(parser, expr);
            expr->typeType = TYError;
          } else if (expr->right) {
            expr->typeType = expr->right->typeType;
            expr->collType = expr->right->collType;
          }
        }

      } else if (isCmpOp(expr) || isBoolOp(expr)) {
        // Handle comparison and logical operators (always return a
        // bool)
        expr->typeType
          = (expr->right->typeType == TYError
              || (!expr->unary && expr->left->typeType == TYError))
          ? TYError
          : TYBool;
      } else {
        // Set the type from the ->right expr for now. if an error
        // type is on the right, this is accounted for.
        if (expr->right) {
          expr->typeType = expr->right->typeType;
          expr->collType = expr->right->collType;
          expr->canEval = expr->right->canEval;
        }
      }

      if (expr->right)
        expr->elemental = expr->right->elemental || expr->kind == tkColon;
      // TODO: actually, indexing by an array of integers is also an
      // indication of an elemental op

      if (!expr->unary && expr->left) {
        expr->elemental = expr->elemental || expr->left->elemental;
        expr->dims = expr->right->dims;
        expr->canEval = expr->right->canEval && expr->left->canEval;
        // if (expr->kind == tkColon and expr->right->kind ==
        // tkColon)
        //     expr->right->dims = 0; // temporarily set it to 0 to
        //     allow
        // parsing stepped ranges 1:s:n

        if (expr->dims != expr->left->dims
          && !(inFuncArgs
            && (expr->kind == tkComma || expr->kind == tkAssign))) {
          // if either one has 0 dims (scalar) it is an elemental op
          // with a scalar.
          if (expr->left->dims != 0 && expr->right->dims != 0) {
            err_binOpDimsMismatch(parser, expr);
            expr->right->typeType = TYError;
          } else if (ISIN(6, expr->kind, tkPlus, tkMinus, tkTimes, tkSlash,
                       tkPower, tkMod)) {
            expr->dims = expr->left->dims + expr->right->dims;
            expr->collType
              = max(expr->left->collType, expr->right->collType);
            // todo: stop distinguishing array and tensor!!! then
            // you dont need this. this strongly depends on the op &
            // is too much repeated work
            // eprintf("ok `[+-*/^%]` dims@ %d %d %d %d\n",
            // expr->line,
            //     expr->col, expr->left->dims, expr->right->dims);
          } else if ((expr->kind == tkIn || expr->kind == tkNotin)
            && expr->left->dims == 0 && expr->right->dims == 1) {
            // eprintf("ok `in` dims@ %d %d %d %d\n", expr->line,
            //     expr->col, expr->left->dims, expr->right->dims);
          } else if (expr->kind == tkColon //
            && expr->left->dims == 1 && expr->left->kind == tkColon
            && expr->right->dims == 0) {
            // eprintf("ok `:` dims@ %d %d %d %d\n", expr->line,
            //     expr->col, expr->left->dims, expr->right->dims);
            // expr->dims = 1;
          } else {
            err_binOpDimsMismatch(parser, expr);
          }

          // ^ TODO: but you can also have some ops on 2D and 1D
          // operands e.g. linear solve. what about those?
        } else {
          // eprintf("(ignore) dims@ %d %d %d %d\n", expr->line,
          //     expr->col, expr->left->dims, expr->right->dims);
        }
        // ranges always create a 1D entity (not always array, but 1D)
        if (expr->kind == tkColon) {
          expr->dims = 1;
          expr->collType = CTYRange;
        }
      }
      if (!expr->unary && expr->left
        && !(inFuncArgs
          && (expr->kind == tkComma || expr->kind == tkAssign))) {
        // ignore , and = inside function call arguments. thing is
        // array or dict literals passed as args will have , and =
        // which should be checked. so when you descend into their
        // args, unset inFuncArgs.
        TypeTypes leftType = expr->left->typeType;
        TypeTypes rightType = expr->right->typeType;

        if (leftType == TYBool
          && (expr->kind == tkLE || expr->kind == tkLT)) {
          // Special case: chained LE/LT operators: e.g. 0 <= yCH4
          // <= 1.

        } else if ((expr->kind == tkAssign || expr->kind == tkEQ
                     || expr->kind == tkNE)
          && ((leftType == TYObject && rightType == TYNil)
            || (leftType == TYNil && rightType == TYObject))) {
          // a == nil or nil !=a etc.

        } else if (leftType == TYUnknown
          && ISIN(2, expr->left->kind, tkIdentR, tkSubscriptR)
          && ISIN(2, expr->kind, tkAssign, tkEQ)) {
          // non-local inference. something wasnt resolved earlier, so try
          // on a subsequent assignment (or eq test?)
          if ((expr->left->var->spec->typeType = expr->right->typeType)
            == TYObject)
            expr->left->var->spec->type = expr_getTypeOrEnum(expr->right);
          // TODO: expr->left->var->inferline = expr->line;

        } else if (leftType != rightType) {
          // Type mismatch for left and right operands is always
          // an error.
          err_typeMismatchBinOp(parser, expr);
          expr->typeType = TYError;
        } // from now on, lefttype == righttype holds _________________
        else if (leftType == TYString
          && (ISIN(3, expr->kind, tkAssign, tkEQ, tkNE))) {
          // Allow assignment, equality test and != for strings.
          // TODO: might even allow comparison operators, and
          // perhaps allow +=, or better .= or something. Or
          // perhaps append(x!) is clearer
          ;
        } else if (leftType == TYBool
          && ISIN(
            6, expr->kind, tkAssign, tkEQ, tkAnd, tkOr, tkNot, tkNotin))
          ;
        else if (isArithOp(expr)
          && (!Typetype_isnum(leftType) || !Typetype_isnum(rightType))) {
          // Arithmetic operators are only relevant for numeric
          // types.
          // if(  leftType==TYObject&&expr->left->)
          // TODO: allow enum +
          err_invalidTypeForOp(parser, expr);
        }
        // check if an error type is on the left, if yes, set the
        // expr type
        if (leftType == TYError) expr->typeType = leftType;
      }
      //  else {
      //   unreachable("woops: %s", TokenKind_names[expr->kind]);
      //   ;
      // }
      // TODO: here statements like return etc. that are not binary
      // but need to have their types checked w.r.t. an expected type
      // TODO: some ops have a predefined type e.g. : is of type Range
      // etc,
    } else {
      unreachable("unknown expr kind: %s", TokenKind_names[expr->kind]);
    }
  }
}

static void scope_analyse(
  Parser* parser, Scope* scope, Func* ownerFunc, Module* mod) {

  foreach (Expr*, stmt, scope->stmts)
    expr_analyse(parser, stmt, scope, mod, ownerFunc, false);

  // if (isCtrlExpr(stmt) && stmt->body)
  //   scope_checkUnusedVars(parser, stmt->body);
  foreach (Var*, var, scope->locals) {
    if (!var->used) {
      warn_unusedVar(parser, var);
    } else if (!var->usedInSameScope && !var->usedInChildScope) {
      hint_varRefineScope(parser, var);
    }

    if (var->escapes) { hint_varEscapes(parser, var); }
    if (var->promote) { hint_varPromoteScope(parser, var); }
  }
}

static void type_analyse(Parser* parser, Type* type, Module* mod) {
  if (type->analysed) return;
  if (type->super) {
    resolveTypeSpec(parser, type->super, mod, type->params);
  }

  // foreach (TypeSpec*, spec, type->params)
  //   if (mod_getType(spec->name))err_duplicateType(parser,);

  foreach (Type*, type2, mod->types) {
    if (type2 == type) break;
    if (!strcasecmp(type->name, type2->name)) {
      if (type->isEnum) {
        err_duplicateEnum(parser, type, type2);
      } else {
        err_duplicateType(parser, type, type2);
      }
    }
  }
  if (strchr(type->name, '_')) err_invalidTypeName(parser, type);
  if (!isupper(*type->name)) err_invalidTypeName(parser, type);

  // Mark the semantic pass as done for this type, so that recursive
  // paths through calls found in initializers will not cause the compiler
  // to recur. This might be a problem if e.g. the type has a, b, c and
  // the initializer for b has a dependency on the type's .c member, whose
  // type has not been set by the time b's initializer is processed.
  // However if you set analysed after all statements, then you risk
  // getting caught in a recursive path. One way to fix it is to have
  // granularity at the member level, so not entire types but their
  // individual members are processed. In that case the only problem can
  // be a recursive path between a member var and a function that it calls
  // in its initializer.
  type->analysed = true;
  // nothing to do for declared/empty types etc. with no body

  // scope_analyse will report unused vars right away, but you have to scan
  // the entire module to know all usages and only then you can report
  // unused type members. so just analyse exprs now & wait for later to
  // report unused members.
  // if (type->body) scope_analyse(parser, type->body, NULL, mod);
  //
  foreach (Expr*, stmt, type->body->stmts)
    expr_analyse(parser, stmt, type->body, mod, NULL, false);
}
static void func_hashExprs(Parser* parser, Func* func);

static void func_analyse(Parser* parser, Func* func, Module* mod) {
  if (func->analysed) return;
  // eprintf("expr_analyse: %s at ./%s:%d\n", func->sel,
  // parser->filename, func->line);

  bool isCtor = false;
  // Check if the function is a constructor call and identify the type.
  // TODO: this should be replaced by a dict query
  foreach (Type*, type, mod->types) {
    if (!strcasecmp(func->name, type->name)) {
      if (func->spec && !(func->isStmt || func->isDefCtor))
        err_ctorHasType(parser, func, type);
      if (!func->spec) {
        func->spec = spec_new(TYObject, CTYNone);
        func->spec->type = type; // Ctors AlWAYS return a new object.
        func->returnsNewObjectAlways = true; // even Ctors with args.
      }
      // TODO: isStmt Ctors should have the correct type so e.g.
      // you cannot have
      // Point(x as Number) := 3 + 5 * 12
      // but must return a Point instead. This cannot be enforced
      // here since type resolution hasn't been done at this
      // stage. Check this after the type inference step when the
      // stmt func has its return type assigned.
      // if (func->isStmt)
      //     err_ctorHasType(this, func, type);
      if (!isupper(*func->name)) warn_ctorCase(parser, func);

      func->name = type->name;
      isCtor = true;
    }
  }

  // Capitalized names are not allowed unless they are constructors.
  if (!func->isDefCtor && !isCtor && isupper(*func->name))
    err_unrecognizedCtor(parser, func);

  // The rest of the processing is on the contents of the function.
  if (!func->body) {
    func->analysed = true;
    return;
  }

  // Check for duplicate functions (same selectors) and report errors.
  // TODO: this should be replaced by a dict query
  foreach (Func*, func2, mod->funcs) {
    if (func == func2) break;
    if (!strcasecmp(func->sel, func2->sel))
      err_duplicateFunc(parser, func, func2);
  }

  // Mark the semantic pass as done for this function, so that recursive
  // calls found in the statements will not cause the compiler to recur.
  func->analysed = true;

  // Run the statement-level semantic pass on the function body.
  scope_analyse(parser, func->body, func, mod);
  // foreach (Expr*, stmt, func->body->stmts)
  //   expr_analyse(parser, stmt, func->body, mod, func, false);

  // Check unused variables in the function and report warnings.
  // func_checkUnusedVars(parser, func);

  foreach (Var*, arg, func->args)
    if (!arg->used) warn_unusedArg(parser, arg);
  // Statement functions are written without an explicit return type.
  // Figure out the type (now that the body has been analyzed).
  if (func->isStmt) setStmtFuncTypeInfo(parser, func);
  // TODO: for normal funcs, expr_analyse should check return statements
  // to have the same type as the declared return type.

  // if you could not infer the type of ans, it must be void
  if (!func->spec->typeType) func->spec->typeType = TYVoid;

  // this is done here so that linter can give hints on what is picked up
  // for CSE. This func should not modify the Jet except marking CSE
  // candidates!
  func_hashExprs(parser, func);

  // Do optimisations or ANY lowering only if there are no errors
  if (!parser->issues.errCount && parser->mode != PMLint) {

    // Handle elemental operations like arr[4:50] = mx[14:60] + 3
    scope_scalarize(func->body);
    // Extract subexprs like count(arr[arr<1e-15]) and promote them to
    // full statements corresponding to their C macros e.g.
    // Number _1; Array_count_filter(arr, arr<1e-15, _1);
    scope_lower(func->body);
  }
}

static void test_analyse(Parser* parser, Test* test, Module* mod) {
  if (!test->body) return;
  if (*test->name == '-') return;

  // Check for duplicate test names and report errors.
  // TODO: this should be replaced by a dict query
  foreach (Test*, test2, mod->tests) {
    if (test == test2) break;
    if (!strcasecmp(test->name, test2->name))
      err_duplicateTest(parser, test, test2);
  }

  // Run the statement-level semantic pass on the function body.
  // Don't bother setting visited since tests cannot be recursive (they
  // can't be called at all)
  scope_analyse(parser, test->body, NULL, mod);
  // foreach (Expr*, stmt, test->body->stmts)
  //   expr_analyse(parser, stmt, test->body, mod, NULL, false);

  // Check unused variables in the function and report warnings.
  // JetTest_checkUnusedVars(parser, test);
  // Do optimisations or ANY lowering only if there are no errors
  if (!parser->issues.errCount && parser->mode != PMLint) {
    scope_scalarize(test->body);
    scope_lower(test->body);
  }
}

static void func_reduceUsage(Func* func) {
  if (func->visited) return; // don't get stuck in recursive funcs
  func->visited = true;
  foreach (Var*, arg, func->args) // reduce usage of each arg type
    if (arg->spec->typeType == TYObject) arg->spec->type->used--;
  // reduce usage of the return type
  if (func->spec && func->spec->typeType == TYObject)
    func->spec->type->used--;
  foreach (Func*, fn, func->callees) // reduce usage of each callee
    if (fn->used)
      if (!--fn->used) func_reduceUsage(fn);
  // TODO: you need to walk all exprs, descend scopes & fish out types &
  // ctors & reduce their usage. this won't be complete until then.
  func->visited = false;
}

static void type_reduceUsage(Type* type) {
  if (type->visited) return;
  type->visited = true;
  foreach (Expr*, stmt, type->body->stmts) {
    if (stmt->kind == tkVarDefn) {
      stmt->var->used = 0; // wipe out all members
      if (stmt->var->init) expr_reduceVarUsage(stmt->var->init);
    }
  }
  type->visited = false;
}

static void mod_unmarkTypesVisited(Module* mod);
static int expr_markTypesVisited(Parser* parser, Expr* expr);
static int type_checkCycles(Parser* parser, Type* type);

void mod_analyse(Parser* parser, Module* mod) {
  // FUNC_ENTRY
  // If function calls are going to be resolved based on the type of
  // first arg, then ALL functions must be visited in order to
  // generate their selectors and resolve their typespecs. (this does
  // not set the resolved flag on the func -- that is done by the
  // semantic pass)
  foreach (Func*, func, mod->funcs) {
    // foreach (TypeSpec*, spec, func->params)
    //   if (mod_getType(spec->name))err_duplicateType(parser,);
    foreach (Var*, arg, func->args)
      resolveTypeSpec(parser, arg->spec, mod, func->params);
    if (func->spec) resolveTypeSpec(parser, func->spec, mod, func->params);
    getSelector(func);
  }

  // If we are linting,
  //     the whole file must be analysed.this happens
  // regardless of whether start was found or not
  // if (parser->mode == PMTest || parser->mode == PMLint) {
  foreach (Expr*, stmt, mod->scope->stmts)
    expr_analyse(parser, stmt, mod->scope, mod, NULL, false);
  // foreach (Var*, var, mod->scope->locals)
  //     if (var->init)
  //         expr_analyse(parser, var->init, mod->scope, mod, false);
  foreach (Test*, test, mod->tests) { test_analyse(parser, test, mod); }
  foreach (Func*, func, mod->funcs) { func_analyse(parser, func, mod); }
  foreach (Type*, type, mod->types) { type_analyse(parser, type, mod); }
  foreach (Type*, en, mod->enums) { type_analyse(parser, en, mod); }
  // Check dead code -- unused funcs and types, and report warnings.

  foreach (Func*, func, mod->funcs) { func_checkRecursion(func); }

  foreach (Func*, func, mod->funcs)
    if (/*!func->intrinsic
      &&*/
      (!func->used || !func->analysed
        // || (func->recursivity > 1 && func->used == 1)
        // TODO: setting unused is tricky when there is a cycle. just
        // like refcounting GC!
        )) {
      func_reduceUsage(func);
      Type* t = mod_getType(mod, func->name);
      if (t && !--t->used) type_reduceUsage(t);
    }

  foreach (Func*, func, mod->funcs)
    if (!func->intrinsic
      && (!func->used || (!func->analysed && !func->isDefCtor)))
      warn_unusedFunc(parser, func);

  foreach (Type*, type, mod->types)
    if (!type->used || !type->analysed) type_reduceUsage(type);

  foreach (Type*, type, mod->types) {
    if (!type->used || !type->analysed) warn_unusedType(parser, type);
    if (type->body) foreach (Var*, var, type->body->locals)
        if (!var->used) warn_unusedVar(parser, var);
    // actually type membs can be used across modules, but their usage
    // counts will be set correctly even from usages in other mods. warning
    // about unused members is unnecessary, when you emit C you can just
    // skip them. Works because all modules will have been processed by the
    // time you call emit.
    // at some point also sort type members by size.
  }

  foreach (Type*, type, mod->enums)
    if (!type->used || !type->analysed) type_reduceUsage(type);

  foreach (Type*, type, mod->enums)
    if (!type->used || !type->analysed) warn_unusedType(parser, type);

  // now that funcs are marked recursive you can do a second pass
  // analysis,
  // which deals with var storage decisions, inlining,  etc. or perhaps
  // this pass can be called 'optimising'.

  // check each stmt in each type to find cycles.
  foreach (Type*, type, mod->types)
    if (type->analysed && type->body && !type->visited) {
      if (type_checkCycles(parser, type)) {
        // cycle was detected. err has been reported along with a
        // backtrace. now just unset the dim control codes.
        eprintf(" ...%s\n", "\e[0m");
        // just report the first cycle found. typically there will
        // be only one cycle and you will end up reporting the same
        // cycle for all types that are in it, which is useless.
        // break;
        // the last type (for which the error was reported) won't
        // have its cycle check flags cleared, but who cares. OTHER
        // IDEA: clear the flag only if there was no error. that way
        // the next iteration will skip over those whose flags are
        // already set.
      } else
        mod_unmarkTypesVisited(mod);
    }
}

// return 0 on no cycle found, -1 on cycle found
static int type_checkCycles(Parser* parser, Type* type) {
  FUNC_ENTRY
  foreach (Expr*, stmt, type->body->stmts)
    if (expr_markTypesVisited(parser, stmt)) {
      eprintf("  -> created in type \e[;1;2m%s\e[0;2m at ./%s:%d:%d \n",
        type->name, parser->filename, stmt->line, stmt->col);
      return -1;
    }
  return 0;
}

static int expr_markTypesVisited(Parser* parser, Expr* expr) {
  FUNC_ENTRY
  Type* type = NULL;
  if (!expr) return 0;
  switch (expr->kind) {
  case tkVarDefn: return expr_markTypesVisited(parser, expr->var->init);
  case tkFuncCall: return expr_markTypesVisited(parser, expr->left);
  case tkFuncCallR:
    if (expr_markTypesVisited(parser, expr->left)) return -1;
    if (expr->func->spec->typeType == TYObject
      && expr->func->returnsNewObjectAlways)
      type = expr->func->spec->type;
    break;
  case tkSubscript:
  case tkSubscriptR: return expr_markTypesVisited(parser, expr->left);

  case tkIdentR:
  case tkString:
  case tkIdent:
  case tkNo:
  case tkYes:
  case tkNil:
  case tkNumber:
  case tkRawString:
  case tkComment: return 0;
  default:
    if (expr->prec) {
      int ret = 0;
      if (!expr->unary) ret += expr_markTypesVisited(parser, expr->left);
      ret += expr_markTypesVisited(parser, expr->right);
      if (ret) return ret;
    } else
      unreachable("unknown expr kind: %s at %d:%d\n",
        TokenKind_names[expr->kind], expr->line, expr->col);
  }
  if (!type) return 0;
  if (type->visited) {
    err_constructorHasCycle(parser, type);
    eprintf("%s", "\e[;2m"); // Backtrace (innermost first):\n");
    return -1;
  }
  type->visited = true;
  return type_checkCycles(parser, type);
}

static void mod_unmarkTypesVisited(Module* mod) {
  // reset the cycle check flag on all types
  foreach (Type*, type, mod->types)
    type->visited = false;
}

#include "cse.h"
