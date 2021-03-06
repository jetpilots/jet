struct Analyzer {
  Diagnostics& errs;

  Analyzer(Diagnostics& errs)
      : errs(errs) { }

  void analyse(Module& mod) {
    analyse(mod, mod.types);
    analyse(mod, mod.funcs);
    analyse(mod, *mod.scope, nullptr);
    unused(mod);
  }
  void analyse(Module& mod, Func& func) { analyse(mod, *func.body, &func); }

  private:
  template <typename T>
  void analyse(Module& mod, List<T&>& items) {
    for (T& item : items) analyse(mod, item);
  }
  void analyse(Module& mod, Var& var) { }
  void analyse(Module& mod, Type& type) {
    analyse(mod, type.vars), analyse(mod, type.funcs);
  }
  void analyse(Module& mod, Scope& scope, Func* func) {
    analyse(mod, scope.vars);
    for (Expr& stmt : scope.stmts) analyse(mod, stmt, nullptr, scope, func);
  }
  void analyse(Module& mod, Expr& expr, Expr* parent, Scope& scope,
      Func* ownerFunc) {
    switch (expr.kind) {

      // -------------------------------------------------- //
    case tkFunctionCallResolved: {
      if (ownerFunc) { // TODO: ownerFunc should not be NULL, even
                       // top-level and type implicit ctors should have a
                       // func created
        ownerFunc->callees.shift(*expr.func);
        expr.func->callers.shift(*ownerFunc);
      }
      if (expr.left->countCommaList() != expr.func->args.count())
        errs.argsCountMismatch(expr);
      if (expr.func->ret) expr.spec = expr.func->ret->spec;
      // expr.spec.typeType = expr.func->ret->spec
      //     ? expr.func->ret->spec.typeType
      //     : TYNoType; // should actually be TYVoid
      // expr.spec.collectionType = expr.func->ret
      //     ? expr.func->ret->spec.collectionType
      //     : CTYNone; // should actually be TYVoid
      expr.elemental = expr.func->elemental;
      // if (expr.func->ret->spec)
      // expr.spec.dims = expr.func->ret->spec->dims;
      // isElementalFunc means the func is only defined for (all)
      // Number arguments and another definition for vector args
      // doesn't exist. Basically during typecheck this should see
      // if a type mismatch is only in terms of collectionType.
      if (!expr.left) break;
      expr.elemental = expr.elemental and expr.left->elemental;
      expr.throws = expr.left->throws or expr.func->throws;
      Expr* currArg = expr.left;
      for (Var& arg : expr.func->args) {
        Expr* cArg = (currArg->is(tkOpComma)) ? currArg->left : currArg;
        if (cArg->is(tkOpAssign)) cArg = cArg->right;
        if (cArg->spec.typeType != arg.spec.typeType)
          errs.argTypeMismatch(cArg, arg);
        // TODO: check dims mismatch
        // TODO: check units mismatch
        // TODO: set enum base
        if (!(currArg = currArg->right)) break;
      }

      currArg = expr.left;
      while (currArg) {
        Expr* cArg = (currArg->is(tkOpComma)) ? currArg->left : currArg;
        if (cArg->is(tkOpAssign)) {
          // LHS will be a tkIdentifier. You should resolve it to one
          // of the function's arguments and set it to
          // tkArgumentLabel.
          assert(cArg->left->is(tkIdentifier));
          Var* theArg = nullptr;
          for (Var& arg : expr.func->args) {
            if (!strcasecmp(cArg->left->string, arg.name)) theArg = &arg;
          }
          if (not theArg) {
            unreachable("unresolved argument %s!", cArg->left->string);
            // cArg->left->kind = tkIdentifier;
            // change it back to identifier
          } // TODO: change this to parser error
          else {
            cArg->left->var = theArg;
            cArg->left->kind = tkIdentifierResolved;
          }
        }

        currArg = currArg->is(tkOpComma) ? currArg->right : nullptr;
      }

    } break;

      // -------------------------------------------------- //
    case tkFunctionCall: {
      char buf[128] = {};
      char* bufp = buf;
      if (expr.left) analyse(mod, *expr.left, &expr, scope, ownerFunc);

      // TODO: need a function to make and return selector
      Expr* arg1 = expr.left;
      if (arg1 and arg1->is(tkOpComma)) arg1 = arg1->left;
      const char* typeName = arg1->typeName();
      //        const char* collName = "";
      //        if (arg1)
      //        collName=CollectionType_nativeName(arg1->collectionType);
      if (arg1) bufp += sprintf(bufp, "%s_", typeName);
      bufp += sprintf(bufp, "%s", expr.string);
      if (expr.left)
        strarglabels(expr.left, bufp, 128 - ((int)(bufp - buf)));

      Func* found = mod.getFunc(buf);
      if (found) {
        expr.kind = tkFunctionCallResolved;
        expr.func = found;
        analyse(mod, *found);
        analyse(mod, expr, parent, scope, ownerFunc);
        return;
      }
      if (!strncmp(buf, "Void_", 5))
        errs.callingFuncWithVoid(expr, arg1);
      else {
        errs.unknownFunc(expr, buf);

        if (*buf != '<') // not invalid type
        {
          int sugg = 0;
          for (Func& func : mod.funcs) {
            if (!strcasecmp(expr.string, func.name)) {
              eprintf("\e[36;1minfo:\e[0;2m   not viable: %s with %d "
                      "arguments at %s:%d\e[0m\n",
                  func.selector, func.args.count(), mod.filename,
                  func.line);
              sugg++;
            }
            if (!func.intrinsic and strcasecmp(expr.string, func.name)
                and leven(
                        expr.string, func.name, expr.loc.len, func.loc.len)
                    < 3
                and func.args.count() == expr.left->countCommaList()) {
              eprintf("\e[36;1minfo:\e[0m did you mean: "
                      "\e[34m%s\e[0m (%s at "
                      "./%s:%d)\n",
                  func.name, func.selector, mod.filename, func.line);
              sugg++;
            }
          }
          if (sugg) eputs("-----x\n");
        }
      }
    } break;

      // -------------------------------------------------- //
    case tkVarAssign: {
      // TypeInfo& spec = expr.var->spec;
      Var& var = *expr.var;

      if (var.spec.is(TYUnresolved)) resolve(expr.var, mod);

      if (not expr.var->init) {
        // if the typespec is given, generate the init expr yourself
        // this is only for basic types like primitives.
        // and arrays of anything can be left without an init.
        if (!var.spec.dims) // goto errorMissingInit;
          switch (var.spec.typeType) {
          case TYNumber: expr.var->init = &expr_const_0; break;
          case TYString: expr.var->init = &expr_const_empty; break;
          case TYBool: expr.var->init = &expr_const_no; break;
          case TYObject:
            if (not expr.var->isEnum()) {
              expr.var->init = &expr_const_nil;
              break;
            }
          default:
          errorMissingInit:
            // this is an error, no way to find out what you want
            errs.missingInit(expr);
          }
      } else {
        // if (spec.is(TYUnresolved))
        //     resolveTypeSpec(spec, mod);
        // first try to set enum base if applicable.
        Expr& init = *expr.var->init;

        if (expr.var->isEnum()) setEnumBase(init, spec, mod);

        analyse(mod, init, &expr, scope, ownerFunc);

        if (!init.spec.is(TYNilType)) {
          // if not nil, get type info from init expr.
          // expr.spec.typeType = init.typeType;
          // expr.spec.collectionType = init.collectionType;
          // expr.nullable = init.nullable;
          // expr.elemental = init.elemental;
          // expr.throws = init.throws;
          // expr.impure = init.impure;
          // expr.spec.dims = init.dims;
          expr.spec = init.spec;
        } else if (var.spec.is(TYObject)) {
          // if nil, and typespec given and resolved, set type info
          // from typespec.
          expr.spec = *spec; //->spec.typeType;

        } else if (var.spec.is(TYUnresolved)) {
          // this will already have caused an error while resolution.
          // since specified type is unresolved and init is nil, there
          // is nothing to do except to mark it an error type.
          var.spec.typeType = expr.spec.typeType = TYErrorType;

        } else {
          // this would be something like init'ing primitives with
          // nil.
          errs.initMismatch(expr);
          var.spec.typeType = expr.spec.typeType = TYErrorType;
        }

        if (var.spec.is(TYUnresolved)) {
          var.spec.typeType = init.spec.typeType;
          if (init.spec.is(TYObject)) {
            if (init.is(tkFunctionCallResolved)) {
              expr.var->spec = init.func->ret->spec;
              // ^ TODO: DROP old expr.var->spec!!
              var.type = init.func->ret->type;
              var.spec.dims = init.func->ret->spec.dims;
            } else if (init.is(tkIdentifierResolved)) {
              expr.var->spec = init.var->spec;
              var.type = init.var->type;
              var.spec.dims = init.var->spec.dims;
            } else if (init.is(tkArrayOpen))
              var.type = init.type;
            else if (init.is(tkBraceOpen))
              var.type = init.type;
            else if (init.is(tkPeriod)) {
              Expr* e = &init;
              if (init.left->var->isEnum()) {
                var.type = init.left->var->type;
              } else {
                while (e->is(tkPeriod)) e = e->right;
                // at this point, it must be a resolved ident or
                // subscript
                var.type = e->var->type;
                var.spec.dims = e->var->spec.dims;
              }

            } else {
              unreachable("%s", "var type inference failed");
            }
            analyse(mod, *var.type);
          }
        } else if (var.spec.typeType != init.spec.typeType
            // and init.typeType != TYNilType
        ) {
          // init can be nil, which is a TYNilType
          errs.initMismatch(expr);
          expr.spec.typeType = TYErrorType;
        }

        if (var.spec.dims == 0) {
          var.spec.collectionType = init.spec.collectionType;
          var.spec.dims = init.is(CTYTensor) ? init.spec.dims
              : init.is(CTYArray)            ? 1
                                             : 0;
        } else if (var.spec.dims != 1 and init.is(CTYArray)) {
          errs.initDimsMismatch(expr, 1);
          expr.spec.typeType = TYErrorType;
        } else if (var.spec.dims != 2 and init.is(CTYTensor)) {
          errs.initDimsMismatch(expr, 2);
          expr.spec.typeType = TYErrorType;

        } else if (var.spec.dims != 0 and init.is(CTYNone)) {
          errs.initDimsMismatch(expr, 0);
          expr.spec.typeType = TYErrorType;
        }
      }
    } break;

      // -------------------------------------------------- //
    case tkKeyword_match: {
      Expr* cond = expr.left;
      if (cond) {
        analyse(mod, *cond, &expr, scope, ownerFunc);
        if (expr.body and cond->isEnum()) {
          for (Expr& cas : expr.body->stmts)
            if (cas.left) setEnumBase(cas.left, &cond->spec, mod);
        }
      }

      for (Expr& stmt : expr.body->stmts) {
        analyse(mod, stmt, &expr, *expr.body, ownerFunc);
        if (cond and stmt.is(tkKeyword_case) and stmt.left
            and (stmt.left->spec != cond->spec))
          errs.typeMismatch(*cond, *stmt.left);
      }
    } break;
    case tkKeyword_else:
    case tkKeyword_if:
    case tkKeyword_for:
    case tkKeyword_elif:
    case tkKeyword_while:
    case tkKeyword_case:
      if (expr.left) analyse(mod, *expr.left, &expr, scope, ownerFunc);
      analyse(mod, *expr.body, ownerFunc);
      break;

      // -------------------------------------------------- //
    case tkSubscriptResolved: {
      // assert(expr.left->is(tkArrayOpen));
      int nhave = expr.left->countCommaList();
      if (nhave != expr.var->spec.dims) errs.indexDimsMismatch(expr, nhave);
    }
      // fallthru
    case tkSubscript:
      if (expr.left) analyse(mod, *expr.left, &expr, scope, ownerFunc);
      if (expr.is(tkSubscriptResolved)) {
        expr.spec = expr.var->spec; //->spec.typeType;
        // expr.spec.collectionType =
        // expr.var->spec.collectionType;
        // TODO: since it is a subscript, if it has no left (i.e. arr[])
        // i'm guessing it is a full array, and therefore can be an
        // elemental op
        expr.elemental = expr.left ? expr.left->elemental : true;
        // TODO: check args in the same way as for funcs below, not
        // directly checking expr.left.}
        // TODO: dims is actually a bit complicated here. For each dim
        // indexed with a scalar (not a range), you reduce the dim of
        // the subscript result by 1. For now the default is to keep the
        // dims at 0, which is what it would be if you indexed something
        // with a scalar index in each dimension. analyse for a : op
        // should set dims to 1. Then you just walk the comma op in
        // expr.right here and check which index exprs have dims=0.
        // Special case is when the index expr is a logical, meaning you
        // are filtering the array, in this case the result's dims is
        // always 1.
      }
      break;

    case tkString:
    case tkRawString:
      expr.spec.typeType = TYString;
      prepareInterp(expr, scope);
      break;

    case tkNumber: expr.spec.typeType = TYNumber; break;

    case tkKeyword_yes:
    case tkKeyword_no: expr.spec.typeType = TYBool; break;

    case tkKeyword_nil: expr.spec.typeType = TYNilType; break;

    case tkIdentifier: // all vars must have been resolved
      expr.spec.typeType = TYErrorType;
      break;

    case tkIdentifierResolved:
      expr.spec.typeType = expr.var->spec.typeType;
      expr.spec.collectionType = expr.var->spec.collectionType;
      expr.elemental = expr.spec.collectionType != CTYNone;
      expr.spec.dims = expr.var->spec.dims;

      // this was done just to ensure enums are caught and analysed in
      // non-lint mode. In lint mode they are done anyway.
      // TODO: remove this, you shouldnt be doing it on each var use it
      // will be too intensive. let it just be done on each var decl and
      // you figure out a way to set the enum type on var decls.
      if (expr.spec.is(TYObject)) analyse(mod, *expr.var->type);
      break;

    case tkArrayOpen:
      expr.spec.collectionType = CTYArray;
      if (expr.right) {
        analyse(mod, *expr.right, &expr, scope, ownerFunc);
        expr.spec.typeType = expr.right->spec.typeType;
        expr.spec.collectionType
            = expr.right->is(tkOpSemiColon) ? CTYTensor : CTYArray;
        expr.spec.dims = expr.right->is(tkOpSemiColon) ? 2 : 1;
        // using array literals you can only init 1D or 2D
        if (expr.spec.typeType == TYObject) {
          // you need to save the exact type of the elements, it's not
          // a primitive type. You'll find it in the first element.
          Expr* first = expr.right;
          while (first->in(tkOpComma, tkOpSemiColon)) first = first->left;
          switch (first->kind) {
          case tkIdentifierResolved:
            expr.type = first->var->type;
            if (first->var->spec.dims
                or first->var->spec.collectionType != CTYNone)
              unreachable(
                  "trying to make array of arrays %d", expr.loc.line);
            break;
          case tkFunctionCallResolved:
            expr.type = first->func->ret->spec.type;
            if (first->func->ret->spec.dims
                or first->var->spec.collectionType != CTYNone)
              unreachable(
                  "trying to make array of arrays line %d", expr.loc.line);
            break;
          default:
            break;
            // TODO: object init literals
            // case tkObjectInitResolved:
            // expr.type = first->var->spec.type;break;
          }
          // expr.type =
        }
      }
      break;

    case tkBraceOpen:
      if (expr.right) {
        analyse(mod, *expr.right, &expr, scope, ownerFunc);
        // TODO: you told analyse to not care about what's on the
        // LHS of tkOpAssign exprs. Now you handle it yourself. Ensure
        // that they're all of the same type and set that type to the
        // expr somehow.
        analyseDictLiteral(expr.right, mod);
        expr.spec.typeType = expr.right->spec.typeType;
        if (expr.spec.typeType == TYObject) {
          // you need to save the exact type of the elements, it's not
          // a primitive type. You'll find it in the first element.
          Expr* first = expr.right;
          while (first->is(tkOpComma)) first = first->left;
          if (first->is(tkOpAssign)) first = first->right;
          // we care about the value in the key-value pair. We'll
          // figure out the key type later or not, whatever.
          switch (first->kind) {
          case tkIdentifierResolved:
            expr.type = first->var->spec.type();
            break;
          case tkFunctionCallResolved:
            expr.type = first->func->ret->spec.type();
            break;
          default:
            break;
            // TODO: object init literals
            // case tkObjectInitResolved:
            // expr.type = first->var->spec.type;break;
          }
        }
        expr.spec.collectionType = CTYDictS;
        // these are only Dicts! Sets are normal [] when you detect they
        // are only used for querying membership.
        // TODO: what were the gazillion Dict subtypes for?
      }
      break;
    case tkPeriod: {

      if (not expr.left) {
        errs.noEnumInferred(expr.right);
        break;
      }

      assert(expr.left->in(tkIdentifierResolved, tkIdentifier));
      analyse(mod, *expr.left, &expr, scope, ownerFunc);

      // The name/type resolution of expr.left may have failed.
      if (not expr.left->spec.typeType) break;

      Expr* member = expr.right;
      if (member->is(tkPeriod)) {
        member = member->left;
        if (not member->is(tkIdentifier)) {
          errs.unexpectedExpr(*member);
          break;
        }
      }

      if (not member->in(tkIdentifier, tkSubscript, tkFunctionCall)) {
        errs.unexpectedExpr(*member);
        break;
      }
      //  or member->is(tkFunctionCall));
      if (not member->is(tkIdentifier) and member->left)
        analyse(mod, *member->left, member, scope, ownerFunc);

      // the left must be a resolved ident
      if (not expr.left->is(tkIdentifierResolved)) break;

      if (expr.left->var->spec.is(TYErrorType)) {
        expr.spec.typeType = TYErrorType;
        break;
      }
      assert(not expr.left->var->spec.is(TYNilType));

      Type* type = expr.left->getTypeOrEnum();
      if (!type) {
        expr.spec.typeType = TYErrorType;
        break;
      }

      // Resolve the member in the scope of the type definition.
      resolve(member, type);
      // Name resolution may fail...
      if (not member->is(tkIdentifierResolved)) {
        expr.spec.typeType = TYErrorType;
        break;
      }
      analyse(mod, member, memberParent, scope, ownerFunc);

      if (expr.right->is(tkPeriod))
        analyse(mod, *expr.right, &expr, scope, ownerFunc);

      expr.spec.typeType
          = type->isEnum ? TYObject : expr.right->spec.typeType;
      expr.spec.collectionType = expr.right->spec.collectionType;
      expr.elemental = expr.right->elemental;
      expr.spec.dims = expr.right->spec.dims;
    } break;

    case tkLineComment: break;

    case tkArgumentLabel:

      break;

      //    case tkRawString:
      // TODO: analyse regex, compile it already, whatever
      //        break;
      // -------------------------------------------------- //
      // case tkKeyword_in:
      // case tkKeyword_notin:
      // these ops may take an array of enums, so set their base type.
      // if (expr.left->kind==tkIdentifierResolved)

      // there's some work being done on these as standard binops, so
      // fallthrough;
    default:
      if (expr.prec) {
        if (!expr.unary and expr.left)
          analyse(mod, *expr.left, &expr, scope, ownerFunc);
        // some exprs like return can be used without any args

        if (expr.in(tkKeyword_in, tkKeyword_notin, tkOpAssign, tkOpEQ,
                tkOpNE)) {
          Type* type = expr.left->getObjectType();
          if (type and type->isEnum()) {
            setEnumBase(expr.right, type, mod);
          }
          // else {
          //     spec = getObjectTypeSpec(expr.left);
          //     if (spec and spec->spec.typeType == TYObject
          //         and spec->type->isEnum) {
          //         setEnumBase(expr.right, spec, mod);
          //     }
          // }
        }

        if (expr.right) analyse(mod, *expr.right, &expr, scope, ownerFunc);

        if (expr.is(tkKeyword_or) and expr.left->spec.typeType != TYBool) {
          // Handle the 'or' keyword used to provide alternatives for
          // a nullable expression.
          ;
        } else if (expr.isCmpOp() or expr.isBoolOp()) {
          // Handle comparison and logical operators (always return a
          // bool)
          expr.spec.typeType
              = (expr.right->spec.typeType == TYErrorType
                    or (!expr.unary
                        and expr.left->spec.typeType == TYErrorType))
              ? TYErrorType
              : TYBool;
        } else {
          // Set the type from the ->right expr for now. if an error
          // type is on the right, this is accounted for.
          if (expr.right) {
            expr.spec.typeType = expr.right->spec.typeType;
            expr.spec.collectionType = expr.right->spec.collectionType;
          }
        }
        if (expr.right)
          expr.elemental = expr.right->elemental or expr.is(tkOpColon);
        // TODO: actually, indexing by an array of integers is also an
        // indication of an elemental op

        if (!expr.unary and expr.left) {
          expr.elemental = expr.elemental or expr.left->elemental;
          expr.spec.dims = expr.right->spec.dims;
          // if (expr.is(tkOpColon) and expr.right->kind ==
          // tkOpColon)
          //     expr.right->dims = 0; // temporarily set it to 0 to
          //     allow
          // parsing stepped ranges 1:s:n

          if (expr.spec.dims != expr.left->spec.dims
              and !(inFuncArgs and (expr.in(tkOpComma, tkOpAssign)))) {
            // if either one has 0 dims (scalar) it is an elemental
            // op with a scalar.
            if (expr.left->spec.dims != 0 and expr.right->spec.dims != 0) {
              errs.binOpDimsMismatch(expr);
              expr.right->spec.typeType = TYErrorType;
            } else if (expr.in(tkOpPlus, tkOpMinus, tkOpTimes, tkOpSlash,
                           tkOpPower, tkOpMod)) {
              expr.spec.dims = expr.left->spec.dims + expr.right->spec.dims;
              expr.spec.collectionType = max(expr.left->spec.collectionType,
                  expr.right->spec.collectionType);
              // todo: stop distinguishing array and tensor!!!
              // then you dont need this. this strongly depends on
              // the op & is too much repeated work eprintf("ok
              // `[+-*/^%]` dims@ %d %d %d %d\n", expr.line,
              //     expr.col, expr.left->dims,
              //     expr.right->dims);
            } else if ((expr.in(tkKeyword_in, tkKeyword_notin))
                and expr.left->spec.dims == 0
                and expr.right->spec.dims == 1) {
              // eprintf("ok `in` dims@ %d %d %d %d\n",
              // expr.line,
              //     expr.col, expr.left->dims,
              //     expr.right->dims);
            } else if (expr.is(tkOpColon) //
                and expr.left->spec.dims == 1 and expr.left->is(tkOpColon)
                and expr.right->spec.dims == 0) {
              // eprintf("ok `:` dims@ %d %d %d %d\n", expr.line,
              //     expr.col, expr.left->dims,
              //     expr.right->dims);
              // expr.spec.dims = 1;
            } else {
              errs.binOpDimsMismatch(expr);
            }

            // ^ TODO: but you can also have some ops on 2D and 1D
            // operands e.g. linear solve. what about those?
          } else {
            // eprintf("(ignore) dims@ %d %d %d %d\n", expr.line,
            //     expr.col, expr.left->dims, expr.right->dims);
          }
          // ranges always create a 1D entity (not always array, but
          // 1D)
          if (expr.is(tkOpColon)) expr.spec.dims = 1;
        }
        if (!expr.unary and expr.left
            and !(inFuncArgs and (expr.in(tkOpComma, tkOpAssign)))) {
          // ignore , and = inside function call arguments. thing is
          // array or dict literals passed as args will have , and =
          // which should be checked. so when you descend into their
          // args, unset inFuncArgs.
          TypeType leftType = expr.left->spec.typeType;
          TypeType rightType = expr.right->spec.typeType;

          if (leftType == TYBool and (expr.in(tkOpLE, tkOpLT))) {
            // Special case: chained LE/LT operators: e.g. 0 <= yCH4
            // <= 1.
            ;
          } else if (leftType != rightType) {
            // Type mismatch for left and right operands is always
            // an error.
            errs.typeMismatchBinOp(expr);
            expr.spec.typeType = TYErrorType;
          } else if (leftType == TYString
              and (expr.in(tkOpAssign, tkOpEQ, tkOpNE))) {
            // Allow assignment, equality test and != for strings.
            // TODO: might even allow comparison operators, and
            // perhaps allow +=, or better .= or something. Or
            // perhaps append(x!) is clearer
            ;
          } else if (leftType == TYBool
              and (expr.in(tkOpAssign, tkOpEQ) //
                  or expr.in(tkKeyword_and, tkKeyword_or, tkKeyword_not)
                  or expr.is(tkKeyword_notin)))
            ;
          else if (expr.isArithOp()
              and (not isnum(leftType) or not isnum(rightType))) {
            // Arithmetic operators are only relevant for numeric
            // types.
            // if(  leftType==TYObjectandexpr.left->)
            // TODO: allow enum +
            errs.invalidTypeForOp(expr);
          }
          // check if an error type is on the left, if yes, set the
          // expr type
          if (leftType == TYErrorType) expr.spec.typeType = leftType;
        }
        // TODO: here statements like return etc. that are not binary
        // but need to have their types checked w.r.t. an expected type
        // TODO: some ops have a predefined type e.g. : is of type Range
        // etc,
      } else {
        unreachable("unknown expr kind: %s", TokenKinds_names[expr.kind]);
      }
    }
  }

  void unused(Scope& scope) {
    for (Var& var : scope.vars) var.used ? (void)0 : errs.unusedVar(var);
    for (Expr& stmt : scope.stmts)
      stmt.isCtrlExpr() and stmt.body ? unused(*stmt.body) : (void)0;
  }
  void unused(Func& func) {
    for (Var& arg : func.args)
      arg.used ? (void)0 : errs.unusedArg(arg, func);
    unused(*func.body);
  }
  void unused(Type& type) {
    if (type.body)
      for (Var& memb : type.body->vars)
        memb.used ? (void)0 : errs.unusedMemb(memb, type);
  }
  void unused(Module& mod) {
    unused(*mod.scope);
    for (Type& type : mod.types)
      type.used ? unused(type) : errs.unusedType(type);
    for (Func& func : mod.funcs)
      func.used ? unused(func) : errs.unusedFunc(func);
  }
};