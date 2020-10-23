The purpose of reduction is

- improve cache usage by reducing `x[] = a[] + b[] + c[] + d[]` to `x[] = a[]; x += b[]; x += c[]; x+= d[]`
- minimize the number of temporaries by reducing `x = f(g) + f(h) + f(k)` to `x = f(g); x += f(h); x += f(k)` and inserting drop calls where appropriate

- they are both the same thing. just that in the first they are lvalues that can be dropped after last use, and rvalues (not referenced elsewhere) by definition are their own last use (and can be dropped immediately).



A motivating example is OpenFOAM's addition of `fvScalarMatrix` types to obtain the ubiquitous "xxEqn" objects (I don't know or care what their implementation does to minimize temporaries).



`x = a + f(b) + g(h)`

is really just 

`a = add(a, add(f(b), g(h)))`

just like `x = f(y, g(h, m(z)))` in general. (Note: this applies only to non-elemental funcs, see below.)



It's better to have `elemental` in the language so that the compiler can know it is better to not extract. (**BUT** can you not detect and avoid the need?)



### Drop

Drop for stack-allocated vars is a no-op. For heap or pool variables it is a dealloc (means it goes on the freelist). ALL variables without exception are dropped immediately after their last usage. Drop calls are inserted as a final pass, after ALL kinds of lowering! This ensures temporaries can be dropped as soon as possible.

I guess drop should really mean decrement ref count and really drop only if it hits 0.

### Extraction

Taking subexpressions and extracting them into a temporary variable that lives in the current scope. (As applies to all variables, these are dropped immediately after their last usage, not at the end of the scope). Temporaries are created either as (a) simple references to whatever the subexpression evaluates to, or (b) a clone of it. 

(b) happens when the target of the temporary is not ready to be dropped. This means its last usage is not the statement under consideration (can only happen for lvalues or for *referenced* rvalues - the latter is tricky).

```python
var a = x + y + f(3)
# x can be dropped but not y
# f(3) temporary can be dropped -- unless f(3) gave you a ref to some lvalue
print(a)
print(y)
# now y can be dropped
```



#### What to extract

Looks like almost everything. Basically, extract all function calls to a temp. But you know that even `+`, `-`, etc. even `=`, `+=` are function calls. And all func calls have the result as the last argument.

```python
x = y + z
# add(y, z, x)
# drop [y, z] if you can
```



```python
x = f(g) + m(d)
# _1 = f(g) -> cgen makes it: _1 = f(g, _1)
# _2 = m(d) -> _2 = m(d, _1)
# _3 = add(_1, _2) -> _3 = add(_1, _2, _3)
# x = _3 # when a var is moved like this, set its lastUsage to 999999 so it wont ever be dropped. Now the decision to drop should be made on x, not on _3 anymore.
# drop [_1, _2] if you can
```

Still, here you have 2 temporaries _1 and _2 and a third x for the result. How about bringing it down to 1 temporary?

**inplacing** You know that `CIP(add)` is true. For such functions, look at the first arg they receive. Since you extracted everything, it can only be a variable (temporary or not). Can it be in-placed? if yes, change the temporary to the first arg. So in `_3 = add(_1, _2)` you replace `_3` with `_1` to make it `_1 = add(_1, _2)` which after cgen becomes `_1 = add(_1, _2, _1)` .

 **note** cgen always emits all func calls like this: `ret = fun(arg1, arg2, ..., argn, ret)` . At jet level they are `ret = fun(arg1, ..., argn)`. Because it emits all func decls with an additional argument at the end to hold the result.

if you're going to extract nearly everything, then you have vars being assigned only a few kinds of things:

```python
x = y # move ref
x = f(...) # func call
x = sth binop sth # sth -> any of the other forms, binop = +-/*^% ...
x = unop sth # unary op
x = "asd" # literals
x = 23 # 
x = no #
x = ? # nearly everything is a func call after lowering, so what's left?
```



The problem is you extract all elementwise functions which could have been done with zero temporaries.

```python
x[] = a[] + b[] + c[]
```

needs no temporaries. But you will end up creating 1 or 2 by extracting. Maybe cache utilisation will be better then, but who knows. For large arrays, that is a lot of space for temporaries. So **don't extract elemental funcs or ops**. What functions are elemental? Basically all functions, except those defined on arrays. So every function defined on a scalar or type is elemental and can be applied to an array of that scalar or type. Funcs defined on arrays cannot be elemental, because there are no arrays of arrays... (why can't they be 2D?)

You will be extracting non-elemental funcs anyway (+dropping), so the OpenFOAM example will still be valid, all those subexprs will be extracted. 



### Hoisting

Taking subexpressions that are loop-invariant and hoisting them out of the loop.



### Scalarisation

Replacing an elementwise array operation with the actual loop required to perform it. A still open point is whether to hoist RHS expressions to ensure they are not repeatedly evaluated. e.g. `x[1:end] = random()`. Is this one random number applied on all elements of x, or various random numbers? 

If you go with hoisting such functions, (a) it is work and (b) explicit for loops will have different semantics than implicits:

`x[1:end] = random()` is not the same as `for i in 1:len(x) do x[i] = random()`. 

If you don't hoist them, the user might inadvertently leave a func with side-effects in the RHS and be surprised.

`x[1:end] = now()` One might expect that to put the same time instant everywhere in the array.

(But one *shouldn't*, right? *If you want it cached, you should cache it*, not *If you want it re-evaluated, you should re-evaluate it*)

### Inplaceability

#### ... of a function `f`

 `CIP(f)` where `f` is a function indicates whether `f` *can* work with its first argument aliased to its output argument. This is not the case with stencil ops for instance, or anything with intra-array deps or intra-object deps, unless in some cases  the order of traversing the array makes it possible: e.g. see Jet's in-place `hex` and `unhex`. This is an important consideration to know whether or not `f` *can in-place* (hence `CIP`).

To call a function that can work in-place, the result argument *may* be set to the same as its first argument, allowing the function to reuse the first argument's storage. Whether or not the result argument *should* be set like this depends on the kind of the first argument itself.

`CIP(f)` being true means that `f`  returns something of the exact same type, units, dimensions etc. as its first argument. Therefore the result argument being passed in must either be the first argument repeated (if you really want to do it in-place) or a clone of the first argument being passed. The cloning is done by the caller. Called functions never have to bother about the result argument being initialized, allocated or whatever. They just write to it.

```python
func add2d2d(arr as Real[:,:], arr2 as Real[:,:]) result (out as Real[:,:])
  check size(arr) == size(arr2)
  for i in 1:size(arr,1), j in 1:size(arr,2)
    out[i,j] = arr[i,j] + arr2[i,j]
  end 
end 
```

If `arr` is aliased to `out`, this function can work fine. In any case, `out` must be initialized with the right size. **NOOOO** why would anyone think that `out` is initialized to the size of `arr` by default? What they would do is allocate `out`. And in this case it might just be easier to not bother with inplacing and just let extract+drop do as much as it can to eliminate temps. **NOT IF** the idiomatic way is to `resize()` arrays and there is no `zeros`, `ones` etc. Then the user does as the first thing

```python
out!.resize(arr2.size)
```

Let's just say the system functions (`+`,`-`,`*`, ...) are written this way and for user functions, well, think of something later.



#### ... of a variable `v`

Can a variable be overwritten in a given function call to `f` (i.e. can it **be** in-placed)? Yes if the variable is ready to be dropped after the current statement. This consideration is moot if `CIP(f)` is `false`. When is a variable ready to be dropped? If the last usage of the variable is in the statement under consideration. Note that these decisions are made on the lowered form, i.e. with extractions and hoisting already done.



### Sequence



1. Extraction (in same scope)
2. Hoisting (to parent scope)
3. Inplacing
4. Scalarisation
5. Insert drops

