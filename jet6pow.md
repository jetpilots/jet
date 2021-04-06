# Jet's 6 power features that will change your approach to programming



In this six-part series, I will delve into the peculiar features of Jet that challenge the way we think while designing and writing programs.



### There is only one kind of collection.

**It uses the best kind of collection for the job at hand.**

array, list, set, dlist, ...

maps are *kind of* user-exposed due to {} syntax



### Don't worry about *where* something is stored.

**It's stored in the best place for the variable in question.**

is it `T*`? or  `T[1]`? or `T`? when and why



### Don't worry about temporary allocations.

**Nearly all of the ones you think would happen won't happen.**

inplacing

scalarisation

reduction

elemental funcs

~~Allocations can *only* happen when a new `var` is defined. In other words, if an expression requires a temporary array, for instance, it will fail to compile unless you extract the relevant subexpression to a variable. The linter will do this automatically. (Of course it will not attempt to solve the greatest problem of computing: naming the variable.)~~

~~You know there's nothing *stopping* the compiler from creating the temporary and just generating the code for it. It doesn't have to annoy you with the error -- however this is done for readability. No matter how much *you* may like writing a deep expression in one single line, the reader (*you later*) doesn't always find this helpful. And if the reader is looking for stray allocations, it's easy peasy reading knowing that there cannot be any, where there is no variable declared.~~

~~Functions cannot (effectively) allocate memory and return it. When you do that, say in C or C++, it means you can only return something on the heap (unless you return an external or static buffer and have your colleagues frown at you). You can't, after all, return a variable that is placed on the *stack frame of the caller*. This is where you need to change your paradigm a bit: in Jet, the caller is responsible for all allocations of space tha~~ var x = f1(); x = f2() << how will you handle this if f2 generates new stuff?



~~You know already that compound type and collection variables are passed around by (immutable) reference. There are no copies unless you ask for a copy.~~ << make copy-decision automatic. (auto-insert `copy()`). what about readability? well its CoW everywhere. and most refs (all named refs) are immutable anyway.



`a = avg(aA[]*bB[])`

`m = avg(f(aA[])*g(bB[]))`

`a = avg(aA[1:6]*bB[8:14])`

```c
struct Array { // unified Array and Slice (do same for String / StringRef)
 union { // 8B
   struct {int refc, weak;};
   struct Array* ref;
 };
  
}
```



Lazy evaluation: for an expr like `a = sum(aA+aB)` where `aA` and `aB` are arrays, dont generate temp array from the mult. Instead send in a special structure: `ArrayMulOp`: 

```c
struct ArrayMulOp { // struct for passing lazy eval info. make similar ArrayAddOp, ArrayFuncOp, etc.
    Array* left;
    Array* right;
};
struct ArrayBinOp { // for dynamic general tree
  union {
    Array* a;
    ArrayBinOp* o;
  } left;
	union {
    Array* a;
    ArrayBinOp* o;
  } right;
  BinOpKind kind; // +, -, *, /, ...
};
struct ArrayFuncOp { // for a func call
  union {
    Array* a;
    ArrayBinOp* o;
  } arg;
	void (*func)(void*) ; // cast it to whatever, or wrap it
};
```

Passing `a[]` to a func implies that a may be in fact an `ArrayBinOp` instead of an actual array.

In fact you dont need arraybinop and mulop ArrayFunc is all you need. Then compiler generates the temp func for that expression and the func takes only index i and gives the ith element of the expr result. args tothe func are generated as globals vars that hold the addr of the passed arg (or arg directly?) I guess this is the classic iterator/generator and lambda capture. compiler transparently passes ArrayFunc instead of Array, and receiving func just iterates or gets item i or whatever. I guess implementation can benefit from cpp since virtual methods can be called for begin/end/++/[] etc. when implementing jet's for... or [].

the lambda itself is an object refcounted, if it goes out of scope it takes the globals hlding its args with it. maybe those globals need to be thread local.yes they do because their referenced values can be diff in diff threads.

for now you can just manually do the vtable for all collection kinds in C. if you need this functionality generally then consider virtual funcs in Jet later. actually this is becoming openfoam perf-wise. numeric code is mostly arrays, why should i do runtime dispatch. maybe just generate array-taking funcs as two copies one for array one for generator. and many combinations of args. so when i define sum(x) generate Array_sum and Generator_sum.



### Talk to the compiler about your expectations.

**Short and sweet hints can help the compiler a lot.**

check, assume etc. , constraints 



### There is only one Number type.

**It uses the best kind of number depending on the context.**

Well, it's not JavaScript where there *really* is only one Number type. In Jet, there is only one Number type *exposed* to the programmer. The compiler can figure out what the best underlying type for a given variable is, given the non-local context and operations performed on that variable, any constraints placed on its values, and so on.

Recall that Jet code is always "whole-program" code, as in there are no external libraries, and even if code is in separate compilation units, analysis information is available across those units. In other words, the compiler knows what happens inside *any* function, in terms of whether the function has side-effects, throws errors, does certain operations or calls certain functions on input arguments, allocates any memory, returns any new objects, and so on. You don't have to annotate a function with ever more keywords to tell the compiler that it throws or modifies an argument or something. It's a *compiler*, for heaven's sake *it* should be telling *you* what's going on.

It's no secret the backend generates C99 code and runs your system C compiler on it. So what is `Number`'s underlying C type?

You generally shouldn't ask that question, because the answer is any of `long long`,  `int`,  `char`,  `short` (signed or `unsigned`), `float`, `double`, `Complex`, or more, depending on the context. In fact it can even be `Rational`, `Dual`, `Reciprocal` or any of these special Jet types (which can themselves use any of C's primitive numeric types). I know that's not helpful -- that's why you shouldn't ask that question. If you want to know exactly under what conditions a certain C type is generated, even though it goes against Jet's overarching principle of "don't bother", well, continue reading and bother yourself.



Let's start with `double`. `Number` is `double` unless there is a reason to believe it shouldn't be. Literals are `double`s, even `42`. That's the easy part.



What about `float`? I don't need or want `double`s for my machine learning code, or possibly some coarse geometry code for instance. How can I auto-deduce the use of `float`? And yes it *is* important because I want to run it on my GPU which only works (fast) with half-ass precision.



`int`s? They are kind of easy. A range like `[1:1:x]` is asking for it, so is a for-loop counter, or anything used as an array index or a size/length. 

> it's a *compiler*, right? It should know if a var ends up being used as an array subscript somewhere later, don't you think? YES, across function boundaries and module boundaries, thank you...

The size of the integer is still tricky to get right. Constraints can help here, e.g. if a var is declared to be `in [1:200]`, or local hints or checks can shed light on a var's intended range. It's not the end of the world to guess the size wrong, although it does matter for arrays and struct packing and SIMD performance.



~~`Complex` is staring at you in the face: it is initialized as e.g. `1+1i`.~~ << Complex should be own type

Something initialized with `a/b` becomes a `Rational` (or `Reciprocal` if `a == 1`), when 

- further ops would still be efficient on it 

- the result of `a/b` in a `double` would need precision to more than 15 places 

  

`Dual`s are used when (forward) auto-differentiation is deemed necessary. Otherwise a graph for reverse AD is maintained.



Related: `Boolean`s generate to bitfields or a bitvector, if there are many. The memory fetch is expensive enough to justify that compared to a bunch of bitwise ops. Also, `enum` vars are `int`s, possibly just a few bits to pack the containing struct tight. 

 

enums tl;dr:

- enums have associated data, you can get it without `match/case`, `if/else`, or any branching.

- enum values can easily be converted to and from strings or numbers.

- enum names can be inferred, just start them with a `.`

  