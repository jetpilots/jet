Jet is a small and fast language for next-gen computing.



## Parallelism built-in

Jet has built-in parallel constructs directly on top of pthreads and MPI: no libraries, runtimes, frameworks, no bloat. You can do handwritten MPI interwoven with threads, or you can use ready-made convenience functions for common parallelism patterns.

If you only use pthreads, Jet won't even link to an MPI library. If you don't use threads at all, Jet won't even link pthreads. See [compilation]().

### Message Passing

Message passing using, well, the [Message Passing Interface](), is built-in to Jet and enables passing not only primitive numeric values and arrays, but any meaningful Jet type, across processes scattered on multiple compute nodes.

#### Single operations

`send` and `receive` can be called on any Jet type, whether a scalar, array, or tensor of any dimensions and rank. It can also be called on most dynamic types, such as `JSON`, `XML`, `YAML`, `Table` and others. `send` takes an optional parameter `async` which can be set to `yes` to do a non-blocking send. `receive` needs no such parameter: you just have to declare the variable you are receiving into as `async`.

```csharp
[async] varr = receive(fromID, size=[300, 300], tag=...)
[async] send(data, toID=..., tag=...)
```

> **Implementation note**
>
> Normally, you would think the initializer of an `async` variable is dispatched on a pthread, and that's how it generally works. But the compiler understands what you are trying to do here, and instead transforms the `MPI_Recv` call into `MPI_IRecv`.



> **Internal note**
>
> It may be useful to have async as an operator that goes on a top-level statement and dispatches that. Sometimes you don't need the result at all, so there is no async *var* to speak of.



#### Collective operations

```csharp
[async] gather(&loc, part=glob, toRank=...)
[async] scatter(&loc, whole=glob, fromRank=...)
[async] gather(&loc, part=glob) // allgather

[async] alltoall(&loc, data=glob) // alltoall
    
[async] broadcast(&c)
    
barrier()

```



> **Internal note**
>
> Definitely need to have `async` as a top-level operator, and maybe also have more async statements like `async for` but not `async if`. NO `async for` is problematic since it can mutate lots of local vars. async stuff should be able to be tracked with 1 target var only thats why you have async vars.
>
> Can't return the value, it has to be a mutable arg, since you need to know the lengths when they are variable (`gatherv`/`scatterv`)
>
> Resist renaming alltoall() to scatter without a fromRank, since it is no longer conceptually a "part -> whole" or "whole -> part" composition or decomposition.



#### Reduction operations

> **Internal note**
>
> `sum`, `product`, `mean`, `count`, etc. are all global by default unless you pass `local=yes`. 
>
> Or have separate functions `mpsum`, `mpmean`, etc. 



### Asynchronous evaluation

Concurrency and parallelism are combined in Jet. Asynchronous tasks by definition run on a separate pthread, so you can use them for parallelism.

Launching an async task is a fine example of the simplicity of Jet :

```csharp
async var x = sum(myarr[:])
... some unrelated work ...
print(x)
```

Unlike in most other languages, *functions* are not declared `async`, *variables* are. 

It simply means whatever is in the initialization of the variable is dispatched to a new *pthread*. The initializer need not be a function, it can be any expression: you can even do `async x = 1 + 1` or `async x = 0` just for the fun of it[^0].

Conversely, functions are not restricted to being either sync or async. You can call a given function synchronously in some places and asynchronously elsewhere. The same function. Without writing two separate versions of it. It's *your call*.

There is no explicit `await` either; the first usage of the async variable implicitly awaits it. Pipelining is as simple as filling the space between the definition of an async variable and its usage with an unrelated task: something that doesn't use `x` or mutate any of its dependencies (here `myarr[:]`), since `x` is possibly still being computed in another thread. You can always make a copy if you can afford to pay in bytes to reduce such dependencies.

Actually, you don't have to write your code explicitly like that, nor should you. Jet's compiler is smart enough to move statements around, based on the dependency graph. The following is actually equivalent to the previous example:

```csharp
async var x = sum(myarr[:])
print(x)
... some unrelated work ...	
```

This means you can group your code chunks logically, rather than having to manually interleave them to make the pipelining explicit. During compilation, statements are moved up as far as their dependencies permit. The current example is therefore implicitly pipelined: it will be transformed by the compiler into the previous example.

When using `async` variables, it's strongly recommended to just keep your code in clean, logical chunks within functions, and let the compiler reorder and interleave statements to create the best pipeline possible. This way, when dependencies change due to a change in access patterns, you won't have to reconsider whether your pipeline is still efficient (or worse, still valid). Rich hints in the compiler's optimisation reports will always tell you when (often small) changes can enable additional optimisations.



> **Implementation notes** 
>
> - The async variable is indeed a *promise* until it is fully computed, but the promise *object* itself is never accessible. Instead, accessing it directly gives you the value behind the promise (after waiting). In most other languages, the promise object is part of the language; in Jet it is abstracted away.
> - Since it cannot be accessed, the promise *object* cannot be treated like a value, passed around, waited on separately, polled for intermediate progress[^1], etc. 
> - All async variables are either awaited in their defining function (in the same scope or an inner scope) or discarded.  
> - If you don't use the result of an async variable, the compiler will attempt to not generate the definition in the first place, unless it contains side effects (I/O), in which case the dispatched thread will simply do its work and terminate with no joining overhead[^2]. 



[^0]:  Do that to see how low the *pthreads* overhead is, since all threads are detached and are given a very small stack. You can easily do 100K+ threads at once on a regular desktop-class machine. Jet uses a thread pool to manage this based on your physical core count.
[^1]: You can always call a progress-handler function (or callback) from within your dispatched expression. Just don't update GUI elements from threads.
[^2]: Async tasks are are always dispatched on a *detached*  pthread.



## Lean mean API machine

Any Jet function can be turned into a REST, SOAP, gRPC, or JetRPC API call by preceding it with the `api` keyword. You then simply run `jet serve` on your source file and a local HTTP server instantly pops up and listens for connections. Serialization and validation happen completely transparently. See [API features](api-features.md).