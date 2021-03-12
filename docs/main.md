# Jet: Quick Start



Jet programs are made up of source files with the `.jet` extension. Each file is a module, consisting of function and type definitions and variables. Every Jet program starts at a function called `start`. This function takes no arguments, and must be present in any source file you try to run.

```lua
function start()
	... -- comments start with two hyphens
end
```

All executable code must go within functions. In what follows, code samples should be assumed to be within the body of a function, even if not explicitly shown, for brevity.

Variables are straightforward:

```lua
var myVar = 2.023
var another = "I once was lost"
```

The `var` keyword is used to distinguish a new variable (from an assignment to an existing variable). Since Jet is a statically typed language, the type of a variable is decided when it is created, and cannot be changed later. Jet knows quite a few basic types.

| Type      | Examples                                  | Notes                                                 |
| --------- | ----------------------------------------- | ----------------------------------------------------- |
| `String`  | `"The quick brown fox"`, `'today'`        | Can span multiple lines.                              |
| `Number`  | `42`, `3.14`, `6.63e12`                   |                                                       |
| `Boolean` | `yes`, `no`, `on`, `off`, `true`, `false` | The linter standardizes all literals to `yes` or `no` |
| `Complex` | `1+0i`, `4.212+2i`                        |                                                       |
| `Size`    | `800x600`, `1920x1280`                    |                                                       |
| `Colour`  | `#FEDCBA`, `##aliceBlue`                  | Named X11 colors can be used directly                 |
| `Regex`   | `'[a-zA-Z][a-zA-Z0-9]+'`                  | Can span multiple lines, freeform regex by default    |



[^0]: Unlike many other statically typed languges, Jet has only one `Number` type (not including the `Complex` number type). The optimizing compiler may decide to generate it to any of the standard C99 numeric data types, depending on its analysis of the context and the operations being performed on a given `Number` variable. (For this reason, it is not called "Real" or "Float": it may well generate an integer data type if appropriate.)



You can defined your own types that are aggregates (or structures) of many variables:

```lua
type MyType
    var firstName = "John"
    var lastName = "Doe"
    var age = 44
end

function start()
    var mt = MyType() -- constructs a new object
    write(mt.firstName) -- accessing a member 
    ...
end
```

Variables within type definitions must be initialized. Every type has, by default, a function with the same name that returns a new instance of that type. This is the default constructor. While you can have other constructors as we will see later, the default constructor can never be overridden. It always does exactly the same thing: initializes member variables to the values you specify in the type definition.



[^0]: It's important to remember that in Jet, type names must start with a capital letter, and variable and function names with a small letter. Constructors are the exception: they are functions but retain the capitalization. In any case, you don't have to bother with the case manually, since the linter will fix it for you.



It's a good thing you can do whatever the hell you like to initialize the members of a type, they do not have to be literals or constants. You can even depend on them to run in the order you specify.

```lua
type Person
    var firstName = ask("First name?") -- ask() prompts for user input
    var lastName = ask("Last name?")
    var fullName = "$firstName $lastName" -- $ interpolates a variable into a string
    var age = num(ask("Age?")) -- num() converts a numeric String to a Number
end
```

[^1]: Here `firstName` and `lastName` will be initialized before `fullName`, because the latter is dependent on them. However, `id` can be initialized independent of everything else, and so `someComplicatedFunc()` may be evaluated out of order, possibly even on a separate thread. Not your problem.



Functions can take any number of arguments, and return one variable. The return variable must be named, and can be assigned to as a normal variable. Within function and type definitions, you can not annotate variable declarations with types, because it is usually clear from the initialing expression. However, function arguments are also variables, and they are always annotated with the type. The `as` keyword does this:

```fortran
function add(a1 as Number, a2 as Number) result (s as Number)
    s = a1 + a2
end
```



Variables in Jet are generally read-write, except in two cases: 

1. all module level (or top-level, or global) variables are read-only

2. all function arguments are read-only (with one exception, sometimes)

```lua
var pi = 22/7 -- global, or top-level variable
function add(a1 as Number, a2 as Number) result (s as Number)
    s = a1 + a2
    a1 = s -- compile error, a1 is read-only
    pi = 3 -- compile error, pi is read-only
end
```

Generally, functions cannot modify any of their arguments. Except when they are named with a `!` at the end: this indicates that the function can then modify the first argument (only).

```lua
function addIn!(a1 as Number, a2 as Number)
    a1 += a2
end
```

To call a function in Jet, you must annotate the callsite with labels for all arguments except the first. The label is the name of the argument as defined. For example:

```lua
var s = add(2, a2=4) -- not `add(2, 4)` 
```

Functions can be overloaded, with a simple rule. The same function name can be used as long as the number of arguments or the labels of arguments are different.

```lua
function overl(a as Number, b as String) -- function 1
function overl(a as String, b as Number) -- function 2
-- ^ error, labels are not distinct from function 1
function overl(a as String, c as Number) -- function 3, ok
```

As a consequence, you cannot have `add` and `add!` as two separate functions, if their arguments and argument labels are the same. 

[^0]: The `!` sign is ignored while resolving the overload. In other words it cannot always be added automatically by the linter if you forget it in a function call. This is because it's impossible to tell whether you meant to indeed call `add` instead of `add!`.

Lastly, functions can have a simpler statement syntax if there is a single expression to be returned:

```python
add(a1 as Number, a2 as Number) := a1 + a2
```



Variables can be held in collections. In Jet, there are only two kinds of collections visible to the user[^n]: arrays and dictionaries. However, there are various ways to refer to arrays and their elements, or a subset of their elements. Arrays can hold variables of one given type (which may be any basic or user-defined type). Dictionaries are defined for a given type-pair, for the key and value type. Again, these may be any basic or user-defined type for the value as well as the key [^a].

[^n]: Only two kinds of collections are **visible** to the user. In reality the compiler may generate a collection to: a contiguous-storage array, a singly linked or doubly linked list, a hash set/map or binary tree, a temporary array on the function's stack frame, or one of several specialized data structures in the standard library.
[^a]: This means that `hash()` and `equal()` are generated automatically for user-defined types (as far as possible).



```lua
function update!(ar[] as Number)
    ar[1:end] = 53 -- broadcast to whole array
end

function arrayDemo()
    var arr = [1, 2, 3, 4] -- array of 4 numbers
    -- indexing in Jet is 1-based
    var sli = arr[1:2] -- creates a slice based on range of indices
    var filt = arr[arr<=2] -- creates a filter based on a predicate
    var sel = arr[[1,3]] -- creates a selection based on a list of indices

    -- named slices, filters and selections are immutable
    update!(sli) -- compile-time error
    -- unnamed (implicit) slices, filters and selections are mutable
    -- but only when used as the first argument to a !-function
    update!(arr[1:2]) -- ok

    -- multi-dimensional. same general rules apply
    var tens3d = zeros([64,64,64]) -- 3D array

    -- Arrays own their storage. Slices, filters and selections
    -- are only references to a base array.

end

```






Motivation

Mechanism

Design Considerations

Implementation