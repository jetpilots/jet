## Enums

### Basics

Here are two enums:

```
enum Flags = #read + #write + #append + #binary
enum TokenKind = #func, #var, #type
```

The first kind is for when you want to be able to combine the members (commonly used to set multiple *flags* or options), and the second is for when you want to choose exactly one.

>  Unlike in other languages, you can't set explicit numeric values on enum members.

The name of the enum is capitalized (like types), because that can be used as the type specification of other variables or functions:

```
var f Flags = #read + #binary
var v TokenKind = #func
```

Enum member names start with a `#`. They **can** use keywords, unlike anything else in Jet.



### Number of members

`len` works on enums just like it works on regular arrays.

```
var l = len(TokenKinds)
```



### String conversion

You can get an enum member's name as a string (at runtime) in the same way you can get a string from anything in Jet:

```
var v Flags = #write
print("mode: $v")
```

You can set an enum value from a string, but you must always provide an alternative (in case no actual member matches the input string). It doesn't matter whether the user enters a choice with the `#` or without.

```
var s = ask()
var v = Flags(s) or #read
```

So, while enums don't automatically get a *default* constructor like types, they do get one that takes a `String`. 

By the way, a better way to ask the user might be:

```
var ens = join(Flags, sep='/')
var s = ask("Pick: [$ens] > ")
...
```

`join` works on an enum just like it would work on a regular `String[]`.



### Associated data

For single-choice enums, the basic syntax is:

```
enum TokenKind = #plus, #minus, #multiply
```

If you want to associate some data with each member, you can use a slightly different syntax instead:

```
enum TokenKind = {
	#plus = "+",
	#minus = "-",
	#multiply = "*"
}
```

The default instance `TokenKind` can be used like an immutable associative array (but without any runtime hashing overhead). To get an associated value:

```
var v TokenKind = #plus
var c = TokenKind[v]
```

Note that all associated data must be of the same type.

Now, nothing stops you from making more dicts of enums:

```
var m Expr[TokenKind] = {
	#zero = Expr(0),
	#blank = Expr(''),
	#complex = Expr(1+3i)
}
```

But be aware that it's easy to miss certain members in a new dict (or when the enum is updated). Jet will warn you, as always, but it's just more work on your part maintaining parallel data tables instead of keeping the data *inside* the enum.

In any case, as usual in Jet, any dict that is not mutated after its definition is optimised to a compile-time lookup table. Because the key type is an enum (fully known at compile time), this implies zero lookup overhead since there is nothing to hash.

