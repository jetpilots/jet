### Numeric types

Type | C Type | Description
---|---|---
`Real`|`double`|64-bit real number type. This is the default numeric type in **F+**.
`Integer`|`intptr_t`|64-bit signed integer type.
`Rational`|-|A fraction that can be represented with two `Integer`s as `x/y`.
`Natural`|`uintptr_t`|An `Integer` that is always non-negative.
`Complex`|-|A complex number type with real and imaginary parts.
`IntRange`|-|Set of inclusive interval(s) over two `Integer` bounds each.
`RealRange`|-|Set of inclusive interval(s) over two `Real` bounds each.

 All variables initialized with numeric literals and not explicitly annotated are inferred as `Real` values, even when they are written without any decimal point.

 Other numeric types can be initialized with numeric literals as long as the type is explicitly added by the programmer:

 ```typescript
 var vec as Integer = [2, 3, 4]
 var vecc as Complex = [2, 3, 4] // this is really 2+0i, 3+0i, 4+0i
 var vrng as IntRange = 3 // this is really [3:3]
 ```

 There are some special `Real` literals:
  `nan`, `inf`, `tiny` (smallest representable `Real`), `huge` (largest representable `Real`). Except for `nan`, they can all be negated.

 `Complex` literals use the familiar form: `1i`, `3+4i`, etc.

 `IntRange` and `RealRange` can be initialized with `[a:b]` or `[a:b, c:d, e:f]` and this is understood as an interval (and disjoint interval), not an array of values. Here `a` and `b` need not be constant, they may be any numeric expressions.

 `IntRange` and `RealRange` can be formed by parsing a string, using the built-in constructors: `IntRange("[4:5]")`. In this form, only constant numeric expressions are allowed within the string to be parsed. They can be serialized and deserialized as usual, with `json()`, `bson()`, `yaml()`, `xml()`, `hdf5()`, `mat()`, and `fpb()`.

 ### Booleans

 The boolean type in **F+** is `YesOrNo`. The two literals used to represent the possible boolean values are `yes` and `no`.

 ### Strings

 There is only one `String` type.

 Under the hood it may generate to any one of a number of optimised types, such as stack-storage or heap-storage strings, resizable or fixed, and null-terminated or storing a length, depending on the context.

 ### Collections

 There are only 2 collection types: `List` and `Dict`. These are high-level abstractions and programmers are not expected to worry about the actual data structure used.

 Depending upon the context in which a collection is used, it may be converted, in the case of `List`, to a dynamic or static array, singly or doubly linked list, n-ary tree, KD-tree, resizable or fixed-size ND-array, hash set, or other more specific types, and for `Dict` to a sorted, ordered, or unordered hash map, or native JSON or YAML tree.