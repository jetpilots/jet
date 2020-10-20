### Timing

```tsx
var t0|s = clock()
```

Gets the time in seconds measured from the start of the program.

```tsx
var elapsed|ms = clock() - t0
```

Compute the time difference between a fresh and a previous `clock()` invocation.

```js
describe(elapsed)
>> elapsed|ms = 3.14159
```



> Note: `clock()` gets the time since the UNIX epoch in nanoseconds, subtracts the time of the program start, and returns this converted to `double` .



The best way would be to just return a `DateTime` (internally represented as seconds from epoch in UTC) and then subtract if you like. That way 

- you don't do the extra sub in each call to `clock()` and
- if someone prints the return value of `clock()` it is a sensible value (it actually pretty-prints the time instant)
- `clock()` can just be called `now()`

