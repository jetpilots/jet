
## Parsing
#### [✓] parse + resolve vars, imports
(recursive for scopes, otherwise iterative)

## Analysis

### Pass1

#### [✓] Infer (or check) and propagate type info

#### [✓] Resolve func calls, type specifications

#### [✓] Count usages of types, funcs, (and changes to) vars

#### [...] Interval arithmetic, compile-time fact checks, etc.

### --> can reformat at this point.
### --> no further errors.

### Interlude
#### [✓] check call graph & mark recursive funcs (& types?)

### Pass2
#### [...] decide var storage class

## Optimisation
#### [...] CSE
#### [✓] extract vars
#### [...] convert + - * / etc to += -= *= /= ...(only for arrays to avoid temps)
#### [...] scalarize array ops (wrap in loop)
#### [...] inplacing for funcs that support it
#### [...] move var decls down as close as possible to first usage (minimize lifetime)
#### [...] move var decls up (maximize lifetime) for async vars

## Save processed module
#### [...] Write out in binary format

## Emit C99 source
#### [✓] emit header
#### [✓] insert drops for locals after their last usage
#### [✓] generate format strings for interpolation
#### [✓] emit types, funcs, exprs

## Build object file

## Link into executable

## Spawn executable
