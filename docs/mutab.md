
Mutable | Immutable
-- | --
Array | Slice
 . | String
 . | Filter
 . | Selection

Slice, Filter, Selection are immutable when assigned to a name. If passed directly to a mutating function's first arg, they are mutable.

```
var arr[] = [1, 2, 3, 4, 5] # array, owned
var sli[] = arr[1:3] # slice, refs arr
mfunc!(sli[]) # this is an error, sli is immutable
mfunc!(arr[1:3]) # this is ok, the slice is not named
mfunc!(arr[]) # this is obviously ok
sli[:] = 0 # err
arr[1:3] = 0 # ok
```

Slice: subarray over range
Filter: subarray by predicate
Selection: subarray by list of indices
```
arr[] = [1, 2, 3, 4, 5] # array, owned
sli[] = arr[1:3] # slice, refs arr
fil[] = arr[arr<=3] # filter, refs arr
sel[] = arr[[1,3,4]] # selection, refs arr

```

Big Question is
can I do
```
arr = [1, 2, 6]
```
to drop the old value of arr and assign a new array

If the rule of always having [] in the name of all collections
then it becomes
```
arr[] = [1, 2, 6]
# which means
arr[:] = [1, 2, 6, 0, 0]

arr[] = zeros([6, 7])
arr[] = ones([8, 9]) # allowed??
```



String is always immutable once created. If you want to build strings, use String[] and join.

NOTHING IS EVER COPIED IN JET unless you call copy(). You can call it on everything in Jet: single objects, arrays, slices, filters, selections, strings. It returns array for slice, filter or selection and the arg's type for everything else.

If you really want to modify a string you can cheat:
```
bytes(str as String) as Byte[]
```
which is a mutable array (it is the actual storage of str, no copy).