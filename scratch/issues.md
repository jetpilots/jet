### Known issues



#### Analysis

- Test bodies are not checked for errors unless `mode == PMGenTests`. This is not a bug, since you don't want to waste time checking what you aren't generating. Probably tests should be disallowed in normal files and should go into files of their own in `tests/`. Or like Go they can have a file next to each module. Or you can leave tests embedded in the module and not care until they are run. 



