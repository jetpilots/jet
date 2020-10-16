#include <stdio.h>

/*

In Jet this looks like:

type TType
    var lev as Integer
    var ret as String
end type

var myStaticDict as TType[String] = {
    "one" = {.lev = 0, .ret = "what"},
    "two" = {.lev = 1, .ret = "whaasdat"},
    "three" = {.lev = 3, .ret = "whaasdsdt"}
}

Just be sure that myStaticDict is never changed!!! Otherwise you have to
generate it really as a Dict.

You can also check the strings passed when indexing:
print(myStaticDict["five"].ret) <-- compile-time error, "five" out of bounds

Might be problematic if user tries to enter vars for indexing. Then how about
having enums and making the intent clearer?

enum myStaticDict as TType = {
    .one = {.lev = 0, .ret = "whadasd"},
    .two = { ... }
}

How about a basic type Label? must be resolved at compile time

var myStaticDict as TType[Label] = {
    .one = {.lev = 0, .ret = "whadasd"}
    .two = { ... }
}

I think enum is best, because it provides a set of names to a numbering,
as well as associated values etc. and most of all clear intent. The numeric
values are inherent and the associated data is IN ADDITION to those. You can
always leave out associated data if you just want some func arg options etc.

After all they are IMMUTABLE so they can be at module level.

enum blendModes = {
    .diffuse,
    .colorBurn,
    .colorDodge,
    .dissolve,
    .normal
}

(in own module)
var b as ui.gfx.blendModes = .diffuse

But I want to be able to convert them to/from a string, this is how users code.
Enums do provide inbuilt serialisation, but no way to convert to/from an arbit
string. you need this:
how about callable enums?
var b = ui.gfx.blendModes(ask()) or .normal

That's why I think you should let an "apparent" Dict generate to this fancy
stuff behind the scenes - users like Pythonic stuff and they WILL just write
Dicts and not bother to care about enums. Then you have funcs to write/read
integral values based on string names (or complain if out of range).

But enum has advantages that it can be used for autocomplete
blendModes. <== poppup autocomplete dialog

For enums, if you have associated data generate the numeric values as 1,2,3,4
if you dont, then generate as 1,2,4,8 so they can be combined. NO I want to use
indivudual bitfield booleans instead of combining flags!

Just have a warning to convert detected static dicts to enums for higher
performance! sorted

*/

struct TType {
    int lev;
    char* ret;
};
enum myStDict__indxs { one, two, three };
static const struct TType myStDict[] = {
    [one] = { 0, "what" },
    [two] = { 1, "whasdat" },
    [three] = { 2, "whadasdt" },
};

int main()
{
    printf("%s\n", myStDict[two].ret);
    return 0;
}