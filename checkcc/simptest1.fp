# declare type Text
declare type Strings
# declare type point

# declared funcs are basically implemented in C, they
# can be funcs or macros, doesn't matter here

# var ui = import("ch.gui")
# var jsm = import("jsManager.jsx", from = "sindresho/SwiftyJson", tag = "5.6")

# need an inheritance graph to avoid two types inheriting from each other or generally mutually recursive inheritance
# need a call graph to understand recursion patterns etc. and more imp. to avoid runaway recursion in the compiler when e.g. a constructor and a function depend on each other

# when do you run analyseExpr on a type? when an instance of it used in a func (vars or args)
# and on a func? whenever it encountered.
# start processing at main, and see where you go.
# that means dead code will not be analyzed, but what the heck, not my problem.

# deprecated v20200923
# export
type Expr
    var meg = 33.2
    var bx = 4 < 5
    # var b = msp(Point())
    # shared
    var f = 3<3
end type

# for enums:
# define T_value(e) -> T_vals[e]
# macro will be generated by astfunc_genc
# default ctor will fill in those values btw.
# and each enum gens its print/describe/json etc funcs.
# printing name by default not numeric value.

# export
type Another #extends Point
    var g = 12
    var exp = Expr()
end type

type YetAnother
    var g = Another()
end type

# ACTUALLY DONT BOTHER WITH CYCLE DETECTION AT ALL
# DISALLOW INHERITANCE & FAVOUR COMPOSITION
# AND ALLOW RECURSIVE CONSTRUCTOR (stack overflow checker works)
type Other # extends Another
    var m = 43
    var we = Another()
    # var po = point()
end type

type Point # extends Other
    # var p = Other()
    var x = fxfunc(3)
    var y = 69.6723
    # var o = Other()
    var z = y + 5.6 * x #+ o.we.g
    # var o = nil(other) + 6
    var cstr = "xyz"
end type

# export
fxfunc(x as Number) := x * 1.5

# export
function Point(x as Number)
    var p = Point()
    p.y = x
    describe(x)
    return p
end function

function main(args as Strings) result Number
    var po = Point()
    var pcx = Point(78)
    var mg = args
    var cm = [8, 7, 6, 5]
    # json(cm[3])
    json(po)
    var masd = YetAnother()
    json(pcx)
    var nuk = "hur$pCX+ $(mG)(8.2) 500 \$"
    var mk = 3 < 6 <= 5
    # check po.y < cm[0]+masd.g.g < 8
    json(mk)
    # var sd = po.o.we #+ ui.Window(1024x768, title = "Jim jox")
    # json(sd)
    # describe(po.o.we.exp.bx)
    # var win = Window(1024x768, offset = 100x100)
    # -- lists heterogenous, allowing subtypes of a base type
    # var controls[] = [
    #     Label("The quick:"),
    #     Label("Bright"),
    #     HStack([
    #         Label("Thirty"),
    #         Button("Start", action = fireBase),
    #         TextBox("There it is", cue = "Name", change = chngSub)
    #     ]),
    #     TreeView(
    #         JSON([
    #             .base = 34,
    #             .kind = 3
    #         ])
    #     )
    # ]

    # var fn as Function = fnict
    # if noBase then fn = fbaseless

    # var fn =
    #     if noBase then
    #         fbaseless
    #     else if whatever then
    #         fwhat
    #     else
    #         fnict

    # var controls[] as Control = [
    #     {
    #         .label = "The quick:"
    #     }
    #     {
    #         .label = "Bright"
    #     }
    #     {
    #         .hStack = [
    #             {
    #                 .label = "Thirty"
    #             }
    #             {
    #                 .button = "Start"
    #                 .action = fireBase
    #             }
    #             {
    #                 .text = "There it is"
    #                 .cue = "Name"
    #                 .changing = @validateTx
    #                 .changed = @chngSub
    #             }
    #         ]
    #     },
    #     {
    #         .tree =
    #         JSON([
    #             .base = 34,
    #             .kind = 3
    #         ])
    #     }
    # ]


    # - lx as Label
    #     text = "The quick:"
    # - lb as Label
    #     text = "Bright"
    # - hst as HStack
    #     - la as Label
    #         text = "Thirty"
    #     - lb as Button
    #         text = "Start"
    #         action = fireBase
    #     - txb as TextBox
    #         text = "There it is"
    #         cue = "Name"
    #         changing = @validateTx
    #         changed = @chngSub
    # - tree as JSON
    #     base = 34
    #     kind = 3

    # var tree = JSON('{
    #     "text" = 43,
    #     "six" = 23
    # }')

    # var tree = YAML('
    #     thirty: 30
    #     fifty: 50
    #     maps:
    #         obj1: !Object
    #             value = true
    #         arr: !Array<Int>
    #         - 3
    #         - 4
    #         - 5
    # ')

    # var doc = HTML('
    #     <html>
    #     <head>
    #         <title>The lazy dog.</title>
    #     </head>
    #     <body>
    #         <p>The quick brown fox.</b>
    #     </body>
    #     </html>
    # ')

    # -- Collection API
    # -- 1. mutators.
    # function insert(anItem as Type, into[] as Type, after as Type) result (ret[] as Type)
    # function insert(anItem as Type, into[] as Type, before as Type) result (ret[] as Type)
    # function insert(anItem as Type, into[] as Type, at as Integer) result (ret[] as Type)
    # function push(item as Type, onto[] as Type) result (ret[] as Type)
    # function pop(list as Type) result (ret[] as Type)

    # insert(3, into = &list, after = 4) or skip c

    # var v = a.b.c.e or return

    # func(x, a = y, b = z.y.u)
    #  is implicitly wrapped in IFs. so it wont be called if x y z z.y z.y.u is any null.

    # a = func(x, a = y, b = z.y.u)
    #  now this results in observable change unless a is unused. so you need to specify OR.
    # a = func(x, a = y, b = z.y.u) or 42 // or skip c

    # match error
    # case .fileNotFound
    #     print("o $error")
    # case .noPermission
    #     print("p")
    # end match

    # -- Real_insert_into_after(3, list, 4);
    # -- if (err) goto skip_c;
    # -- //if (err==TRACE) BTRACE; // since 'or' expr cannot throw
    # -- //elif (err) goto err;

    # throw .fileNotFound # if catch is present, great, no BT.
    # #if not, then this is a BT.
    # #scan the func and check handlers. 'or' is easy.
    # # if handled, set ERR=ERR_TRACE before calling the func.
    # # maybe all throwing exprs must be extracted to vars.
    # # maybe checks will need to be done for each individual err kind
    # # and the trace flag may be set for each err kind.


    # --| 2. don't look like mutators, and don't act like them unless the
    # --| variable is being overwritten or moved. So for example
    # --|     m = shuffle(m) -- shuffle works in place on m
    # --|     g = shuffle(m) -- shuffle works in place on m only if m
    # --|                    -- is unused later (m is "moved" to g)
    # --|     var l = max(m) -- technically g should be inplaced since
    # --|                    -- max is unaffected by shuffle.
    # --| Inside the function, the first thing is:
    # --|     ret = list
    # --| i.e. the return value is set to one of the arguments.
    # --| this is not a copy: if inplacing is not possible, either
    # --| because the called function has dependencies on the input or
    # --| the target variable is used later in the caller, the *caller*
    # --| should have sent a clone of the object.
    # function clear(list[] as Type) result (ret[] as Type)
    # function resize(list[] as Type) result (ret[] as Type)
    # function shuffle(list[] as Type) result (ret[] as Type)

    # var dso = HTML(read('dso.html'))

    # var vname = 'thirty'
    # var vval = 30

    # var embC = '
    #     #include <stdio.h>
    #     static int $vname = $vval;
    #     int main() {
    #         printf("basics %s: %d\n", $vname);
    #     }
    # '

    # var rgx = `
    #     ^\s+ -- beginning spaces
    #     (\w+3)dsde -- actual pattern
    #     $ -- end of line
    # `

    # json(po.o)
    describe(mk)
    funky()
    return 6
    # print(pcx)
    # po = 0
    # var pm number
    # var nzz = zeros(450)
    # nzz[2:65] = 1
    # nzz[1:2:-1] = random()
    # nzz[nzz<4] = 4
    # describe(sum(nzz[nzz<5]))
    # nzz[:] = 34
    # describe(sum(nzz))

    # sum will be promoted out, but it has an elemental op INSIDE it.
    # so in addition to the isElemental flag you need a hasElemental
    # function that dives.
    # Like promotion where the loop repeats to check for multiple
    # promotions in the same statement, the same should happen here,
    # but the current stmt should also be set back to the newly promoted
    # one so that itcan be searched for embedded elemental ops. the next
    # stmt will be the original, so elemental ops remaining will be treated
    # again # make sure that after elemental promotions are done, the
    # flag unset.

    # the problem that the term inside nzz will be promoted even though
    # it has a dependency. hopefully no one writes code like this.
    # # promoted within the enclosing 'for', not outside it
    # nzz[7:89] = nzz[1:82] + sum(nzz[7:47]+nzz[80:120]) * nzz[31:112]

    # var pox = ui.Window(400x300)
    # var pox = new point, frame = 400x300)
    # pox.x = 6
    # pox.cstr.jmp = "mereger"
    # json(po)
    # var postr = pox.json()
end function

# var mx  date] = [now(), now(), now()]
# var mx date[4,4] = [now(), now(), now()]
# var mx number|kg.m/s[6,:] = [now(), now(), now()]
# var mx Date = [now(), now(), now()]
# var mx[4,4] Date = [now(), now(), now()]
# var mx[6,:]|kg.m/s = [now(), now(), now()]

function funky()
    joyce()
end function

function joyce()
    var name = "BhAbru"
    var nam2 = "Bhabru"
    var x = fxfunc(3 + 5i + 4)
    describe(x)
    describe(x+5)
    var point = Point()
    var joyce = 55.3
    joyce += 5

    if x == 3
        print(x)
    else if x == 5
        print(x+4)
    end if

    if x == 3
        print(x)
    else
        print(x+4)
    end if

    # for xt = [6:9:-1]
    # FOR(int,xt,it_start(range(6,9)),it_end(range(6,9)),it_step(range(6,9))))
    # for (int xt=6; xt<=9; xt+=1)
    # for xt as XTDev = xtColl[]
    # FOR(int,xt,it_start(xtColl),it_end(xtColl),it_step(xtColl)))

    #     print(xt)
    # end for

    while x > 6
        x -= 1
        json(x)
    end while

    describe(joyce)
    var y = 3-x * 2.5 / x
    # check 7 < 7-x < x+y
    check x + fxfuNC(y) - fxfunc(x) >=  3*y + 5 + 2*x
    # check name == naM2
end function
