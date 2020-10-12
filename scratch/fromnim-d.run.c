      rpn: (null) Integrator (null) NumFunc (null) Real (null) Real ,  ,  (null) Real = 
      p: (null) 
      rpn: f (null) NumFunc x (null) Real h (null) Real ,  ,  (call) x (call)  :=  
      result: f 
      p: (null) 
      rpn: f (null) NumFunc x (null) Real h (null) Real ,  ,  (call) x h 2 / + (call)  :=  
      result: f 
      p: (null) 
      rpn: f (null) NumFunc x (null) Real h (null) Real ,  ,  (call) x h + (call)  :=  
      result: f 
      p: (null) 
      rpn: f (null) NumFunc x (null) Real h (null) Real ,  ,  (call) x (call) x h + (call) + 2
simpson(f as NumFunc, x as Real, h as Real) := (f(x) + 4*f(x+h/2) + f(x+h)) / 6

alias Integrator := Function(NumFunc, Real, Real)(Real)

# like Integrator
# common signature (NumFunc, as Real, as Real)(Real)
#     leftRect(f, x, h) := f(x)
#     midRect(f, x, h) :=  f(x + h/2)
#     rightRect(f, x, h) := f(x + h)
#     trapezium(f, x, h) := (f(x) + f(x+h)) / 2
#     simpson(f, x, h) := (f(x) + 4*f(x+h/2) + f(x+h)) / 6
# end common

# like Integrator
# export function simpson(f, x, h) result (s)
#     s = (f(x) + 4*f(x+h/2) + f(x+h)) / 6
# end function

type Integrator := Function(Real|N/m)(Real)
function ident(x as Real) := x
function recip(x as Real) := 1 / x
function cubed(x as Real) := x^3

# type NumFunc = function (Real|N/m) result (Real)
# like NumFunc ident(x as Real) := x
# like NumFunc recip(x as Real) := 1 / x
# like NumFunc cubed(x as Real) := x^3

randTestFunc() as NumFunc := random([ident, recip, cubed])

# template random(array T[]) T := array[random(from=1, to=len(array))]

# Integrator
# leftRect(f, x, h) := f(x)
# midRect(f, x, h) :=  f(x + h/2)
# rightRect(f, x, h) := f(x + h)
# trapezium(f, x, h) := (f(x) + f(x+h)) / 2
# simpson(f, x, h) := (f(x) + 4*f(x+h/2) + f(x+h)) / 6

# NOOOOOOOO ident/recip etc are NOT NumFuncs, but definitions that match
# NumFunc! var a NumFunc = ident would be a NumFunc

# -- >8 -------------- NOPE
# type NumFunc as Real func(Real x)
# NumFunc ident := x
# NumFunc recip := 1/x
# NumFunc cubed := x^3

# type Integrator as Real func(NumFunc f, as Real x, h)
# Integrator leftRect := f(x)
# Integrator midRect := f(x + h/2)
# Integrator rightRect := f(x + h)
# Integrator trapezium := (f(x) + f(x+h)) / 2
# Integrator simpson := (f(x) + 4*f(x+h/2) + f(x+h)) / 6


# type Integrator = func(f NumFunc, x as Real, h as Real) as Real
# var leftRect Integrator := f(x)
# var midRect Integrator := f(x + h/2)
# var rightRect Integrator := f(x + h)
# var trapezium Integrator := (f(x) + f(x+h)) / 2
# var simpson Integrator := (f(x) + 4*f(x+h/2) + f(x+h)) / 6

# type NumFunc = func(x as Real) as Real
# var ident NumFunc := x
# var recip NumFunc := 1 / x
# var cubed NumFunc := x^3
# ----- NOPE ^^ ----------

# type Funcy = function(d Reak) Reaj
# ident(d) = d+2

function integrate(f = NumFunc, a as Real, b as Real, steps as Real, meth = Integrator) result (s as Real)
    var h = (b-a) / steps
    for i in 0:steps-1 do s += meth(f, x=a+i*h, h=h)
    s *= h
end function

function integrate(f NumFunc, from as Real, to as Real, steps as Real, meth Function(NumFunc, Real, Real)(Real)) result (s as Real)

function integrate(f<NumFunc>, from<Real>, to<Real>, steps<Real>, meth<Function(NumFunc, Real, Real)(Real)>) result (s <Real>)

function integrate(f NumFunc, from as Real, to as Real, steps as Real, meth as Function(NumFunc, Real, Real)(Real)) result (s as Real)

function integrate(f NumFunc, from as Real, to as Real, steps as Real, meth Function(NumFunc, Real, Real):=(Real)) result (s as Real)

# function integrate(NumFunc f, as Real from, as Real to, as Real steps, Function(NumFunc, as Real, as Real)(Real) meth) result (Real s)

function integrate(f NumFunc, from as Real, to as Real, steps as Real, meth function (NumFunc, as Real, as Real) result (Real)) result (s as Real)

    var h as Real = (to-from) / steps
    for i in 0:steps-1 do s += meth(f, x=from+i*h, h=h)
    s *= h

    var meth2 Function(NumFunc, as Real, as Real):=(Real) = simpson

    var meth2 Function(NumFunc, as Real, as Real)(Real) = simpson

    var meth2 as Function(NumFunc, Real, Real)(Real) = simpson # I pick this one

    var meth2 <Function(NumFunc, as Real, as Real)(Real)> = simpson

    # var as Function(NumFunc, as Real, as Real)(Real) meth2 = simpson

    # as Real x := 3

    var meth2 function(NumFunc, as Real, as Real) result (Real) = simpson

end function


# type TestCase
#     var a = 0
#     var b = 1
#     var steps = 100
#     var fun = identity
# end type

function main()
    for tc as TestCase in [ #create objs of an unnamed type which is inferred on the fly with member names & tyoes. mayb not a good idea since it cant be clearly annotated
        { from = 0, to =   1, steps = 100, fun = cubed },
        { from = 1, to = 100, steps = 1|k, fun = recip },
        { from = 0, to = 5|k, steps = 5|M, fun = ident },
        { from = 0, to = 6|k, steps = 6|M, fun = ident }] # if later found to be passed somewhere, take that a hint for the type

        write("$tc.fun integrated from $tc.from to $tc.to ($tc.steps steps) using...") # you can stringify funcs and types to get their name

        for meth as Integrator in [leftRect, midRect, rightRect, trapezium, simpson]
            var ret as Real = integrate(tc.fun, from=tc.from, to=tc.to, steps=tc.steps, meth=meth)
            write("   $meth: $ret")
        end for
    end for
end function
 /  :=  
      result: f 
      p: (null) 
      rpn: f (null) NumFunc x (null) Real h (null) Real ,  ,  (call) x (call) 4 x h 2 / + (call) * + x h + (call) + 6

alias Integrator := Function(NumFunc, Real, Real)(Real)

# like Integrator
# common signature (NumFunc, as Real, as Real)(Real)
#     leftRect(f, x, h) := f(x)
#     midRect(f, x, h) :=  f(x + h/2)
#     rightRect(f, x, h) := f(x + h)
#     trapezium(f, x, h) := (f(x) + f(x+h)) / 2
#     simpson(f, x, h) := (f(x) + 4*f(x+h/2) + f(x+h)) / 6
# end common

# like Integrator
# export function simpson(f, x, h) result (s)
#     s = (f(x) + 4*f(x+h/2) + f(x+h)) / 6
# end function

type Integrator := Function(Real|N/m)(Real)
function ident(x as Real) := x
function recip(x as Real) := 1 / x
function cubed(x as Real) := x^3

# type NumFunc = function (Real|N/m) result (Real)
# like NumFunc ident(x as Real) := x
# like NumFunc recip(x as Real) := 1 / x
# like NumFunc cubed(x as Real) := x^3

randTestFunc() as NumFunc := random([ident, recip, cubed])

# template random(array T[]) T := array[random(from=1, to=len(array))]

# Integrator
# leftRect(f, x, h) := f(x)
# midRect(f, x, h) :=  f(x + h/2)
# rightRect(f, x, h) := f(x + h)
# trapezium(f, x, h) := (f(x) + f(x+h)) / 2
# simpson(f, x, h) := (f(x) + 4*f(x+h/2) + f(x+h)) / 6

# NOOOOOOOO ident/recip etc are NOT NumFuncs, but definitions that match
# NumFunc! var a NumFunc = ident would be a NumFunc

# -- >8 -------------- NOPE
# type NumFunc as Real func(Real x)
# NumFunc ident := x
# NumFunc recip := 1/x
# NumFunc cubed := x^3

# type Integrator as Real func(NumFunc f, as Real x, h)
# Integrator leftRect := f(x)
# Integrator midRect := f(x + h/2)
# Integrator rightRect := f(x + h)
# Integrator trapezium := (f(x) + f(x+h)) / 2
# Integrator simpson := (f(x) + 4*f(x+h/2) + f(x+h)) / 6


# type Integrator = func(f NumFunc, x as Real, h as Real) as Real
# var leftRect Integrator := f(x)
# var midRect Integrator := f(x + h/2)
# var rightRect Integrator := f(x + h)
# var trapezium Integrator := (f(x) + f(x+h)) / 2
# var simpson Integrator := (f(x) + 4*f(x+h/2) + f(x+h)) / 6

# type NumFunc = func(x as Real) as Real
# var ident NumFunc := x
# var recip NumFunc := 1 / x
# var cubed NumFunc := x^3
# ----- NOPE ^^ ----------

# type Funcy = function(d Reak) Reaj
# ident(d) = d+2

function integrate(f = NumFunc, a as Real, b as Real, steps as Real, meth = Integrator) result (s as Real)
    var h = (b-a) / steps
    for i in 0:steps-1 do s += meth(f, x=a+i*h, h=h)
    s *= h
end function

function integrate(f NumFunc, from as Real, to as Real, steps as Real, meth Function(NumFunc, Real, Real)(Real)) result (s as Real)

function integrate(f<NumFunc>, from<Real>, to<Real>, steps<Real>, meth<Function(NumFunc, Real, Real)(Real)>) result (s <Real>)

function integrate(f NumFunc, from as Real, to as Real, steps as Real, meth as Function(NumFunc, Real, Real)(Real)) result (s as Real)

function integrate(f NumFunc, from as Real, to as Real, steps as Real, meth Function(NumFunc, Real, Real):=(Real)) result (s as Real)

# function integrate(NumFunc f, as Real from, as Real to, as Real steps, Function(NumFunc, as Real, as Real)(Real) meth) result (Real s)

function integrate(f NumFunc, from as Real, to as Real, steps as Real, meth function (NumFunc, as Real, as Real) result (Real)) result (s as Real)

    var h as Real = (to-from) / steps
    for i in 0:steps-1 do s += meth(f, x=from+i*h, h=h)
    s *= h

    var meth2 Function(NumFunc, as Real, as Real):=(Real) = simpson

    var meth2 Function(NumFunc, as Real, as Real)(Real) = simpson

    var meth2 as Function(NumFunc, Real, Real)(Real) = simpson # I pick this one

    var meth2 <Function(NumFunc, as Real, as Real)(Real)> = simpson

    # var as Function(NumFunc, as Real, as Real)(Real) meth2 = simpson

    # as Real x := 3

    var meth2 function(NumFunc, as Real, as Real) result (Real) = simpson

end function


# type TestCase
#     var a = 0
#     var b = 1
#     var steps = 100
#     var fun = identity
# end type

function main()
    for tc as TestCase in [ #create objs of an unnamed type which is inferred on the fly with member names & tyoes. mayb not a good idea since it cant be clearly annotated
        { from = 0, to =   1, steps = 100, fun = cubed },
        { from = 1, to = 100, steps = 1|k, fun = recip },
        { from = 0, to = 5|k, steps = 5|M, fun = ident },
        { from = 0, to = 6|k, steps = 6|M, fun = ident }] # if later found to be passed somewhere, take that a hint for the type

        write("$tc.fun integrated from $tc.from to $tc.to ($tc.steps steps) using...") # you can stringify funcs and types to get their name

        for meth as Integrator in [leftRect, midRect, rightRect, trapezium, simpson]
            var ret as Real = integrate(tc.fun, from=tc.from, to=tc.to, steps=tc.steps, meth=meth)
            write("   $meth: $ret")
        end for
    end for
end function
 /  :=  
      result: f 
      p: (null) 
      rpn: alias Integrator (null) NumFunc Real Real ,  ,  Real  :=  
      result: alias Integrator 
      p: (null) 
      rpn: (null) Integrator (null) Real|N/m (null) Real  :=  
      p: (null) 
      rpn: (null) x (null) Real (call) x
function recip(x as Real) := 1 / x
function cubed(x as Real) := x^3

# type NumFunc = function (Real|N/m) result (Real)
# like NumFunc ident(x as Real) := x
# like NumFunc recip(x as Real) := 1 / x
# like NumFunc cubed(x as Real) := x^3

randTestFunc() as NumFunc := random([ident, recip, cubed])

# template random(array T[]) T := array[random(from=1, to=len(array))]

# Integrator
# leftRect(f, x, h) := f(x)
# midRect(f, x, h) :=  f(x + h/2)
# rightRect(f, x, h) := f(x + h)
# trapezium(f, x, h) := (f(x) + f(x+h)) / 2
# simpson(f, x, h) := (f(x) + 4*f(x+h/2) + f(x+h)) / 6

# NOOOOOOOO ident/recip etc are NOT NumFuncs, but definitions that match
# NumFunc! var a NumFunc = ident would be a NumFunc

# -- >8 -------------- NOPE
# type NumFunc as Real func(Real x)
# NumFunc ident := x
# NumFunc recip := 1/x
# NumFunc cubed := x^3

# type Integrator as Real func(NumFunc f, as Real x, h)
# Integrator leftRect := f(x)
# Integrator midRect := f(x + h/2)
# Integrator rightRect := f(x + h)
# Integrator trapezium := (f(x) + f(x+h)) / 2
# Integrator simpson := (f(x) + 4*f(x+h/2) + f(x+h)) / 6


# type Integrator = func(f NumFunc, x as Real, h as Real) as Real
# var leftRect Integrator := f(x)
# var midRect Integrator := f(x + h/2)
# var rightRect Integrator := f(x + h)
# var trapezium Integrator := (f(x) + f(x+h)) / 2
# var simpson Integrator := (f(x) + 4*f(x+h/2) + f(x+h)) / 6

# type NumFunc = func(x as Real) as Real
# var ident NumFunc := x
# var recip NumFunc := 1 / x
# var cubed NumFunc := x^3
# ----- NOPE ^^ ----------

# type Funcy = function(d Reak) Reaj
# ident(d) = d+2

function integrate(f = NumFunc, a as Real, b as Real, steps as Real, meth = Integrator) result (s as Real)
    var h = (b-a) / steps
    for i in 0:steps-1 do s += meth(f, x=a+i*h, h=h)
    s *= h
end function

function integrate(f NumFunc, from as Real, to as Real, steps as Real, meth Function(NumFunc, Real, Real)(Real)) result (s as Real)

function integrate(f<NumFunc>, from<Real>, to<Real>, steps<Real>, meth<Function(NumFunc, Real, Real)(Real)>) result (s <Real>)

function integrate(f NumFunc, from as Real, to as Real, steps as Real, meth as Function(NumFunc, Real, Real)(Real)) result (s as Real)

function integrate(f NumFunc, from as Real, to as Real, steps as Real, meth Function(NumFunc, Real, Real):=(Real)) result (s as Real)

# function integrate(NumFunc f, as Real from, as Real to, as Real steps, Function(NumFunc, as Real, as Real)(Real) meth) result (Real s)

function integrate(f NumFunc, from as Real, to as Real, steps as Real, meth function (NumFunc, as Real, as Real) result (Real)) result (s as Real)

    var h as Real = (to-from) / steps
    for i in 0:steps-1 do s += meth(f, x=from+i*h, h=h)
    s *= h

    var meth2 Function(NumFunc, as Real, as Real):=(Real) = simpson

    var meth2 Function(NumFunc, as Real, as Real)(Real) = simpson

    var meth2 as Function(NumFunc, Real, Real)(Real) = simpson # I pick this one

    var meth2 <Function(NumFunc, as Real, as Real)(Real)> = simpson

    # var as Function(NumFunc, as Real, as Real)(Real) meth2 = simpson

    # as Real x := 3

    var meth2 function(NumFunc, as Real, as Real) result (Real) = simpson

end function


# type TestCase
#     var a = 0
#     var b = 1
#     var steps = 100
#     var fun = identity
# end type

function main()
    for tc as TestCase in [ #create objs of an unnamed type which is inferred on the fly with member names & tyoes. mayb not a good idea since it cant be clearly annotated
        { from = 0, to =   1, steps = 100, fun = cubed },
        { from = 1, to = 100, steps = 1|k, fun = recip },
        { from = 0, to = 5|k, steps = 5|M, fun = ident },
        { from = 0, to = 6|k, steps = 6|M, fun = ident }] # if later found to be passed somewhere, take that a hint for the type

        write("$tc.fun integrated from $tc.from to $tc.to ($tc.steps steps) using...") # you can stringify funcs and types to get their name

        for meth as Integrator in [leftRect, midRect, rightRect, trapezium, simpson]
            var ret as Real = integrate(tc.fun, from=tc.from, to=tc.to, steps=tc.steps, meth=meth)
            write("   $meth: $ret")
        end for
    end for
end function
  :=  
      p: (null) 
      rpn: (null) x (null) Real (call) 1 x
function cubed(x as Real) := x^3

# type NumFunc = function (Real|N/m) result (Real)
# like NumFunc ident(x as Real) := x
# like NumFunc recip(x as Real) := 1 / x
# like NumFunc cubed(x as Real) := x^3

randTestFunc() as NumFunc := random([ident, recip, cubed])

# template random(array T[]) T := array[random(from=1, to=len(array))]

# Integrator
# leftRect(f, x, h) := f(x)
# midRect(f, x, h) :=  f(x + h/2)
# rightRect(f, x, h) := f(x + h)
# trapezium(f, x, h) := (f(x) + f(x+h)) / 2
# simpson(f, x, h) := (f(x) + 4*f(x+h/2) + f(x+h)) / 6

# NOOOOOOOO ident/recip etc are NOT NumFuncs, but definitions that match
# NumFunc! var a NumFunc = ident would be a NumFunc

# -- >8 -------------- NOPE
# type NumFunc as Real func(Real x)
# NumFunc ident := x
# NumFunc recip := 1/x
# NumFunc cubed := x^3

# type Integrator as Real func(NumFunc f, as Real x, h)
# Integrator leftRect := f(x)
# Integrator midRect := f(x + h/2)
# Integrator rightRect := f(x + h)
# Integrator trapezium := (f(x) + f(x+h)) / 2
# Integrator simpson := (f(x) + 4*f(x+h/2) + f(x+h)) / 6


# type Integrator = func(f NumFunc, x as Real, h as Real) as Real
# var leftRect Integrator := f(x)
# var midRect Integrator := f(x + h/2)
# var rightRect Integrator := f(x + h)
# var trapezium Integrator := (f(x) + f(x+h)) / 2
# var simpson Integrator := (f(x) + 4*f(x+h/2) + f(x+h)) / 6

# type NumFunc = func(x as Real) as Real
# var ident NumFunc := x
# var recip NumFunc := 1 / x
# var cubed NumFunc := x^3
# ----- NOPE ^^ ----------

# type Funcy = function(d Reak) Reaj
# ident(d) = d+2

function integrate(f = NumFunc, a as Real, b as Real, steps as Real, meth = Integrator) result (s as Real)
    var h = (b-a) / steps
    for i in 0:steps-1 do s += meth(f, x=a+i*h, h=h)
    s *= h
end function

function integrate(f NumFunc, from as Real, to as Real, steps as Real, meth Function(NumFunc, Real, Real)(Real)) result (s as Real)

function integrate(f<NumFunc>, from<Real>, to<Real>, steps<Real>, meth<Function(NumFunc, Real, Real)(Real)>) result (s <Real>)

function integrate(f NumFunc, from as Real, to as Real, steps as Real, meth as Function(NumFunc, Real, Real)(Real)) result (s as Real)

function integrate(f NumFunc, from as Real, to as Real, steps as Real, meth Function(NumFunc, Real, Real):=(Real)) result (s as Real)

# function integrate(NumFunc f, as Real from, as Real to, as Real steps, Function(NumFunc, as Real, as Real)(Real) meth) result (Real s)

function integrate(f NumFunc, from as Real, to as Real, steps as Real, meth function (NumFunc, as Real, as Real) result (Real)) result (s as Real)

    var h as Real = (to-from) / steps
    for i in 0:steps-1 do s += meth(f, x=from+i*h, h=h)
    s *= h

    var meth2 Function(NumFunc, as Real, as Real):=(Real) = simpson

    var meth2 Function(NumFunc, as Real, as Real)(Real) = simpson

    var meth2 as Function(NumFunc, Real, Real)(Real) = simpson # I pick this one

    var meth2 <Function(NumFunc, as Real, as Real)(Real)> = simpson

    # var as Function(NumFunc, as Real, as Real)(Real) meth2 = simpson

    # as Real x := 3

    var meth2 function(NumFunc, as Real, as Real) result (Real) = simpson

end function


# type TestCase
#     var a = 0
#     var b = 1
#     var steps = 100
#     var fun = identity
# end type

function main()
    for tc as TestCase in [ #create objs of an unnamed type which is inferred on the fly with member names & tyoes. mayb not a good idea since it cant be clearly annotated
        { from = 0, to =   1, steps = 100, fun = cubed },
        { from = 1, to = 100, steps = 1|k, fun = recip },
        { from = 0, to = 5|k, steps = 5|M, fun = ident },
        { from = 0, to = 6|k, steps = 6|M, fun = ident }] # if later found to be passed somewhere, take that a hint for the type

        write("$tc.fun integrated from $tc.from to $tc.to ($tc.steps steps) using...") # you can stringify funcs and types to get their name

        for meth as Integrator in [leftRect, midRect, rightRect, trapezium, simpson]
            var ret as Real = integrate(tc.fun, from=tc.from, to=tc.to, steps=tc.steps, meth=meth)
            write("   $meth: $ret")
        end for
    end for
end function
 /  :=  
      p: (null) 
      rpn: (null) x (null) Real (call) x 3

# type NumFunc = function (Real|N/m) result (Real)
# like NumFunc ident(x as Real) := x
# like NumFunc recip(x as Real) := 1 / x
# like NumFunc cubed(x as Real) := x^3

randTestFunc() as NumFunc := random([ident, recip, cubed])

# template random(array T[]) T := array[random(from=1, to=len(array))]

# Integrator
# leftRect(f, x, h) := f(x)
# midRect(f, x, h) :=  f(x + h/2)
# rightRect(f, x, h) := f(x + h)
# trapezium(f, x, h) := (f(x) + f(x+h)) / 2
# simpson(f, x, h) := (f(x) + 4*f(x+h/2) + f(x+h)) / 6

# NOOOOOOOO ident/recip etc are NOT NumFuncs, but definitions that match
# NumFunc! var a NumFunc = ident would be a NumFunc

# -- >8 -------------- NOPE
# type NumFunc as Real func(Real x)
# NumFunc ident := x
# NumFunc recip := 1/x
# NumFunc cubed := x^3

# type Integrator as Real func(NumFunc f, as Real x, h)
# Integrator leftRect := f(x)
# Integrator midRect := f(x + h/2)
# Integrator rightRect := f(x + h)
# Integrator trapezium := (f(x) + f(x+h)) / 2
# Integrator simpson := (f(x) + 4*f(x+h/2) + f(x+h)) / 6


# type Integrator = func(f NumFunc, x as Real, h as Real) as Real
# var leftRect Integrator := f(x)
# var midRect Integrator := f(x + h/2)
# var rightRect Integrator := f(x + h)
# var trapezium Integrator := (f(x) + f(x+h)) / 2
# var simpson Integrator := (f(x) + 4*f(x+h/2) + f(x+h)) / 6

# type NumFunc = func(x as Real) as Real
# var ident NumFunc := x
# var recip NumFunc := 1 / x
# var cubed NumFunc := x^3
# ----- NOPE ^^ ----------

# type Funcy = function(d Reak) Reaj
# ident(d) = d+2

function integrate(f = NumFunc, a as Real, b as Real, steps as Real, meth = Integrator) result (s as Real)
    var h = (b-a) / steps
    for i in 0:steps-1 do s += meth(f, x=a+i*h, h=h)
    s *= h
end function

function integrate(f NumFunc, from as Real, to as Real, steps as Real, meth Function(NumFunc, Real, Real)(Real)) result (s as Real)

function integrate(f<NumFunc>, from<Real>, to<Real>, steps<Real>, meth<Function(NumFunc, Real, Real)(Real)>) result (s <Real>)

function integrate(f NumFunc, from as Real, to as Real, steps as Real, meth as Function(NumFunc, Real, Real)(Real)) result (s as Real)

function integrate(f NumFunc, from as Real, to as Real, steps as Real, meth Function(NumFunc, Real, Real):=(Real)) result (s as Real)

# function integrate(NumFunc f, as Real from, as Real to, as Real steps, Function(NumFunc, as Real, as Real)(Real) meth) result (Real s)

function integrate(f NumFunc, from as Real, to as Real, steps as Real, meth function (NumFunc, as Real, as Real) result (Real)) result (s as Real)

    var h as Real = (to-from) / steps
    for i in 0:steps-1 do s += meth(f, x=from+i*h, h=h)
    s *= h

    var meth2 Function(NumFunc, as Real, as Real):=(Real) = simpson

    var meth2 Function(NumFunc, as Real, as Real)(Real) = simpson

    var meth2 as Function(NumFunc, Real, Real)(Real) = simpson # I pick this one

    var meth2 <Function(NumFunc, as Real, as Real)(Real)> = simpson

    # var as Function(NumFunc, as Real, as Real)(Real) meth2 = simpson

    # as Real x := 3

    var meth2 function(NumFunc, as Real, as Real) result (Real) = simpson

end function


# type TestCase
#     var a = 0
#     var b = 1
#     var steps = 100
#     var fun = identity
# end type

function main()
    for tc as TestCase in [ #create objs of an unnamed type which is inferred on the fly with member names & tyoes. mayb not a good idea since it cant be clearly annotated
        { from = 0, to =   1, steps = 100, fun = cubed },
        { from = 1, to = 100, steps = 1|k, fun = recip },
        { from = 0, to = 5|k, steps = 5|M, fun = ident },
        { from = 0, to = 6|k, steps = 6|M, fun = ident }] # if later found to be passed somewhere, take that a hint for the type

        write("$tc.fun integrated from $tc.from to $tc.to ($tc.steps steps) using...") # you can stringify funcs and types to get their name

        for meth as Integrator in [leftRect, midRect, rightRect, trapezium, simpson]
            var ret as Real = integrate(tc.fun, from=tc.from, to=tc.to, steps=tc.steps, meth=meth)
            write("   $meth: $ret")
        end for
    end for
end function
 ^  :=  
      p: (null) 
      rpn: NUL (null) NumFunc (call) ident recip cubed ,  ,  [ (call)  :=  
      result: NUL 
      p: (null) 
      rpn: (null) f NumFunc = a (null) Real b (null) Real steps (null) Real meth Integrator = ,  ,  ,  ,  (null) s (null) Real (call) 
      p: (null) 
      rpn: (null) 
      p: (null) 
      rpn: (null) f NumFunc from (null) Real to (null) Real steps (null) Real meth (null) NumFunc Real Real ,  ,  Real ,  ,  ,  ,  (null) s (null) Real (call) 
      p: (null) 
      rpn: (null) f NumFunc < > from Real < > to Real < > steps Real < > meth (null) NumFunc Real Real ,  ,  Real < > ,  ,  ,  ,  (null) s Real < > (call) 
      p: (null) 
      rpn: (null) f NumFunc from (null) Real to (null) Real steps (null) Real meth (null) (null) NumFunc Real Real ,  ,  Real ,  ,  ,  ,  (null) s (null) Real (call) 
      p: (null) 
      rpn: (null) f NumFunc from (null) Real to (null) Real steps (null) Real meth (null) NumFunc Real Real ,  ,  Real  :=  ,  ,  ,  ,  (null) s (null) Real (call) 
      p: (null) 
      rpn: (null) f NumFunc from (null) Real to (null) Real steps (null) Real meth (null) NumFunc (null) Real (null) Real ,  ,  (null) Real ,  ,  ,  ,  (null) s (null) Real (call) 
      p: (null) 
      rpn: NumFunc (null) Real (null) Real ,  ,  Real simpson

    var meth2 Function(NumFunc, as Real, as Real)(Real) = simpson

    var meth2 as Function(NumFunc, Real, Real)(Real) = simpson # I pick this one

    var meth2 <Function(NumFunc, as Real, as Real)(Real)> = simpson

    # var as Function(NumFunc, as Real, as Real)(Real) meth2 = simpson

    # as Real x := 3

    var meth2 function(NumFunc, as Real, as Real) result (Real) = simpson

end function


# type TestCase
#     var a = 0
#     var b = 1
#     var steps = 100
#     var fun = identity
# end type

function main()
    for tc as TestCase in [ #create objs of an unnamed type which is inferred on the fly with member names & tyoes. mayb not a good idea since it cant be clearly annotated
        { from = 0, to =   1, steps = 100, fun = cubed },
        { from = 1, to = 100, steps = 1|k, fun = recip },
        { from = 0, to = 5|k, steps = 5|M, fun = ident },
        { from = 0, to = 6|k, steps = 6|M, fun = ident }] # if later found to be passed somewhere, take that a hint for the type

        write("$tc.fun integrated from $tc.from to $tc.to ($tc.steps steps) using...") # you can stringify funcs and types to get their name

        for meth as Integrator in [leftRect, midRect, rightRect, trapezium, simpson]
            var ret as Real = integrate(tc.fun, from=tc.from, to=tc.to, steps=tc.steps, meth=meth)
            write("   $meth: $ret")
        end for
    end for
end function
 =  :=  
      result: NumFunc 
      p: (null) 
      rpn: NumFunc (null) Real (null) Real ,  ,  Real simpson

    var meth2 as Function(NumFunc, Real, Real)(Real) = simpson # I pick this one

    var meth2 <Function(NumFunc, as Real, as Real)(Real)> = simpson

    # var as Function(NumFunc, as Real, as Real)(Real) meth2 = simpson

    # as Real x := 3

    var meth2 function(NumFunc, as Real, as Real) result (Real) = simpson

end function


# type TestCase
#     var a = 0
#     var b = 1
#     var steps = 100
#     var fun = identity
# end type

function main()
    for tc as TestCase in [ #create objs of an unnamed type which is inferred on the fly with member names & tyoes. mayb not a good idea since it cant be clearly annotated
        { from = 0, to =   1, steps = 100, fun = cubed },
        { from = 1, to = 100, steps = 1|k, fun = recip },
        { from = 0, to = 5|k, steps = 5|M, fun = ident },
        { from = 0, to = 6|k, steps = 6|M, fun = ident }] # if later found to be passed somewhere, take that a hint for the type

        write("$tc.fun integrated from $tc.from to $tc.to ($tc.steps steps) using...") # you can stringify funcs and types to get their name

        for meth as Integrator in [leftRect, midRect, rightRect, trapezium, simpson]
            var ret as Real = integrate(tc.fun, from=tc.from, to=tc.to, steps=tc.steps, meth=meth)
            write("   $meth: $ret")
        end for
    end for
end function
 = 
      result: NumFunc 
      p: (null) 
      rpn: NumFunc Real Real ,  ,  Real simpson = 
      result: ,  = 
      p: = 
      rpn: (null) NumFunc (null) Real (null) Real ,  ,  Real > simpson

    # var as Function(NumFunc, as Real, as Real)(Real) meth2 = simpson

    # as Real x := 3

    var meth2 function(NumFunc, as Real, as Real) result (Real) = simpson

end function


# type TestCase
#     var a = 0
#     var b = 1
#     var steps = 100
#     var fun = identity
# end type

function main()
    for tc as TestCase in [ #create objs of an unnamed type which is inferred on the fly with member names & tyoes. mayb not a good idea since it cant be clearly annotated
        { from = 0, to =   1, steps = 100, fun = cubed },
        { from = 1, to = 100, steps = 1|k, fun = recip },
        { from = 0, to = 5|k, steps = 5|M, fun = ident },
        { from = 0, to = 6|k, steps = 6|M, fun = ident }] # if later found to be passed somewhere, take that a hint for the type

        write("$tc.fun integrated from $tc.from to $tc.to ($tc.steps steps) using...") # you can stringify funcs and types to get their name

        for meth as Integrator in [leftRect, midRect, rightRect, trapezium, simpson]
            var ret as Real = integrate(tc.fun, from=tc.from, to=tc.to, steps=tc.steps, meth=meth)
            write("   $meth: $ret")
        end for
    end for
end function
 = 
      p: (null) 
