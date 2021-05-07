local tk = require 'TokenKind'

local function streqi(str, str2)
   return string.lower(str) == string.lower(str2)
end

local function startswith(str, pre, start)
   return streqi(string.sub(str,start or 1,#pre), pre)
end

local function printf(fmt, ...) print(string.format(fmt, ...)) end



-- local c = require 'ansicolors'
useColorOutput = false

local function nnew(o, s)
   o = o or {}
   setmetatable(o, s)
   s.__index = s
   return o end

-- module OO
local subclass = function (parent, init)
   init = init or {}
   init.__index = parent
   init.new = function(self) return setmetatable(init, self) end
   return init end
local class = function (init) return subclass (nil, init) end
-- oo = { subclass = subclass, class = class }
-- end OO

Expr = class ({
   repr = function (self)
      print("Expr")
   end,
   emit = function (self)
      print(self.kind)
   end
})
ExprUnOp = subclass (Expr, {
   repr = function (self)
      print("ExprUnOp")
   end
})

-- ExprUnOp.__index=Expr
-- yaml(Expr)

ex = Expr:new()
exu = ExprUnOp:new()
ex:repr()
ex.kind = "wea"
exu.kind = "topk"
exu:repr()
ex:emit()
exu:emit()


function unreachable(msg)
   io.stderr:write(debug.traceback("unreachable hit: " .. msg).."\n") end

local Expr = {}

function Expr:new(kind, line, col, len, left, right)
   local ret = nnew({},self)
   ret.kind = kind
   ret.line = line or 1
   ret.col = col or 1
   ret.len = len or 1
   ret.left = left
   ret.right = right
   return ret end

function Expr:newFromToken(tok)
   return Expr:new(tok.kind, tok.line, tok.col,tok.len) end

function Expr:emit()
   printf("expr(%s@%d:%d)", self.kind, self.line, self.col) end

local ExprBinOp = Expr:new()

function ExprBinOp:new(kind, line, col, len, left, right)
   return nnew(Expr:new(kind, line, col, len, left, right), self) end

function ExprBinOp:newFromToken(tok)
   return nnew(Expr:newFromToken(tok), self) end

function ExprBinOp:emit()
   printf("binop(%s@%d:%d)", self.kind, self.line, self.col) end

function dump(t, indent, done)
   done = done or {}
   indent = indent or 0
   done[t] = true
   for key, value in pairs(t) do
      print(string.rep(" ", indent))
      if (type(value) == "table" and not done[value]) then
         done[value] = true
         print(key, ":")
         dump(value, indent + 2, done)
         done[value] = nil
      else
         print(key .. " = " .. tostring(value)) end end end

Token = class ({
   newWith = function (self, data)
      local tok = self:new()
      tok.kind = tk.unknown
      tok.line = 1
      tok.data = data
      tok.col = 1
      tok.pos = 1
      return tok end,
   matches = function (tok, str)
      return startswith(tok.data, str, tok.pos) end,
   trymatch = function (tok, str, kind)
      local ok = tok:matches(str)
      if ok then tok.str = str tok.kind = kind end
      return ok end,
   detect = function (tok)
      if tok:trymatch("func ", tk.func) then
      elseif tok:trymatch("end ", tk.end_) then
      elseif tok:trymatch("test ", tk.test) then
      elseif tok:trymatch("type ", tk.type) then
      elseif tok:trymatch("var ", tk.var) then
      elseif tok:trymatch("if ", tk.if_) then
      elseif tok:trymatch("else ", tk.else_) then
      elseif tok:trymatch("match ", tk.match) then
      elseif tok:trymatch("case ", tk.case) then
      elseif tok:trymatch("for ", tk.for_) then
      elseif tok:trymatch("+=", tk.plusEq) then
      elseif tok:trymatch("-=", tk.minusEq) then
      elseif tok:trymatch("*=", tk.timesEq) then
      elseif tok:trymatch("/=", tk.slashEq) then
      elseif tok:trymatch("==", tk.eQ) then
      elseif tok:trymatch(">=", tk.ge) then
      elseif tok:trymatch("<=", tk.le) then
      elseif tok:trymatch("^=", tk.powerEq) then
      elseif tok:trymatch("%=", tk.modEq) then
      else
      end end,
   advance = function (tok)
      tok.pos = tok.pos + tok.len return tok.pos <= #tok.data end,
   makeExpr = function (tok)
      if tok.kind==tk.match then return ExprMatch:newFromToken(tok)
      elseif tok.kind==tk.case then return ExprCase:newFromToken(tok)
      elseif tok.kind==tk.for_ then return ExprFor:newFromToken(tok)
      elseif tok.kind==tk.if_ then return ExprIf:newFromToken(tok)
      elseif tok.kind==tk.end_ then return ExprEnd:newFromToken(tok)
      end end
})

function collect(it)
   local ret = {} for l in it do ret[#ret+1] = l end return ret end

Parser = class ({
   fromFile = function (self, filename)
      local parser = self:new()
      parser.filename = filename or "-"
      if parser.filename == "-" then parser.file = io.stdin
      else parser.file = assert(io.open(filename, "r")) end
      parser.lines = collect(parser.file:lines())
      parser.data = table.concat(parser.lines, '\n')
      parser.token = Token:newWith(parser.data)
      return parser end,
   parse = function (parser)
      local ret = Module:new()
      print(parser.data) end,
   parseVar = function (parser) end
})

Module = class ({
   genH = function (mod)
      for k,ty in pairs(mod.types) do ty:genH(mod) end
      for k,fn in pairs(mod.funcs) do fn:genH(mod) end end ,
   genC = function (mod)
      for k,ty in pairs(mod.types) do ty:genC(mod) end
      for k,fn in pairs(mod.funcs) do fn:genC(mod) end  end,
   analyse = function (mod)
      for k,ty in pairs(mod.types) do ty:analyse(mod) end
      for k,fn in pairs(mod.funcs) do fn:analyse(mod) end end
})

local e = Expr:new(tk.plus)
e:emit()
printf("%s:%d:%d: %s: ", "filename.jet", 23, 3, e.kind)
unreachable("whoa")
-- dump(arg)

local par = Parser:fromFile(arg[1] or "-")
par:parse()
print (startswith("function yuiyt(ui JK)","funcTIon "))
