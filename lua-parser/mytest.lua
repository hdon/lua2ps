require 'ps'
local
  parser
, pp
, source
, filename
, ast

--filename = 'test1.lua'
--filename = "/mnt/oih/hdon/src/git/luaqrcode/qrencode.lua"
filename = 'qrencode.lua'

parser = require "lua-parser.parser"
pp     = require "lua-parser.pp"

function readFile(file)
  local f = io.open(file, "rb")
  local content = f:read("*all")
  f:close()
  return content
end

function writeFile(file, contents)
  local f,err = io.open(file, "w")
  if not f then return print(err) end
  f:write(contents)
  f:close()
end

function dumpAST (t, i, f)
  if f == nil then print('*********8 DUMP TO STDOUT') f = io.stdout end
  if i == nil then i = 0 end
  f:write(string.format("{\n"))
  local tag = t.tag
  if tag == nil then tag = 'NIL!' end
  local pos = t.pos
  if pos == nil then pos = 'NIL!' end
  f:write(string.format("%s[tag] = %s\n", string.rep(" ", i+2), tag))
  f:write(string.format("%s[pos] = %s\n", string.rep(" ", i+2), pos))
  for k,v in ipairs(t) do
    f:write(string.format("%s[%s] = ", string.rep(" ", i+2), tostring(k)))
    if type(v) == "table" then
      dumpAST(v,i+2, f)
    else
      f:write(string.format("%s\n", tostring(v)))
    end
  end
  f:write(string.format("%s}\n", string.rep(" ", i)))
end

function dumpASTfile(file, ast)
  local f, err = io.open(file, 'w')
  if not f then return print(err) end
  dumpAST(ast, 0, io.stdout)
  f:close()
end

function dumpSourceAndASTfile(file, source, ast)
  local f, err = io.open(file, 'w')
  if not f then return print(err) end
  f:write(source)
  f:write('-----------------------\n')
  f:write('-----   AST    --------\n')
  f:write('-----------------------\n')
  dumpAST(ast, 0, f)
  f:close()
end

source = readFile(filename)
ast = parser.parse(source, filename)
print('writing Lua source and AST to _ast.lua')
dumpSourceAndASTfile('_ast.lua', source, ast)

function trace(...)
  print(...)
end

local ps = PS:new('out.ps')
function lua2ps(ast, locals)
  trace(string.format('visiting %s node @ %s', tostring(ast.tag), tostring(ast.pos)))

  -- Block node
  if ast.tag == 'Block' then
    -- For a 'Block' i think this is the right place to save our locals
    local newLocals = {}
    setmetatable(newLocals, { __index = locals })
    locals = newLocals
    -- The Block has one child for each statement
    -- Descend into those nodes
    for _, child in ipairs(ast) do
      lua2ps(child, locals)
    end

  -- Function node
  elseif ast.tag == 'Function' then
    local numArgs = 0 -- number of arguments for our function
    -- First we need to tell our emitter, which is responsible for monitoring
    -- our stack frame, that we're beginning a new function.
    ps:openFunction()
    -- We need to create new locals for function arguments. We should find
    -- the names of arguments in ast[1]
    if (ast[1].tag == 'NameList') then
      numArgs = #ast[1]
      for iArgumentIdNode, argumentIdNode in ipairs(ast[1]) do
        -- Each of these is an Id node
        assert(argumentIdNode.tag == 'Id')
        -- Record new local
        locals[argumentIdNode[1]] = {
          -- These should count downward from numArgs-1 to 0
          indexFromBottom = numArgs - iArgumentIdNode
        }
        trace(string.format('adding local "%s"', argumentIdNode[1]))
      end
    -- If ast[1] isn't our NameList, it should be a 'nil node', indicating
    -- that we have zero arguments.
    else assert(ast[1].tag == nil)
    end
    --dumpLocals(locals)
    -- Emit function introduction, which fits the callee's arguments into
    -- our locals, and sets the groundwork for varargs.
    -- TODO full support for varargs!
    ps:emitFunctionIntroduction(numArgs, ast.pos)
    -- ast[2] should be our function body
    -- Descend into function body
    lua2ps(ast[2], locals)
    -- Without further analysis, we don't know if control flow might reach the
    -- very end of our function, so we'll make sure to clean up the stack frame
    -- here and return from the function. Since we only support *exactly* one
    -- function return value, we'll return nil, or null in PostScript.
    lua2ps({tag='Return', {tag='Nil'}}, locals)
    -- Now we can tell our emitter that we're done defining this function.
    ps:closeFunction()

  -- Localrec node
  elseif ast.tag == 'Localrec' then
    -- I've only seen Localrec node appear for a statement like "local function foo(...) ... end"
    -- We should only have two children
    assert(#ast == 2)
    -- They should be tagged nil
    assert(ast[1].tag == nil)
    assert(ast[2].tag == nil)
    -- They each should have a single nil child node..
    assert(#ast[1] == 1)
    assert(#ast[2] == 1)
    -- First one tagged Id, second Function
    assert(ast[1][1].tag == 'Id')
    assert(ast[2][1].tag == 'Function')
    -- Register the local
    assert(type(ast[1][1][1]) == 'string')
    locals[ast[1][1][1]] = {
      indexFromBottom = ps.stackDepth[1]
    }
    -- Descend into the Function node
    lua2ps(ast[2][1], locals)
    -- TODO bind the result ?!

  -- Local node
  elseif ast.tag == 'Local' then
    -- This is not legal in Lua: local a = 'a', b = 'b'
    -- This is what you want:    local a, b = 'a', 'b'
    -- All of the lvalues precede all of the rvalues
    -- We should have two children, a NameList and an ExpList
    assert(#ast == 2)
    assert(ast[1].tag == 'NameList')
    assert(ast[2].tag == 'ExpList')
    -- Let's keep our lives simple for now and assume that
    -- our ExpList won't be longer than our NameList
    assert(#ast[1] >= #ast[2])
    -- Let's create new locals from the child nodes of our
    -- NameList node, which should all be tagged Id
    for _, idNode in ipairs(ast[1]) do
      assert(idNode.tag == 'Id')
      assert(#idNode == 1)
      assert('string' == type(idNode[1]))
      trace(string.format('adding local "%s"', idNode[1]))
      locals[idNode[1]] = {
        indexFromBottom = ps.stackDepth[1]
      }
    end
    -- Evaluate rvalues and assign them to our locals!
    doAssignments(ast, locals)

  -- Number node
  elseif ast.tag == 'Number' then
    assert(#ast == 1)
    assert('number' == type(ast[1]))
    -- push number onto stack
    ps:emit(ast[1], string.format('%% pos=%s', tostring(ast.pos)))

  -- Nil node
  elseif ast.tag == 'Nil' then
    ps:emit('null', string.format('%% pos=%s', tostring(ast.pos)))

  -- Op node
  elseif ast.tag == 'Op' then
    assert(#ast > 0)
    if #ast == 3 then
      -- Binary operators
      lua2ps(ast[2], locals)
      lua2ps(ast[3], locals)
      if ast[1] == 'mul' then
        ps:emit('mul')
      elseif ast[1] == 'add' then
        ps:emit('add')
      else error("unknown lua operator") end
    elseif #ast == 2 then
      -- Unary operators
      if ast[1] == 'len' then
        -- emit operand
        lua2ps(ast[2], locals)
        -- emit PostScript for #tbl operation
        ps:emit('luaTableLength')
      else error('unknown unary operator: ' .. ast[1]) end
    else error(string.format(
    'Known operators are binary or unary, but found "%s" operator with %d operands!', ast[1], #ast))
    end

  -- Id node
  elseif ast.tag == 'Id' then
    -- This is a Lua identifier. It might evaluate to a local,
    -- or to a global. We need to figure out which it is, and
    -- emit the corresponding code. Id nodes are not always
    -- handled here. We should only handle them here when they
    -- are ordinary rvalue expressions.
    assert(#ast == 1)
    assert('string' == type(ast[1]))
    trace(string.format('encountering identifier "%s"', ast[1]))
    --dumpLocals(locals)
    -- Does identifier identify a local?
    if locals[ast[1]] ~= nil then
      -- Identifies local
      ps:emitLocalRvalue(locals[ast[1]].indexFromBottom, ast[1], ast.pos)
    else
      -- Identifies global
      ps:emit(ps:makeGlobalIdentifier(ast[1]))
    end

  -- TODO note that this assumes single return value from each rvalue expression,
  --      and that the number of lvalues and rvalues are the same!
  -- Set node
  elseif ast.tag == 'Set' then
    doAssignments(ast, locals)

  -- Return node
  elseif ast.tag == 'Return' then
    -- Right now we only support single return values
    assert(#ast == 1)
    -- Descend into our return value
    lua2ps(ast[1], locals)
    -- Emit function return code TODO support other numbers of return values
    ps:emitFunctionReturn(1, ast.pos)

  -- Call node
  elseif ast.tag == 'Call' then
    -- Our first child is the callee.
    assert(#ast >= 1)
    -- If our callee is only an Id node, we are calling either a local or a global
    -- function. For now, this is the only kind of call we'll support.
    assert(ast[1].tag == 'Id')
    local calleeId = ast[1][1]
    -- First we must push a PostScript 'mark.' This is part of the current calling
    -- convention I've come up with.
    -- XXX here i bypass ps:emit()
    ps:write(string.format('mark %% setting up stack frame for call to %s pos=%s\n',
      calleeId, tostring(ast.pos)))
    -- Second, we must push our arguments. To do that, we simply descend into them
    -- via lua2ps(). Currently, our calling convention (mostly implemented in the
    -- ps module) pushes arguments onto the stack in right-to-left order. XXX In
    -- contravention of Lua's behavior, right now to simplify my life, function
    -- arguments are also *evaluated* in right-to-left order!
    for iArgNode = #ast, 2, -1 do
      lua2ps(ast[iArgNode], locals)
    end
    -- With our arguments on the stack, set up the function call
    if locals[calleeId] ~= nil then
      -- Call a local
      error('Calling a local currently unsupported! TODO')
    else
      -- Call a global
      ps:emitGlobalFunctionCall(calleeId)
    end

  -- Table node
  elseif ast.tag == 'Table' then
    -- TODO change default psDictSize?
    local psDictSize = 128
    if psDictSize < #ast then psDictSize = #ast end
    -- Emit PostScript to instantiate LuaTable
    ps:emit(psDictSize, 'luaTableNew')
    -- Iterate over each entry in the table. I'm not 100% sure of how Lua table
    -- literal syntax, but I think I should keep a cursor for the current insert
    -- index, but only use it when a key isn't explicitly specified.
    local cursor = 1
    for iChild, child in ipairs(ast) do
      -- dup dict on PostScript stack
      ps:emit('dup', '% dup lua table dict')
      -- If we have a Pair node, that means we have an explicit key
      if child.tag == 'Pair' then
        -- evaluate key, put on PostScript stack
        lua2ps(child[1])
        -- evaluate value, put on PostScript stack
        lua2ps(child[2])
      else
        -- put auto table key on PostScript stack
        ps:emit(cursor, '% auto table key')
        cursor = cursor + 1
        -- evaluate value, put on PostScript stack
        lua2ps(child)
      end
      -- Emit table assignment
      ps:emit('luaTableSet')
    end

  -- String node
  elseif ast.tag == 'String' then
    assert(#ast == 1)
    ps:emitString(ast[1])

  -- Index node
  elseif ast.tag == 'Index' then
    -- Index node has two children..
    assert(#ast == 2)
    -- ..the expression which evaluates to the table to be indexed, and the key.
    -- First we evaluate our table expression.
    lua2ps(ast[1], locals)
    -- Second we evaluate our key expression.
    lua2ps(ast[2], locals)
    -- Finally, we emit the code to evaluate the Index operation.
    ps:emit('luaTableGet')

  else error(string.format('AST node tag "%s" is unimplemented!',
    tostring(ast.tag)))
  end
end

function dumpLocals(locals)
  for localName, localInfo in mpairs(locals) do
    print(string.format('dump> local "%s"', localName))
    for k, v in pairs(localInfo) do
      print(string.format('dump>  "%s" = "%s"', tostring(k), tostring(v)))
    end
  end
end

-- TODO we might emit more efficient code if we didn't force locals to have
-- a particular place on the stack all the time, though sometimes it would
-- probably be the most efficient way to do it. I'm doing it this way for
-- now, partly because it's simpler, and partly because the other
-- optimizations we might reap might be best done in an optimizer that only
-- deals with PostScript, and doesn't care about the Lua. TODO Research!
function doAssignments(ast, locals)
  -- We should get two child nodes. The first is either a VarList or it is
  -- a NameList, depending on whether this is a Set node or a Local node.
  -- These provide our lvalues. Our rvalues are in the second child, which
  -- should always be an ExpList.
  -- Update: An expression like "function foo() .. end" results in a Set
  -- with two nil nodes as children.
  assert(#ast == 2)
  assert(ast.tag == 'Local' and ast[1].tag == 'NameList'  and ast[2].tag == 'ExpList'
      or ast.tag == 'Set'   and ast[1].tag == 'VarList'   and ast[2].tag == 'ExpList'
      or ast.tag == 'Set'   and ast[1].tag == nil and ast[2].tag == nil
                            and #ast[1] == 1 and #ast[2] == 1
                            and ast[1][1].tag == 'Id' and ast[2][1].tag == 'Function')
  if ast[1].tag == nil then
    -- This is the "function foo() .. end" case.
    assert(ast[2][1].tag == 'Function')
    lua2ps(ast[2][1], locals)
    -- Emit PostScript to make the assignment
    ps:emitGlobalAssignment(ast[1][1][1])
  else
    -- To keep things simple, we'll impose some additional
    -- restrictions on ourselves. Better code in the future
    -- can remove these restrictions.
    assert(#ast[1] == #ast[2])
    -- Lua evaluates all rvalues first, so we'll do the same.
    -- This will push the results of each rvalue to the stack
    -- left-to-right, so the right-most rvalue is on the top
    -- of the stack.
    for _, child in ipairs(ast[2]) do
      lua2ps(child, locals)
    end
    -- Now we will assign these values to our variables. We
    -- must do this in right-to-left order because we evaluated
    -- our rvalues left-to-right.
    for iChild = #ast[1], 1, -1 do
      local child = ast[1][iChild]
      if child.tag == 'Index' then
        -- This is a table index assignment. The lvalue can be a pretty complicated
        -- expression, such as foo[23][42] = value, and more exotic forms. The Index
        -- node contains two children, the first is the expression which evaluates
        -- to the table in which the assignment occurs ("table expression,") and the
        -- second is the -- expression that evaluates to the key for our index
        -- assignment operation.
        -- First let's evaluate our table expression.
        lua2ps(child[1], locals)
        -- The table we want is now on top of the stack. Let's evaluate our key
        -- expression now.
        lua2ps(child[2], locals)
        -- Rearrange the stack a bit so we can call the PostScript prologue procedure
        -- "luaTableSet"
        ps:emit(3, -1, 'roll', 'luaTableSet',
          string.format('%% lua table index assignment pos=%s', tostring(child.pos)))
      elseif locals[child[1]] ~= nil then
        assert(locals[child[1]] ~= nil)
        -- Emit local assignment
        ps:emitLocalAssignment(locals[child[1]].indexFromBottom, child[1], child.pos)
      else
        -- Emit global assignment
        ps:emitGlobalAssignment(child[1])
      end
    end
  end
end

print('--- ast2ps ---')
lua2ps(ast, {})
ps:close()
