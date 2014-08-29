require 'ps'
local
  parser
, pp
, source
, filename
, ast

filename = "test1.lua" -- "/mnt/oih/hdon/src/git/luaqrcode/qrencode.lua"
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
  trace(string.format('visiting %s node @ %d', ast.tag, ast.pos))

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
    local numArgs = 0
    -- We need to create new locals for function arguments. We should find
    -- the names of arguments in ast[1]
    if (ast[1].tag == 'NameList') then
      numArgs = #ast[1]
      for iArgumentIdNode, argumentIdNode in ipairs(ast[1]) do
        -- Each of these is an Id node
        assert(argumentIdNode.tag == 'Id')
        -- Record new local
        locals[argumentIdNode[1]] = {
          indexFromBottom = iArgumentIdNode - 1 -- -1 because stackDepth starts at 0
        }
        trace(string.format('adding local "%s"', argumentIdNode[1]))
      end
    -- If ast[1] isn't our NameList, it should be a 'nil node', indicating
    -- that we have zero arguments.
    else assert(ast[1].tag == nil)
    end
    dumpLocals(locals)
    -- Emit function introduction, which fits the callee's arguments into
    -- our locals, and sets the groundwork for varargs.
    -- TODO full support for varargs!
    ps:emitFunctionIntroduction(numArgs, ast.pos)
    -- ast[2] should be our function body
    -- Descend into function body
    lua2ps(ast[2], locals)

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
      indexFromBottom = ps.stackDepth
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
        indexFromBottom = ps.stackDepth
      }
    end
    -- Evaluate rvalues and assign them to our locals!
    doAssignments(ast, locals)

  -- Number node
  elseif ast.tag == 'Number' then
    assert(#ast == 1)
    assert('number' == type(ast[1]))
    -- push number onto stack
    ps:emit(ast[1], string.format('%% pos=%d', ast.pos))

  -- Op node
  elseif ast.tag == 'Op' then
    assert(#ast == 3)
    lua2ps(ast[2], locals)
    lua2ps(ast[3], locals)
    if ast[1] == 'mul' then
      ps:emit('mul')
    elseif ast[1] == 'add' then
      ps:emit('add')
    else error("unknown lua operator")
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
    dumpLocals(locals)
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
  elseif ast.arg == 'Call' then
    -- Our first child is the callee.
    assert(#ast >= 1)
    -- If our callee is only an Id node, we are calling either a local or a global
    -- function. For now, this is the only kind of call we'll support.
    assert(ast[1].tag == 'Id')
    -- First we must push a PostScript 'mark.' This is part of the current calling
    -- convention I've come up with.
    ps:emit('mark')
    -- Second, we must push our arguments. To do that, we simply descend into them
    -- via lua2ps(). Currently, our calling convention (mostly implemented in the
    -- ps module) pushes arguments onto the stack in right-to-left order. XXX In
    -- contravention of Lua's behavior, right now to simplify my life, function
    -- arguments are also *evaluated* in right-to-left order!
    for iArgNode = #ast, 2, -1 do
      lua2ps(ast[iArgNode])
    end
    -- With our arguments on the stack, set up the function call
    if locals[calleeId] ~= nil then
      -- Call a local
      error('Calling a local currently unsupported! TODO')
    else
      -- Call a global
      ps:emitGlobalFunctionCall(calleeId)
    end

  else error(string.format('AST node tag "%s" is unimplemented!',
    tostring(ast.tag)))
  end
end

function dumpLocals(locals)
  for localName, localInfo in mpairs(locals) do
    print(string.format('local "%s"', localName))
    for k, v in pairs(localInfo) do
      print(string.format('  "%s" = "%s"', tostring(k), tostring(v)))
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
    error('not implemented')
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
      if locals[child[1]] ~= nil then
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
