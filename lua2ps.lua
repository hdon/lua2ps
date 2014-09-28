require 'ps'
local
  parser
, pp
, source
, sourceFilename
, dest
, ast
, argv

argv = {...}

if #argv < 0 or #argv > 2 then
  error('lua2ps requires 1 or 2 arguments')
end

sourceFilename = argv[1]
if #argv == 2 then
  dest = argv[2]
else
  dest = 'out.ps'
end

indent = -2
local ps = PS:new(dest)
-- This function recursively visits an entire Lua AST, emitting PostScript.
-- Arguments:
--   ast:         A syntax node
--   locals:      The locals table
function lua2ps(ast, locals, numReturnValues)
  indent = indent + 2
  trace(string.format('visiting %s node @ %s', tostring(ast.tag), tostring(ast.pos)))
  --dumpLocals(locals)

  -- Block node
  if ast.tag == 'Block' then
    -- The Block has one child for each statement
    -- Descend into those nodes
    for _, child in ipairs(ast) do
      lua2ps(child, locals, 0)
    end

  -- Function node
  elseif ast.tag == 'Function' then
    assert(#ast == 2)
    local numArgs = 0 -- number of arguments for our function

    -- We need to create new locals for function arguments. We should find
    -- the names of arguments in ast[1]. We don't put these into our own
    -- 'locals', because that is within our parent scope, our scope is only
    -- relevant once we descend into our Block node. 'newLocals' is nil
    -- though, and this is what it's for, I guess.
    local newLocals = {}
    if (ast[1].tag == 'NameList') then
      numArgs = #ast[1]
      for iArgumentIdNode, argumentIdNode in ipairs(ast[1]) do
        -- Each of these is an Id node
        assert(argumentIdNode.tag == 'Id')
        -- Record new local
        newLocals[argumentIdNode[1]] = {
          -- These should count downward from numArgs-1 to 0
          indexFromBottom = numArgs - iArgumentIdNode
        }
        trace(string.format('adding local "%s"', argumentIdNode[1]))
      end
    -- If ast[1] isn't our NameList, it should be a 'nil node', indicating
    -- that we have zero arguments.
    else
      assert(ast[1].tag == nil)
    end

    ps:doFunction(numArgs, ast.pos, function()
      -- Descend into function body
      block2ps(ast[2], ast, locals, newLocals)
      -- Without further analysis, we don't know if control flow might reach the
      -- very end of our function, so we'll make sure to clean up the stack frame
      -- here and return from the function. Since we only support *exactly* one
      -- function return value, we'll return nil, or null in PostScript.
      -- XXX don't need this anymore? lua2ps({tag='Return'}, locals)
      ps:write('cleartomark mark exit % end-of-function, return nothing!\n')
    end)

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
      indexFromBottom = ps:getStackDepth()
    }
    -- Descend into the Function node
    lua2ps(ast[2][1], locals)
    -- TODO bind the result ?!

  -- Local node
  elseif ast.tag == 'Local' then
    -- In Lua assignments, you have a list of lvalues, and optionally a list of rvalues.
    -- The assignments are made one-to-one, with the following caveats:
    -- 1) A Call that occurs at the end of the list contributes all of its return values
    --    to the assignment.
    -- 2) Lvalues that exceed the number of available rvalues are set to nil.
    -- 3) Rvalues that exceed the number of available lvalues are discarded.
    -- Our calling convention requires that function arguments push the first argument
    -- onto the stack last, and since these "varargs list" semantics are the same for
    -- assignments as well as the argument passing, 

    -- We should have one or two children, a NameList and optionally an ExpList.
    assert((#ast == 1 or (#ast == 2 and ast[2].tag == 'ExpList')) and ast[1].tag == 'NameList')

    -- Let's allocate stack and emit an initial value of nil for each of our new locals.
    -- We'll allocate stack for our locals in last-to-first order as described above.
    for i = #ast[1], 1, -1 do
      local idNode = ast[1][i]
      assert(idNode.tag == 'Id')
      assert(#idNode == 1)
      assert('string' == type(idNode[1]))
      trace(string.format('adding local "%s"', idNode[1]))
      locals[idNode[1]] = {
        indexFromBottom = ps:getStackDepth()
      }
      ps:emit('null', string.format('%% assign nil by default to local "%s"', idNode[1]))
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

  -- True node
  elseif ast.tag == 'True' then
    ps:emit('true', string.format('%% pos=%s', tostring(ast.pos)))

  -- False node
  elseif ast.tag == 'False' then
    ps:emit('false', string.format('%% pos=%s', tostring(ast.pos)))

  -- Paren node
  elseif ast.tag == 'Paren' then
    assert(#ast == 1)
    -- Putting a call inside parens enforces the "comma rule," so request 1 return value,
    -- which only applies to Call nodes, and has no effect on other nodes.
    lua2ps(ast[1], locals, 1)

  -- Op node
  elseif ast.tag == 'Op' then
    assert(#ast > 0)
    if #ast == 3 then -- We have a binary operator
      -- emit operator
      if ast[1] == 'mul' then
        -- Emit operands
        lua2ps(ast[2], locals)
        lua2ps(ast[3], locals)
        ps:emit('mul')
      elseif ast[1] == 'add' then
        -- Emit operands
        lua2ps(ast[2], locals)
        lua2ps(ast[3], locals)
        ps:emit('add')
      elseif ast[1] == 'eq' then
        -- Emit operands
        lua2ps(ast[2], locals)
        lua2ps(ast[3], locals)
        ps:emit('eq')
      elseif ast[1] == 'concat' then
        -- Emit operands, and coerce them to strings
        lua2ps(ast[2], locals)
        ps:emit('luaToString')
        lua2ps(ast[3], locals)
        ps:emit('luaToString')
        -- Emit concatenation
        ps:emit('luaStrConcat')
      -- 'lt' seems to be both < and >, but with operands switched for >, i guess
      elseif ast[1] == 'lt' then
        -- Emit operands
        lua2ps(ast[2], locals)
        lua2ps(ast[3], locals)
        ps:emit('lt')
      elseif ast[1] == 'sub' then
        -- Emit operands
        lua2ps(ast[2], locals)
        lua2ps(ast[3], locals)
        ps:emit('sub')
      elseif ast[1] == 'div' then
        -- Emit operands
        lua2ps(ast[2], locals)
        lua2ps(ast[3], locals)
        ps:emit('div')
      elseif ast[1] == 'or' then
        -- Evaluate first operand, dup it, and coerce the dup to bool, and then invert it
        lua2ps(ast[2], locals)
        ps:emit('dup', 'luaToBool', 'not')
        -- Then emit an {}if construct
        ps:emitProcs(1, 'if', 0, function()
          -- Our first operand evaluated falsy, so let's pop it and evaluate our second operand
          ps:emit('pop', '% popping first "or" operand because it\'s falsy')
          lua2ps(ast[3], locals)
        end)
      elseif ast[1] == 'and' then
        -- Evaluate first operand, dup it, and coerce the dup to bool
        lua2ps(ast[2], locals)
        ps:emit('dup', 'luaToBool')
        -- Then emit an {}if construct
        ps:emitProcs(1, 'if', 0, function()
          -- Our first operand evaluated falsy, so let's pop it and evaluate our second operand
          ps:emit('pop', '% popping first "and" operand because it\'s truthy')
          lua2ps(ast[3], locals)
        end)
      else error(string.format('unknown binary lua operator: "%s"', ast[1])) end

    elseif #ast == 2 then -- We have a unary operator
      -- emit operand
      lua2ps(ast[2], locals)
      -- emit operator
      if ast[1] == 'len' then
        -- emit PostScript for #tbl operation
        ps:emit('luaTableLength')
      elseif ast[1] == 'not' then
        -- emit boolean coercion operator from prologue.ps, then PostScript not operator
        ps:emit('luaToBool', 'not')
      else error('unknown unary lua operator: ' .. ast[1]) end
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
      --ps:emitLocalRvalue(locals[ast[1]].indexFromBottom, ast[1], ast.pos)
      ps:emitLocalRvalue(locals, ast[1], ast.pos)
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
    -- Evaluate our list of return value expressions in last-to-first order.
    -- XXX This is probably not what Lua does.
    ps:emit('mark', '% constructing return frame')
    for i = #ast, 1, -1 do
      -- In Lua, individual return value expressions, elements in the list of
      -- expressions that are the parameters to the return statement, are all
      -- coerced to exactly one value, except for the very last one, whose size
      -- is unbound.
      local requestNumReturnValues
      if i == #ast then
        requestNumReturnValues = math.huge
      else
        requestNumReturnValues = 1
      end
      lua2ps(ast[i], locals, requestNumReturnValues)
    end
    -- Emit function return code
    trace(':D :D :D return #ast =', #ast)
    -- Bypass PS:emit() stack bookkeeping for return value list
    ps:write(
      'counttomark dup 2 add -1 roll pop counttomark 1 sub dup 2 index  % return code\n'
    ..'sub -1 1 {pop pop} 5 index 6 add 4 roll 4 add exch roll for exit % return code\n'
    )

  -- Call node
  elseif ast.tag == 'Call' then
    -- Our first child is the callee.
    assert(#ast >= 1)
    -- With unbound varargs, the static analysis done by the stack emitter becomes useless, because
    -- it only works with integers, and has no primitives for arithmetic with variables. If it did,
    -- we could nudge the stack by "x" representing the unbound return value list of the call, and
    -- then reverse the nudge by "x" later. However, absent any "x," we must override the emitter's
    -- stack bookkeeping. Perhaps then it is foolish not to use this technique all the time, but it
    -- is there to catch mistakes early when possible, so we'll use it if we can.
    local overrideStackDepth
    -- XXX Please see XXX below for explanation
    -- XXX if numReturnValues == math.huge then
      overrideStackDepth = ps:getStackDepth()
    -- XXX end

    -- Give some good name to our callee, for diagnostic purposes
    local calleeDescription
    if ast[1].tag == 'Id' then
      calleeDescription = ast[1][1]
    else
      calleeDescription = string.format('*expression*')
    end
    -- Emit a PostScript 'mark,' as per our calling convention
    ps:emit('mark', string.format('%% setting up call for "%s()"', calleeDescription))
    -- Evaluate our arguments in right-to-left order
    for i = #ast, 2, -1 do
      local requestNumReturnValues
      if i == #ast then
        requestNumReturnValues = math.huge
      else
        requestNumReturnValues = 1
      end
      lua2ps(ast[i], locals, requestNumReturnValues)
    end
    -- Evaluate our callee expression
    lua2ps(ast[1], locals)
    -- Emit 'exec' PostScript operator to invoke our function
    ps:emit('exec', string.format('%% make call to "%s()"', calleeDescription))
    -- Our call site is responsible for determining what is done with return values,
    -- and will inform us via numReturnValues.
    if numReturnValues == nil then
      error('Parent node must request the wanted number of return values')
    elseif numReturnValues == math.huge then
      -- Retain all return values. Let's just roll the return values under the mark
      -- delineating the return values, and pop it.
      ps:write('counttomark 1 add -1 roll pop % retain all return values\n')
    elseif numReturnValues == 0 then
      -- Discard all return values -- really simple
      ps:write('cleartomark % discard all return values\n')
    elseif numReturnValues > 0 and numReturnValues < math.huge then
      -- Keep a specific number of return values. Discard or push nils (nulls) until
      -- we have the right amount.
      ps:write(string.format(
        'counttomark %d gt                             %% compare num retvals w/ num wanted (%d)\n'
      ..'{ counttomark 1 add %d roll cleartomark }     %% discard some return values\n'
      ..'{ counttomark 1 %d { null exch 1 add 1 roll } %% add extra...                     \n'
      ..'  3 index 5 add -1 roll pop for } ifelse      %%          ... return values!\n'
      , numReturnValues , numReturnValues , numReturnValues , numReturnValues
      ))
    else
      error('internal: function call sites must request number of return values')
    end
    -- XXX XXX XXX
    -- I am cheating here.
    -- I liked that PS could keep track of my stack for me.
    -- But Lua's "varargs" features make this info impossible to know AOT without some more
    -- serious static analysis, and even then it may not be possible to know AOT without just
    -- running the program. As a result, I am going to cheat here. Oh well.
    --
    -- We need to pick up the slack for the emitter here, I guess. Let's account for:
    -- popping the arguments to our call, the PostScript proc containing our function,
    -- and the PostScript mark delineating the call frame, and pushing the return values
    -- from the function. The mark delineating the return frame was pushed by the callee
    -- and has already been popped. BUT in the case of unbound return value list, we
    -- use the override mechanism to cheat. See declaration of 'overrideStackDepth' above.
  --if overrideStackDepth == nil then
  --  ps:nudgeStackDepth(
  --    numReturnValues     -- Number of return values XXX This doesn't work for "retain all return values"
  --  - #ast                -- Number of arguments, plus the function's proc
  --  - 1                   -- The mark delineating the call frame
  --  + 1                   -- The 'exec' already popped the callee C:
  --)
  --else
      ps:overrideStackDepth(overrideStackDepth)
  --end

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
        lua2ps(child[1], locals)
        -- evaluate value, put on PostScript stack
        lua2ps(child[2], locals)
      else
        -- put auto table key on PostScript stack
        ps:emit(cursor, '% auto table key')
        cursor = cursor + 1
        -- evaluate value, put on PostScript stack
        lua2ps(child, locals)
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

  -- While node
--elseif ast.tag == 'While' then
--  assert(#ast == 2 and ast[2].tag == 'Block')
--  -- We'll just create a fake Block node and use ps:doBlockScope
--  -- TODO document why this works i guess
--  ps:doBlockScope(function()
--    local fakeNode = {tag='Block'}
--    fakeNode[1] = ast[1]
--    for i = 1, #ast[2] do
--      fakeNode[i+1] = ast[2][i]
--    end
--    -- evaluate while loop condition
--    lua2ps(ast[1], locals)
--    block2ps({tag='Block'
--  end)

  -- Fornum node
  -- NOTE: The order in which the for loop parameter's expressions are evaluated
  -- probably doesn't match Lua's.
  elseif ast.tag == 'Fornum' then
    -- I think this syntax requires two or three numeric expressions: the initial
    -- value, the terminal value, and optionally the increment. PostScript "for"
    -- loop requires us to specify all three, and then the proc that runs in the
    -- loop. The iterator variable is local to the scope of the loop, which is
    -- handled by our Block child node.
    assert((#ast == 4 or #ast == 5) and ast[1].tag == 'Id' and ast[#ast].tag == 'Block')
    -- Evaluate expression obtaining initial iteration value
    lua2ps(ast[2], locals)
    -- Evaluate expression obtaining iteration interval
    if #ast == 5 then
      lua2ps(ast[4], locals)
    else
      ps:emit(1, '% default Fornum interval')
    end
    -- Evaluate expression obtaining terminal iteration value
    lua2ps(ast[3], locals)
    -- Descend into Block node child, providing iterator variable as a new local.
    -- We wrap the call to block2ps() in ps:doBlockScope() to help construct the
    -- Block on the PostScript side.
    ps:doBlockScope(function()
      -- We have two new locals. ast[1][1] should be a string containing our
      -- iterator variable name. We also have a fake local, "*loopBase," also with
      -- indexFromBottom=0, which can be used to unwind the stack when a Break
      -- statement is encountered. block2ps() will install the latter for us
      -- if we supply its name.
      block2ps(ast[#ast], ast, locals, { [ast[1][1]] = { indexFromBottom = 0 }}, '*loopBase')
    end, 1, 'for')
    ps:emit('% Fornum loop pos=' .. tostring(ast.pos))

  -- Break node
  elseif ast.tag == 'Break' then
    -- At the beginning of a loop, we store a fake local, '*loopBase,' which fancies
    -- itself the premier stack allocation of the loop. Using this, we can calculate
    -- the depth of the stack between its present state in execution of the loop, and
    -- the beginning/end of the loop, allowing us to emit code to correctly clean up
    -- our locals.
    for i = 0, ps:calculateStackIndexOfLocal(locals, '*loopBase') do
      -- Bypass the emitter. The emitter doesn't know where our loop begins, or what
      -- exiting from the loop would be like, and it doesn't have the machinery that
      -- we do to keep track of something like '*loopBase.'
      ps:write('pop % cleaning up locals for "break"\n')
    end
    ps:emit('exit', '% "break"')

  -- If node
  elseif ast.tag == 'If' then
    -- We need a counter to tell us which part of the If node we're at. We could
    -- do this the traditional functional way, but doBlockScope() counts on us
    -- taking advantage of closures.
    local counter = 1

    local function emitIfStuff()
      trace(string.format('emitIfStuff(%d)', counter))
      if counter % 2 == 1 and counter < #ast then
        -- Evaluate condition expression. We use block2ps because it synchronizes
        -- our Locals stack with the PS:stackDepth stack, not because Lua actually
        -- needs a block scope here. Actually by doing it this way, we end up
        -- nesting several empty levels of scope in our locals stack which don't
        -- even begin to resemble what Lua does, but because they're empty, it
        -- works. XXX nvm i'm not relying on block2ps
        --lua2ps(ast[counter], locals)
        --block2ps(ast[counter], ast, locals)
        -- XXX I need to reevaluate everything I've written in the comments above.
        -- PS:doBlockScope() will expect us to derive a new local scope, but we also
        -- call emitIfStuff() ourseles without bothering with another block scope.
        -- The latter will not pass us any arguments, but when we call ourselves we
        -- will, so that we can 
        local fakeLocals
        if counter == 1 then
          fakeLocals = locals
        else
          fakeLocals = setmetatable({}, { __index = locals } )
        end
        lua2ps(ast[counter], fakeLocals)
        local rem = #ast - counter
        counter = counter + 1
        --trace(string.format('**IF** counter=%d #ast=%d rem=%d', counter, #ast, rem))
        if rem == 1 then
          ps:doBlockScope(emitIfStuff, 1, 'if')
        else
          ps:doBlockScope(emitIfStuff, 2, 'ifelse')
        end
      else
        --local rem = #ast - counter
        --trace(string.format('**IF** counter=%d #ast=%d rem=%d', counter, #ast, rem))
        local block = ast[counter]
        counter = counter + 1
        block2ps(block, ast, locals)
      end
    end

    emitIfStuff()

  else error(string.format('AST node tag "%s" is unimplemented!',
    tostring(ast.tag)))
  end

  indent = indent - 2
end

function dumpLocals(locals)
  local some = false
  for localName, localInfo in mpairs(locals) do
    some = true
    trace(string.format('locals> local "%s"', localName))
    for k, v in pairs(localInfo) do
      trace(string.format('locals>  "%s" = "%s"', tostring(k), tostring(v)))
    end
  end
  if some == false then
    trace('locals> NO LOCALS')
  end
end

function dumpLocals2(locals)
  local n = 1
  while 1 do
    local none = true
    for k, v in pairs(locals) do
      trace(string.format('locals[%d]> "%s"', n, k))
      none = false
    end
    if none then
      trace(string.format('locals[%d]> no locals in this scope', n))
    end
    locals = getmetatable(locals)
    if locals == nil then break end
    locals = locals.__index
    n = n + 1
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
  -- But fir
  if ast[1].tag == nil then
    -- This is the "function foo() .. end" case. Assign the Function to global var 'foo'
    assert(ast[2][1].tag == 'Function')
    lua2ps(ast[2][1], locals)
    -- Emit PostScript to make the assignment
    ps:emitGlobalAssignment(ast[1][1][1])
  else
    -- First we'll evaluate all of our rvalues, imposing the "comma rule," in
    -- right-to-left order. We'll push nil (null) or roll the rightmost values
    -- to the top of the stack and pop them off, to balance out #ast[1] with
    -- #ast[2].
    for i = #ast[2], 1, -1 do
      -- If the rightmost rvalue expression is a call, then the function call will give
      -- us *exactly* the number of return values we want, popping or padding with nil
      -- as necessary. But, if it is not a call, then *we* are responsible for padding
      -- with nil (null.)
      if i == #ast[2] then
        if ast[2][i].tag == 'Call' then
          lua2ps(ast[2][i], locals, math.max(0, #ast[1] - #ast[2]))
        else
          -- We're responsible for padding out our rvalue list with nil.
          for j = 1, #ast[1] - #ast[2] do
            ps:emit('null', '% padding rvalue list for assignment statement')
          end
          -- Now evaluate the non-Call rvalue expression
          lua2ps(ast[2][i], locals)
        end
      else
        -- It's harmless to request numReturnValues if it's not a call, so this is simple
        -- enough.
        lua2ps(ast[2][i], locals, 1)
      end
    end

    -- Now that we have our rvalue list, let's get these values into our lvalues. As we
    -- pushed our rvalues in right-to-left order, we'll be assigning in left-to-right
    -- order.
    for i = 1, #ast[2] do
      local child = ast[1][i]
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
        ps:emitLocalAssignment(child[1], locals, child.pos)
      else
        -- Emit global assignment
        ps:emitGlobalAssignment(child[1])
      end
    end
  end
end

-- A simple helper for lua2ps
function block2ps(ast, parent, locals, newLocals, stackBookmark)
  assert(ast.tag == 'Block')
  -- When we visit a Block node, we need a place to to store our locals,
  -- and via the '__table' metamethod, it also provides the facility
  -- which allows us to handle nested local variable scopes, and variable
  -- shadowing. If it's non-nil, that means we have locals which are not
  -- declared by child nodes of our Block node, which means we have to
  -- deal with them here, instead of in, say, a Local node.
  if newLocals == nil then
    newLocals = {}
  end
  -- Function Blocks handle their own stacks.
  if parent.tag ~= 'Function' then
    -- Let's emit the "phony" PostScript operator, which lies to our
    -- PostScript emitter about stack size, allowing our parent node
    -- to take the responsibility for putting this local on the stack
    -- for us.
    for localName, _ in pairs(newLocals) do
      ps:emit('phony', '% for ' .. localName)
    end
  end
  -- If we have a stackBookmark, we install it without adjusting the stack,
  -- as a local with indexFromBottom = 0. It can be used for finding a
  -- specific spot in the stack.
  if stackBookmark ~= nil then
    newLocals[stackBookmark] = { indexFromBottom = 0 }
  end
  -- Set up the __index metamethod for the new locals table so that our
  -- enclosing scopes are accessible, and respecting of variable shadowing,
  -- via the locals table we will pass to recursive visits to this node's
  -- children.
  setmetatable(newLocals, { __index = locals })
  -- This function has done a lot to help, so now we send it back to lua2ps()
  -- for the simplest portion of its processing.
  trace(string.format('block2ps() -> local scope count  up  to %d', 1234))
  lua2ps(ast, newLocals)
  trace(string.format('block2ps() -> local scope count down to %d', 4321))

  -- Functions clean up their own stacks
  if parent.tag ~= 'Function' then
    trace('Cleaning up locals')
    -- At the end of the block, we must pop our locals off the stack.
    -- This loop only visits locals which belong specifically to our
    -- locals, not locals we inherit from enclosing scopes!
    for localName, localProps in pairs(newLocals) do
      -- Don't pop our bookmark!
      if localName ~= stackBookmark then
        ps:emit('pop', string.format('%% cleaning up locals (maybe "%s", index from bottom of stack = %s)',
          localName, tostring(localProps.indexFromBottom)))
      end
      localProps.stackDepth = -1 -- invalidate local
    end
  end
end

parser = require "lua-parser.parser"
pp     = require "lua-parser.pp"

source = readFile(sourceFilename)
ast = parser.parse(source, sourceFilename)
print('writing Lua source and AST to _ast.lua')
dumpSourceAndASTfile('_ast.lua', source, ast)

print('--- ast2ps ---')
lua2ps(ast, {})
ps:close()
