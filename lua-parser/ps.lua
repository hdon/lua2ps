require 'util'

PS = {
  out = io.stdout
}
PS.__index = PS

function PS:new(out)
  local ps = setmetatable({
    stackDepth = {0}
  , psCommands  = {
    -- Standard PostScript commands -
      index         = { pop = 1 , push = 1 }
    , mul           = { pop = 2 , push = 1 }
    , add           = { pop = 2 , push = 1 }
    , null          = { pop = 0 , push = 1 }
    , put           = { pop = 3 , push = 0 }
    , dup           = { pop = 1 , push = 2 }
    , pop           = { pop = 1 , push = 0 }
    , def           = { pop = 2 , push = 0 }
    , mark          = { pop = 0 , push = 1 }
    , roll          = { pop = 2 , push = 0 } -- TODO stack check of roll operand?
    , exch          = { pop = 2 , push = 2 }
    , ['for']       = { pop = 4 , push = 0 }

    -- Standard lua2ps commands, from prologue.ps --
    -- 'phony' is a pseudo-instruction to push a value onto the stack. these are useful when PostScript
    -- is expected to automatically put something on the stack for us at the beginning of a proc.
    , phony         = { pop = 0 , push = 1 }
    -- 'kram' is a pseudo-instruction to clear a mark off the stack.
    -- See emitGlobalFunctionCall() for more info.
    , kram          = { pop = 1 , push = 0 }
    -- these are for dealing with Lua tables
    , luaTableNew   = { pop = 1 , push = 1 }
    , luaTableGet   = { pop = 2 , push = 1 }
    , luaTableSet   = { pop = 3 , push = 0 }
    , luaTableLength= { pop = 1 , push = 1 }
    }
  }, PS)
  if out ~= nil then
    if 'string' == type(out) then
      local f, err
      f, err = io.open(out, 'w')
      if not f then return print(err) end
      ps.out = f
    elseif 'table' == type(out) then
      ps.out = out
    end
  end
  -- Write prologue
  --ps.out:write(readFile('prologue.ps'))
  ps.out:write('(prologue.ps) run\n')
  return ps
end

function PS:write(...)
  trace('ps>', ...)
  self.out:write(...)
end

function PS:makeGlobalIdentifier(id)
  return '__lua_G_' .. id
end
function PS:isGlobalIdentifier(id)
  return string.starts(id, '__lua_G_')
end
function PS:getStackDepth()
  return self.stackDepth[#self.stackDepth]
end
function PS:nudgeStackDepth(n)
  local x = self:getStackDepth()
  self.stackDepth[#self.stackDepth] = n + self.stackDepth[#self.stackDepth]
  trace(string.format('stack depth change %d -> %d', x, self:getStackDepth()))
end
function PS:doStackFrame(fn)
  self.stackDepth[#self.stackDepth+1] = 0
  fn(#self.stackDepth)
  self.stackDepth[#self.stackDepth] = nil
end

-- Using a 'locals' table, which uses the __index metamethod to stack local variables
-- with respect to variable shadowing and compartmentalized by frames and pseudo-
-- frames in the same way that our own PS:stackDepth is, we can calculate the exact
-- position from the top of the stack that contains the value represented by a named
-- local variable.
function PS:calculateStackIndexOfLocal(locals, localName)
  local localProps = locals[localName]
  -- First check if the local exists
  assert(localProps ~= nil)
  dumpLocals2(locals)
  -- This is the index of the self.stackDepth that corresponds to the pseudo-frame of
  -- our search.
  local currentDepthFrame = #self.stackDepth
  -- This is the sum of all self.stackDepth elements we've visited
  local accumulatedStackDepth = 0
  local localsFrame = locals
  while true do
    -- Interrogate the 'locals' table for visibility within each pseudo-frame, starting
    -- at the top, and working our way toward the bottom. When we can't see it anymore,
    -- the last frame we examined is its true home.
    if localsFrame[localName] == nil then break end
    -- Tally up the total stack depth from the top frame to the local's true home.
    accumulatedStackDepth = accumulatedStackDepth + self.stackDepth[currentDepthFrame]
    -- Walk up the chain of nested local variable scopes, and ..
    localsFrame = getmetatable(localsFrame).__index
    -- ..our self.stackDepth[]
    currentDepthFrame = currentDepthFrame - 1
    -- Sanity checks
    assert(localsFrame ~= nil)
    assert(currentDepthFrame > 0)
  end
  -- We found our local in the previous frame, so, here we are
  currentDepthFrame = currentDepthFrame + 1
  self:dumpStack() -- whatever
  -- Ensure a little sanity
  if localProps.indexFromBottom >= self.stackDepth[currentDepthFrame] then
    trace('error')
    --dumpLocals2(locals)
    error(string.format('local "%s" claims to have indexFromBottom=%d, but was found at stack frame %d with a stack depth of %d', localName, localProps.indexFromBottom, #self.stackDepth - currentDepthFrame, self.stackDepth[currentDepthFrame]))
  end
  -- Calculate and return the local's position from the top of the stack.
  return accumulatedStackDepth - localProps.indexFromBottom - 1
end

function PS:dumpStack()
  for i = 1, #self.stackDepth do
    trace(string.format('PS:stackDepth(%d) = %d', #self.stackDepth - i, self.stackDepth[i]))
  end
end

function PS:emit(...)
  local args = {...}
  for i, a in ipairs(args) do

    -- Insert delimiters between PostScript commands
    if i > 1 then self:write(' ') end

    -- Push a number
    if 'number' == type(a) then

      -- increment stack depth
      self:nudgeStackDepth(1)

      -- emit postscript
      self:write(a)

    -- Execute some PostScript command
    elseif 'string' == type(a) then
      
      -- Is this a known command?
      if self.psCommands[a] ~= nil then

        -- Check for stack underflow
        local cmd = self.psCommands[a]
        if self:getStackDepth() < cmd.pop then
          trace(string.format('error emitting "%s" PostScript command', a))
          error('PostScript stack underflow')
        end

        -- Emit the PostScript command
        self:write(a)

        -- Adjust our current stack depth
        self:nudgeStackDepth(cmd.push - cmd.pop)

      -- The command is to evaluate a global
      elseif self:isGlobalIdentifier(a) then
        self:nudgeStackDepth(1)
        self:write(a)

      -- The command is a PostScript nametype
      elseif string.starts(a, '/') then
        self:nudgeStackDepth(1)
        self:write(a)

      -- If the last argument is a string beginning with '%' then
      -- we include it as a comment
      elseif string.starts(a, '%') then
        self:write(a)

      else error(string.format('emitting unknown identifier: "%s"', a))
      end
    else error("illegal emit argument")
    end
  end
  self:write('\n')
end

-- Emit PostScript instructions to evaluate a local variable.
function PS:emitLocalRvalue(locals, localName, pos)
  if pos == nil then pos = -1 end
  -- Find this local's position on the stack, from the top
  local stackIndex = self:calculateStackIndexOfLocal(locals, localName)
  -- Emit PostScript
  self:write(string.format('%d index %% evaluate local "%s" as an rvalue pos=%d\n', stackIndex, localName, pos))
  -- Adjust stack depth
  self:nudgeStackDepth(1)
end

-- Use this to emit PostScript instructions to set the value of
-- a local variable that was previously declared. The rvalue is
-- the value at the top of the stack.
function PS:emitLocalAssignment(localName, locals, pos)
  --
  if pos == nil then pos = -1 end
  trace(string.format('assigning to local "%s" pos=%d', localName, pos))
  -- Find this local's position on the stack, from the top
  local stackIndex = self:calculateStackIndexOfLocal(locals, localName)
  trace(string.format('stack index found: %d', pos))
  -- Emit PostScript (TODO bypassing :emit why?)
  self:write(string.format('%d %d roll pop %d 1 roll %% assign to local "%s" pos=%d\n',
    stackIndex + 1, stackIndex, stackIndex, localName, pos))
  self:nudgeStackDepth(-1)
end

-- TODO this will have to be rethought if I begin using the dictionary
-- stack to implement Lua metatable __index.
-- TODO refactor to avoid the exch? Or leave such an optimization up to
-- a PostScript optimizer?
function PS:emitGlobalAssignment(id)
  assert('string' == type(id))
  trace(string.format('assigning to global "%s"', id))
  self:emit('/' .. self:makeGlobalIdentifier(id), 'exch', 'def', '% assign to global "'..id..'"')
end

function PS:doFunction(numArgs, pos, fn)
  -- Our stackDepth should be zero, unless this is a nested function. This is
  -- not a definitive test, though, as a function that has zero locals at the
  -- point of its nested function will still leave us with a stackDepth of zero.
  if self:getStackDepth() ~= 0 then
    for k, v in ipairs(self.stackDepth) do
      print(string.format('stackDepth[%d] = %d', k, v))
    end
    error("doFunction() encountered unclean stack (nested functions not supported, module-scope locals not supported)")
  end

  self:doStackFrame(function()
    self:write('{ {\n')

    -- Our function calling supports variadic argument, and we simplify things
    -- some by using PostScript's "mark" to delineate stack frames. Our function
    -- introduction code uses these marks to count arguments, and sets their values
    -- to nil, for which we use the PostScript value 'null'.
    self:write(string.format('counttomark %d sub 1 0 { pop null } for %% function pos=%d\n', 
      numArgs - 1
    , pos
    ))

    -- Record our new stack depth. Our caller should count current locals, which
    -- should only be function arguments at this point, from zero.
    self:nudgeStackDepth(numArgs)

    -- Let lua2ps() handle the function body
    fn()

    -- Close the function
    self:write('} loop }\n')
  end)

  -- Increment stack depth to account for the presence of the new function's
  -- PostScript proc being put on the stack
  self:nudgeStackDepth(1)
end

function PS:emitFunctionReturn(numReturns, pos)
  --
  if pos == nil then pos = -1 end
  -- Right now we only support exactly one return value! TODO
  assert(numReturns == 1)
  -- We're going to use the PostScript 'mark' feature to unwind our stack frame.
  -- But we must first roll our return values under the mark.
  self:write(string.format('counttomark 1 add 1 roll cleartomark exit %% return pos=%d\n', pos))
end

function PS:emitGlobalFunctionCall(id, numArgs, evalArg)
  -- Our calling convention requires us to push a PostScript mark, followed by the
  -- function call arguments, in right-to-left order. XXX We're also evaluating
  -- these arguments in right-to-left order, which is not what Lua does.

  -- We can emit 'mark' here safely, since currently we assume that every function
  -- has *exactly* one return value. The callee cleans up the mark, and our stack
  -- depth achieves parity on its own.
  self:emit('mark', string.format('%% setting up call for "%s()"', id))

  -- Evaluate arguments in right-to-left order
  for i = numArgs, 1, -1 do
    evalArg(i)
  end

  -- Let's bypass emit() for simplicity. The code emitted here will invoke the named
  -- PostScript operator, using the dictionary stack. The user dict will contain all
  -- of our globals, so this should work just fine, and the callee will clean up the
  -- stack for us, and leave us *exactly* one return value. We'll also emit a 'phony'
  -- for this return value.
  -- TODO support variadic return values.
  self:write(self:makeGlobalIdentifier(id), '\n')

  -- The callee cleans the arguments and the mark we pushed for it, and returns exactly
  -- a single value.
  self:nudgeStackDepth(-numArgs -1 +1)
end

-- This is used for non-Function Block nodes. Functions have varargs, and deal with the
-- stack their own way. For other Block nodes, like those in loops, or branches, we use
-- this to emit PostScript's curly braces, and also to verify that the stack remains
-- clean in each loop. This function sets up and tears down the block on the PostScript
-- side. On the Lua side, 'fn' is called to process the contents of the associated
-- Block node.
function PS:doBlockScope(fn)
  -- Emit curly brace to begin PostScript proc
  self:write('{')

  self:doStackFrame(function()
    -- Call 'fn' to allow our caller to process the contents of the block
    fn()
    -- We're at the end of our PostScript proc, and we need ensure that the stack depth is
    -- in the state expected at the beginning of the proc, less whatever PostScript will be
    -- putting on the stack for us. Block node processing should take care of it for us,
    -- but let's verify that it did its job correctly.
    if self:getStackDepth() ~= 0 then
      error(string.format('Block mismanged PostScript operand stack by %d operands',
        self.stackDepth[#self.stackDepth]))
    end
  end)

  -- Emit curly brace to close PostScript proc
  self:write('}')
  -- Increment monitored stack depth here, because we've just pushed the PostScript proc
  -- within the curly braces.
  self:nudgeStackDepth(1)
end

function PS:emitString(s)
  self:nudgeStackDepth(1)
  self:write('(', s, ')') -- TODO escape!
end

function PS:close()
  trace('PS:close()')
  self:write('count 0 ne { (Non-empty stack at exit:) == pstack } if quit\n')
  if self.out ~= io.stdout then self.out:close() end
end
