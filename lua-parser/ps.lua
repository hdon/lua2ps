require 'util'

PS = {
  out = io.stdout
}
PS.__index = PS

function PS:new(out)
  local ps = setmetatable({
    stackDepth = {0}
  , psCommands  = {
      index     = { pop = 1 , push = 1 }
    , mul       = { pop = 2 , push = 1 }
    , add       = { pop = 2 , push = 1 }
    , null      = { pop = 0 , push = 1 }
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
  ps.out:write(readFile('prologue.ps'))
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
  trace(string.format('  stack depth change %d -> %d', x, self:getStackDepth()))
end

function PS:emit(...)
  trace('emitting ----->', ...)
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
-- 'index' is an index from the BOTTOM of the stack, not the top!
-- this function converts the index so that it is relative to the
-- top of the stack.
function PS:emitLocalRvalue(index, id, pos)
  -- Give anonymous locals a name (NOTE I'm not sure this is needed
  -- but I'm thinking of using it to make some jobs easier.)
  if id == nil then id = 'anonymous' end
  --
  if pos == nil then pos = -1 end
  trace(string.format('evaluating local "%s" (index from bottom = %d, stack depth = %d) pos=%d',
    id, index, self:getStackDepth(), pos))
  -- Check for PostScript stack underflow
  assert(index < self:getStackDepth())
  -- Emit PostScript
  self:write(string.format('%d index %% evaluate local "%s" as an rvalue stackDepth=%d pos=%d\n',
    self:getStackDepth() - index - 1, id, self:getStackDepth(), pos))
  -- Adjust stack depth
  self:nudgeStackDepth(1)
end

-- Use this to emit PostScript instructions to set the value of
-- a local variable that was previously declared. The rvalue is
-- the value at the top of the stack.
function PS:emitLocalAssignment(index, id, pos)
  -- Give anonymous locals a name (NOTE I'm not sure this is needed
  -- but I'm thinking of using it to make some jobs easier.)
  if id == nil then id = 'anonymous' end
  --
  if pos == nil then pos = -1 end
  -- Check for PostScript stack underflow
  assert(index < self:getStackDepth())
  trace(string.format('assigning to local "%s" (index from bottom = %d) pos=%d', id, index, pos))
  -- Emit PostScript
  self:write(string.format('%d %d roll pop %d 1 roll %% assign to local "%s" pos=%d\n',
    self:getStackDepth() - index
  , self:getStackDepth() - index - 1
  , self:getStackDepth() - index - 1
  , id
  , pos))
  self:nudgeStackDepth(-1)
end

-- TODO this will have to be rethought if I begin using the dictionary
-- stack to implement Lua metatable __index.
-- TODO refactor to avoid the exch? Or leave such an optimization up to
-- a PostScript optimizer?
function PS:emitGlobalAssignment(id)
  assert('string' == type(id))
  trace(string.format('assigning to global "%s"', id))
  self:write(string.format('/%s exch def %% assign to global "%s"\n', self:makeGlobalIdentifier(id), id))
end

function PS:openFunction()
  trace('PS:openFunction()')
  self.stackDepth[#self.stackDepth+1] = 0
  self:write('{ {\n')
end

function PS:closeFunction()
  trace('PS:closeFunction()')
  self.stackDepth[#self.stackDepth] = nil
  self:write('} loop }\n')
end

-- Currently this doesn't work for nested functions!
function PS:emitFunctionIntroduction(numArgs, pos)
  -- Our stackDepth should be zero, unless this is a nested function. This is
  -- not a definitive test, though, as a function that has zero locals at the
  -- point of its nested function will still leave us with a stackDepth of zero.
  assert(self:getStackDepth() == 0)
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

function PS:emitGlobalFunctionCall(id)
  -- We don't have to do anything special here because we assume every function has
  -- a single return value.
  self:write(self:makeGlobalIdentifier(id), '\n')
end

function PS:close()
  trace('PS:close()')
  self:write('pstack quit\n')
  if self.out ~= io.stdout then self.out:close() end
end
