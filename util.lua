function trace(...)
  local line
  if indent > 0 then
    line = string.rep(' ', indent)
  else
    line = ''
  end
  for i, s in ipairs({...}) do
    if i > 1 then
      line = line .. ' '
    end
    line = line .. s
  end
  print(line)
end

function string.starts(String,Start)
   return string.sub(String,1,string.len(Start))==Start
end

function mpairs(t)
  local returned = {}
  local function next_family(_,k)
    local v
    repeat
      k,v = next(t,k)
      if not k then break end
    until not returned[k]
    if k then
      returned[k] = true
      return k,v
    end
    local mt = getmetatable(t)
    if mt and type(mt.__index) == 'table' then
      t = mt.__index
      return next_family(nil,nil)
    end
    return nil
  end
  return next_family
end

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

