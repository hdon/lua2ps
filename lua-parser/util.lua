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
