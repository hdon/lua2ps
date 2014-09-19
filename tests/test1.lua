function foo() return 23 end
function bar(fn) return fn() end
print(bar(foo))

--function foo() return 23 end
--print(foo())

--foo = { bar = function() return 23 end }

--foo = { bar = 3 }
--print(foo.bar)
--foo.bar()

--function foo(b)
--  local a = nil
--  if b == true then
--    a = 42
--  else
--    a = nil
--  end
--  return a
--end
--print(foo(true))

--end
--print(foo(true))
--print(foo(false))
--print(foo())

--print(123)

--function foo()
--  local i = 0
--  for j = 1, 10 do
--    i = i + j
--  end
--  return i
--end
--foo()
--print(foo())

--function foo(table, index)
--  print(table[index])
--end
--foo({fo = 'lol'}, 'fo')

--function macfiber() end

--myTable = {10, 20, 30, 40, 50}
--function doubleIt(table, index)
--  table[index] = table[index] * 2
--end
--for i = 1, 3 do doubleIt(myTable, i) end
--myTable[6] = 'Goodbye!'
--for i = 1, #myTable do print(myTable[i]) end

--for i = 0, 4, 2 do print(i) end
--for i = 0, 3, 1 do print(i) end

--foo = {[1] = 'foo!!!', ['bar'] = 'bar!!!'}
--print(foo[1])
--print(foo['bar'])

--print(foo[23][42]) -- WORKS
--foo[23][42] = 'bar'

--myTable = {[0] = 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89}
--myTable[1] = 'foo'
--print(myTable[1])
--print(myTable[7])

--function foo(a, b)
--  return myTable[a] + myTable[b]
--end

--function foo(a, b)
--  return a + b
--end
--print(foo(5, 6))

--function foo()
--  return 23
--end
--foo()
