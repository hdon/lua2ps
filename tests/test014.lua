function foo(a, b) return a+b end
myTable = { foo = foo }
print(myTable.foo(20, 3))
