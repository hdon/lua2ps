function foo() return 23 end
function bar(fn) return fn() end
print(bar(foo))
