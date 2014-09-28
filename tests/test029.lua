-- lua-parser provides a Paren node for line 5
function foo() return 2, 3 end
function bar() return  foo()  end
--function baz() return (foo()) end
print(bar())
--print(baz())
