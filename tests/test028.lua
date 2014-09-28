-- lua-parser provides a Paren node for the last line
function foo() return 2, 3 end
print(foo())
print((foo()))
