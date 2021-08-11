function test()
  return 1, 2, 3, nil
end

local a, b, c, d, e = test()
print(a, b, c, d, e)
local a, b, c, d, e = test(), 1
print(a, b, c, d, e)
local a, b, c, d, e = 1, test()
print(a, b, c, d, e)

print(test())
print(test(), 1)
print(1, test())
