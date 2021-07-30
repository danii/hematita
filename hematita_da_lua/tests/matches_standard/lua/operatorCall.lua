function test()
  return 1, 2, 3, nil
end

local a, b, c, d, e = test()
print(a, b, c, d, e)

print(test())
print(test(), 1)
