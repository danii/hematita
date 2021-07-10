actor = {}

metatable = {}
function metatable.__le(self, other, ...)
  print(self, other, ...)
  return true, false
end
setmetatable(actor, metatable)

print(actor >= 4)
