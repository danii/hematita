actor = {}

metatable = {}
function metatable.__le(self, other, ...)
  print(self, other, ...)
end
setmetatable(actor, metatable)

print(actor >= 4)
