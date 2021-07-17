a = {}
b = {["__len"] = function() return 1 end}
setmetatable(a, b)
print(#a)

print(true and 1 or print("P"))
