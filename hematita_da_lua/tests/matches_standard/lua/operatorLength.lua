value = {1, 2}
print(#value)

meta = {["__len"] = function() return 5 end}
setmetatable(value, meta)
print(#value)

print(#[[hello?]])
print(#[==[\nhello\t\v\r\b]==])
print(#"\nhello\t\v\r\b")
print(#[[

newlines and stuff
]])
