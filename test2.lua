local count = 0
local function b()
	count = count + 1
	print(count)
	b()
end

b()
