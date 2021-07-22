function bag()
	local value = "default"
	return function(new)
		local old = value
		value = new
		return old
	end
end

local value = bag()
print(value("A"))
print(value("B"))
print(value("C"))
