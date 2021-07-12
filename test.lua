function bag(print)
	local value = {}

	function ret(new)
		print(b)
		if new then
			value = new
		else
			return value
		end
	end

	local b = ":)"

	return ret
end

local r = bag(print)
print(r)
r()
