function deepUpValue()
	local value = {}
	return function()
		return function()
			return function()
				print(type(value))
			end
		end
	end
end


deepUpValue()()()()

function bag()
	local value = {}
	return function(new)
		local old = value
		value = new
		return old
	end
end

local b = bag()
print(type(b("A")))
print(b("B"))
print(b("C"))
