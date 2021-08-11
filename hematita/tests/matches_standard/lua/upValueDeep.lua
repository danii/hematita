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
