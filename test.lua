function bag()
	local value = {a = 5, 4}
	print(value.a)
	return function()
		return function()
			return value
		end
	end
end

print(bag()()())

