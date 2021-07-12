function bag()
	local value = {}

	return function(new)
		if new then
			value = new
		else
			return value
		end
	end, function(new)
		if new then
			value = new
		else
			return value
		end
	end
end
