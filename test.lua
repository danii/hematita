function bag(print)
	local value = {}

	function ret(new)
		if new then
			value = new
		else
			return value
		end
	end

	local b = ":)"

	print("Yo!")
	return ret
end

function randomError()
	iDontExist()
end

print(pcall(randomError))
