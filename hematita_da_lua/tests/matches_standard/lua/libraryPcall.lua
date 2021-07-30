function oops(value)
	error("Oopsie!", value)
end

function yay(value)
	return "Yay!"
end

local a, b = pcall(oops, 4)
print(a, b)
print(pcall(yay, 4))
