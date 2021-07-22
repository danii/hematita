function oops(value)
	error("Oopsie!", value)
end

function yay(value)
	return "Yay!"
end

print(pcall(oops, 4))
print(pcall(yay, 4))
