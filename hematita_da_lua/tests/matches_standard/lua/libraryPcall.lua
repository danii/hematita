function oops(value)
	error("Oopsie!", value)
end

print(pcall(oops, 4))
