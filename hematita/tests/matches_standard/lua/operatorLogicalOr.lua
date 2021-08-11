function w(value)
	if type(value) == "table" then print("table", "side effect")
	else print(value, "side effect") end
	return value
end

print(w(nil) or w(4))
print(w(4) or w(5))
print(w(8) or w(nil))
print(w(true) or w"this is a string")
print(w(false) or w"value")
print(w(true) or w"other")
print(w"i wonder what this string should say..." or w"what this one should say")
print((w{2} or w{1})[1])
