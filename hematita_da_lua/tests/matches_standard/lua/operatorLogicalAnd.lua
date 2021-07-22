function w(value)
	if type(value) == "table" then print("side effect", "table")
	else print("side effect", value) end
	return value
end

print(w(nil) and w(4))
print(w(4) and w(5))
print(w(8) and w(nil))
print(w(true) and w"this is a string")
print(w(false) and w"value")
print(w(true) and w"other")
print(w"i wonder what this string should say..." or w"what this one should say")
print((w{2} and w{1})[1])
