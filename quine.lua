-- This is what is known as a quine, a program that outputs it's own source. They're pretty nifty!
local input = [[print("-- This is what is known as a quine, a program that outputs it's own source. They're pretty nifty!\nlocal input = [" .. "[" .. input .. "]" .. "]\n" .. input)]]
print("-- This is what is known as a quine, a program that outputs it's own source. They're pretty nifty!\nlocal input = [" .. "[" .. input .. "]" .. "]\n" .. input)
