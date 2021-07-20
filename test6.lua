local r = 0
function iter()
	if r == 5 then
		return nil
	else
		r = r + 1
		return r
	end
end

for i in iter do
	print(i)
end
