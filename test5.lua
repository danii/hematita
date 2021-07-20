data = {1, 2, 3, 4, 5}

local i = 0
while #data > i do
  i = i + 1
  print(data[i])
end

local i = 1
repeat
  print(data[i])
  i = i + 1
until i > #data

