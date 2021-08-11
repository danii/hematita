data = {1, 2, 3, 4, 5}

i = 0
while #data > i do
  i = i + 1
  print(data[i])
end

i = 1
repeat
  print(data[i])
  i = i + 1
until i > #data

