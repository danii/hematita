values = {
	useInteger = function(meta, method)
		meta[method] = 1
	end,
	useString = function(meta, method)
		meta[method] = "Hello"
	end,
	useBoolean = function(meta, method)
		meta[method] = true
	end,
	useFunction = function(meta, method)
		meta[method] = function()
			return "Used Function"
		end
	end,
	useObject = function(meta, method)
		meta[method] = {}
	end,
	useCallableObject = function(meta, method)
		local object = {}
		local objectMeta = {
			__call = function()
				return "Used Callable Object"
			end
		}
		setmetatable(object, objectMeta)

		meta[method] = object
	end
}

metamethods = {
	__index = function(object)
		return object.test
	end,
	__newindex = function(object)
		object.test = "Value"
	end,
	__call = function(object)
		return object("Hello")
	end
}

for method, test in pairs(metamethods) do
	for typeName, type in pairs(values) do
		local value = {}
		local meta = {}
		setmetatable(value, meta)

		type(meta, method)
		local success, result = pcall(test, value)

		if success then
			print(method, typeName, "SUCCESS", result)
		else
			print(method, typeName, "ERROR", result)
		end
	end
end

loadstring("b = 5"); print(b)
