MyClass = {}

function MyClass.new()
	local self = {count = 0}
	setmetatable(self, {__index = MyClass})
	return self
end

function MyClass:increment()
	self.count = self.count + 1
end

function MyClass:getCount()
	return self.count
end

local counter = MyClass.new()
counter:increment() counter.increment(counter) MyClass.increment(counter)
print(counter:getCount())
