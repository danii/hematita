Syntax
------
These features require modification to the compiler, involving changes to the lexer, parser and code generator. These typically have to do with how the language looks.

| Feature                      | Remaining Systems  | Example                                                                                       |
| ---------------------------- | ------------------ | --------------------------------------------------------------------------------------------- |
| For Loops                    | codegen            | <pre lang="lua">for x in iter() do<br>  print(x)<br>end</pre>                                 |
| While Loops                  | codegen            | <pre lang="lua">while i < 5 do<br>  print(array[i])<br>end</pre>                              |
| Function Definition Accessor | parser & codegen   | <pre lang="lua">function Namespace.helloWorld()<br>  print("Hello, world!")<br>end</pre>      |
| Method Definition            | parser & codegen   | <pre lang="lua">function Class:setCounter(x)<br>  self.counter = x<br>end</pre>               |
| No Parenthesis Function Call | :heavy_check_mark: | <pre lang="lua">print "Hello"<br>print {1, 2, 3}</pre>                                        |
| Method Call                  | parser & codegen   | <pre lang="lua">local class = Class()<br>class:setCounter(4)</pre>                            |
| Order Of Operations          | parser             | <pre lang="lua">-- Prints 7 like a well behaved interpreter should.<br>print(1 + 2 * 3)</pre> |
| Binary Operators             | codegen            | <pre lang="lua">print(1 == 1 and 2 ~= 3)</pre>                                                |
| Unary Operators              | parser & codegen   | <pre lang="lua">print(not ~8)</pre>                                                           |

Virtual Machine
---------------
These features require modification to the virtual machine and it's opcodes, and occasionally the code generator. These typically have to do with how the language acts.

| Feature           | Remaining Systems         | Technical Notes                                                                                                |
| ----------------- | ------------------------- | -------------------------------------------------------------------------------------------------------------- |
| Global Scope      | virtual machine & codegen | Currently, globals can be accessed but they cannot be assigned to or modified.                                 |
| Userdata          | virtual machine           | Userdata, AKA native types currently do not exist in any capacity.                                             |
| Native Functions  | :heavy_check_mark:        | While native functions are complete, arguments to them may need to be reworked, see upvalue's technical notes. |
| Upvalues          | virtual machine & codegen | In order to properly implement upvalues, a rework of local variables needs to be completed.                    |
| Metatables        | virtual machine           | Metatables do exist, but not all operations use the metamethods on a table's metatable.                        |

Project Structure
-----------------
These features are rather monolithic in scale, but thankfully there's no desperate need to have these implemented any time soon. For now, the plan is these features will be implemented in mid to late beta of the project.

| Feature                 | Remaining Systems | Technical Notes                                                                                                                                                                                                                     |
| ----------------------- | ----------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Garbage Collector       | virtual machine   | Currently, the project uses Rust's Arc type for memory reclamation. In other words, reference counting, which is known to be slow.                                                                                                  |
| String Interner         | all systems       | All strings created and used are either fully owned, or leaked (to avoid having to change types). This feature will be due before the garbage collector because of it's easy nature, and that it will fix some of the `Box::leak`s. |
| Source File Information | all systems       | This involves adding source file information, such as line number and source file name to all data structures. This feature is the least prioritized right now.                                                                     |
