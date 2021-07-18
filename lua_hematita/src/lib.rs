#![forbid(unsafe_code)]
//! Hematita Da Lua is an interpreter for the scripting language Lua, written
//! entirely in 100% safe Rust. Hematita is the portugese word for hematite, a
//! type of iron oxide, or rust, and lua is the portugese word for moon. 'Lua
//! Hematita' is a pin on what this project is, and the discovery that [iron on
//! the moon is rusting][moon-rust].
//!
//! This project is made of four primary components, each one more complicated
//! than the last.
//! - [ast::lexer] - The Lua lexer, responsible for tokenizing Lua source files
//! - [ast::parser] - The Lua parser, responsible for creating statements and
//!   expressions from tokens
//! - [compiler] - The compiler, responsible for turning parsed statements into
//!   bytecode that the virtual machine can understand
//! - [vm] - The virtual machine, responsible for executing bytecode vaguely
//!   representing Lua source code
//! On top of the four primary components, a Lua standard library is also
//! provided within [lua_lib].
//!
//! Examples
//! --------
//! Executing Lua source text is fairly straight forward, and only requires
//! stringing together each component.
//! ```rust
//! use lua_hematita::{ast::{lexer, parser}, compiler, vm, lua_lib};
//!
//! // Ready our Lua source code.
//! let source = "print(\"Hello, World!\")";
//! // Create a lexer (just a token iterator) from the characters of our source
//! // code.
//! let lexer = lexer::Lexer {source: source.chars().peekable()}.peekable();
//! // Parse from the lexer a block of statements.
//! let parsed = parser::parse(&mut parser::TokenIterator(lexer)).unwrap();
//! // Compile bytecode from the block of statements.
//! let compiled = compiler::compile(&parsed);
//!
//! // Running the bytecode is slightly more involved, as we have to prepare a
//! // global scope for the virtual machine, and arguments for our code. It's
//! // not that hard though.
//! let global = lua_lib::standard_globals().arc();
//! let arguments = vm::value::Table::default().arc();
//! // Create the virtual machine...
//! let virtual_machine = vm::VirtualMachine::new(global);
//! // And run the bytecode.
//! virtual_machine.execute(&compiled.into(), arguments).unwrap();
//! ```
//! Note that the `arguments` argument in [VirtualMachine::execute][vm-execute]
//! go unused for bytecode generated from [compile][compiler-compile], as top
//! level code doesn't have a concept of function arguments. If for any reason
//! you want to compile code with local arguments (not recommended over using
//! the global scope), you can use
//! [compile_function][compiler-compile_function].
//!
//! [moon-rust]: https://www.nasa.gov/feature/jpl/the-moon-is-rusting-and-researchers-want-to-know-why
//! [vm-execute]: crate::vm::VirtualMachine::execute
//! [compiler-compile]: crate::compiler::compile
//! [compiler-compile_function]: crate::compiler::compile_function

pub mod ast;
pub mod compiler;
pub mod lua_lib;
pub mod vm;
