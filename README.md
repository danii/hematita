Hematita Da Lua
===============
![](https://img.shields.io/crates/d/hematitia?style=for-the-badge) ![](https://img.shields.io/tokei/lines/github/danii/hematita?style=for-the-badge) ![](https://img.shields.io/crates/v/hematita?style=for-the-badge) ![](https://img.shields.io/badge/compiler%20version-1.53.0-007EC6?style=for-the-badge)
<br>
[![](https://img.shields.io/badge/crates.io-E6B14C?style=for-the-badge&logo=rust&logoColor=000000)](https://crates.io/crates/hematita) [![](https://img.shields.io/badge/lib.rs-282A36?style=for-the-badge&logo=rust)](https://lib.rs/crates/hematita) [![](https://img.shields.io/badge/github.com-24292E?style=for-the-badge&logo=github)](https://github.com/danii/hematita) [![](https://img.shields.io/badge/sponsor_me-FF69B4?style=for-the-badge&logo=github%20sponsors&logoColor=FFFFFF)](https://github.com/sponsors/danii) [![](https://img.shields.io/badge/telegram_group-26A5E4?style=for-the-badge&logo=telegram)](https://t.me/danii_hangout)

Hematita Da Lua is an interpreter for the scripting language Lua, written entirely in 100% safe Rust. Hematita is the portugese word for hematite, a type of iron oxide, or rust, and lua is the portugese word for moon. 'Hematita Da Lua' is a pun on what the project is, and the discovery that [iron on the moon is rusting](https://www.nasa.gov/feature/jpl/the-moon-is-rusting-and-researchers-want-to-know-why).

The purpose of the project is to provide a hardened Lua interpreter resilient to security vulnerabilities. It accomplishes this by using no unsafe code, being compileable on stable, and by relying upon a minimal number of dependencies. With this, we can be confident we are safe from any yet-to-be-found security vulnerabilities in C code. No disrespect to the standard Lua implementation and other C projects.

That said, it is important to note that *Hematita is not stable*, is *very early in it's development* and *may be buggy*. It is my hope that, with enough time to mature, Hematita will be able to guarantee these things.

Running Hematita Standalone
---------------------------
If you'd like to give the interpreter a test drive, run `cargo install hematita_cli`, and then run `hematita_cli`. You'll be placed in a basic REPL, and you can press `Ctrl` + `C` at any time to exit. A large proportion of the Lua code you throw at it should work fine, but not everything, such as some features of `for` loops. Please file an issue if you encounter anything that doesn't work, and it'll be fixed soon in a future version!

The command line interface has quite a few options; running it with `--help` will show them all.
```
OPTIONS:
	-h, --help        Displays this and quits
	-V, --version     Displays version information
	-v, --verbose     Runs with verbose output
	-i, --interactive Runs in interactive mode, after running SOURCE
	-e, --evaluate    Treats source as direct source code, rather than a file
	-b, --byte-code   Shows byte code rather than executing
	-s, --ast         Shows abstract syntax tree rather than executing
	-t, --tokens      Shows tokens rather than executing
```

Currently, `--verbose` does nothing, and is ignored. Running with either `--help` or `--version` will prevent any code from being ran. `--interactive` can be passed with a file, and after execution of the file ends, you'll be dropped into a REPL with all the state of the script left for you to mess with. Using `--evaluate` will evaluate code passed directly on the command line, rather than loading it from a file.

The `--byte-code`, `--ast`, and `--tokens` options all change the output of the interpreter. Using `--byte-code` will print out the compiled byte code of the program rather than executing it. `--ast` will print out the interpreted abstract syntax tree rather than executing, and `--tokens` will print out debug views of the token stream rather than executing. Each of these options correspond to a different segment of the interpreter, see the [internals] section for more information.

Embedding Hematita
------------------
Embedding Hematitia is fairly straight forward and only requires stringing together each segment of the interpreter. As always, require the crate in your `Cargo.toml`. Then, you're just six lines of code away from running Lua code in your next big project.
```rust
use hematita::{ast::{lexer, parser}, compiler, vm, lua_lib, lua_tuple};

// Ready our Lua source code.
let source = "print(\"Hello, World!\")";
// Create a lexer (just a token iterator) from the characters of our source code.
let lexer = lexer::Lexer {source: source.chars().peekable()}.peekable();
// Parse from the lexer a block of statements.
let parsed = parser::parse_block(&mut parser::TokenIterator(lexer)).unwrap();
// Compile bytecode from the block of statements.
let compiled = compiler::compile_block(&parsed);

// Prepare the global scope.
let global = lua_lib::standard_globals();
// Create the virtual machine...
let virtual_machine = vm::VirtualMachine::new(global);
// And run the byte code.
virtual_machine.execute(&compiled.into(), lua_tuple![].arc()).unwrap();
```

`VirtualMachine` is `Send` + `Sync`, and includes a lifetime `'n` for the native functions and user data you make for your convenience. So, bust out the old `crossbeam::thread::Scope`, and go wild. If you're curious about what each of the lines do, see the [internals] section for more information.

Creating your own native function is easy too. All it takes is to modify the global scope with any old function like type, i.e. any dynamically dispatchable `Fn`.
```rust
let number = Mutex::new(0);
// Rust is bad at inferring closure paramaters, so it needs a &_. :(
let counter = move |_, _: &_| {
	let mut lock = number.lock().unwrap();
	let old = *lock;
	*lock += 1;
	Ok(lua_tuple![old].arc())
};

let global = {
	let globals = standard_globals();

	let mut data = globals.data.lock().unwrap();
	data.insert(lua_value!("counter"), Value::NativeFunction(&counter));
	drop(data);

	globals
};
```

The same goes for creating your own user data. Make a type, implement `vm::value::UserData` for it, and insert it into the global scope with `Value::UserData`. (Or, if you prefer, make a native function function that returns it.) As is typical with implementing your own user data, the vast majority of your type will be implemented via the metatable. You can lock your metatable by adding a `__metatable` entry into it.

The Internals
-------------
Hematita is composed of four main segments. Those being, `ast::lexer`, `ast::parser`, `compiler`, and `vm`, each segment depending on the last. Each segment represents a different process, the `lexer` is the lexing process, the `parser` is the parsing process, so on and so forth.

Each segment can be used on it's own, but they're best used all together. If you'd like to just lex and parse lua code, the `ast` module can totally handle that. If you'd like to just run hand crafted bytecode, the `vm` module is well suited for it. But the real effect comes from stringing everything together, to form a complete interpreter.

### The Lexer
The lexer just turns a stream of characters into a stream of `Token`s. It's effectively just an operation over an iterator. You can read it's docs [here](https://docs.rs/hematita/0.1.0/hematita/ast/lexer/index.html), note that they are incomplete.

### The Parser
The parser takes a stream of tokens, and turns it into a `Block`. A `Block` is just a `Vec` of `Statement`s. `Statement`s are an internal representation of a Lua statement. You can read it's docs [here](https://docs.rs/hematita/0.1.0/hematita/ast/parser/index.html), note that they are incomplete.

### The Compiler
The compiler takes a `Block`, and produces a `Chunk`. A `Chunk` is just a `Vec` of `OpCode`s, with some metadata. It is effectively a one to one transformation, so no error handling is needed. You can read it's docs [here](https://docs.rs/hematita/0.1.0/hematita/compiler/index.html), note that they are incomplete.

### The Virtual Machine
The virtual machine takes a `Function`, and executes it. A `Function` is just an instantiated form of a `Chunk`, with associated up-values. It can be made just by calling `into` on a `Chunk`. The virtual machine is effectively a match statement over every `OpCode`, and the code that implements it. You can read it's docs [here](https://docs.rs/hematita/0.1.0/hematita/vm/index.html), note that they are incomplete.

[internals]: #the-internals
