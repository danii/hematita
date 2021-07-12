Lua Hematita
============
Lua Hematita is an interpreter for the scripting language, Lua, written entirely in 100% safe Rust. (Check the top of [lib.rs](./lua_hematita/src/lib.rs)!) Hematita is the portugese word for hematite, a type of iron oxide, or rust, and lua is the portugese word for moon. 'Lua Hematita' is a pun on what the project is, and the discovery that [iron on the moon is rusting](https://www.nasa.gov/feature/jpl/the-moon-is-rusting-and-researchers-want-to-know-why).

The project is currently in alpha, but will be entering beta soon! There's not much documentation on the project, but there are a few items in the source that are documented very well, but beware, some of it is out of date.

If you'd like to test it out, you can build lua_hematita_cli. The CLI takes a verb and a lua source file. There are four verbs, and each verb represents an internal processes within the interpreter. The first three print internal representations, and the fourth one, 'run', runs the lua code.

The 'lex' verb lexes a source file and prints tokens. The 'parse' verb lexes and parses a sourcefile, and prints statements. 'compile' lexes, parses, and generates bytecode, and prints the bytecode. Lastly, 'run' runs a lua source file from the bytecode.
