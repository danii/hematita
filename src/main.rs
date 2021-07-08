mod ast;

use self::ast::{lexer::Lexer, parser::parse};
use std::{fs::File, io::Read};

fn main() {
	let mut code = String::new();
	File::open("test.lua").unwrap().read_to_string(&mut code).unwrap();

	let lexer = Lexer {source: code.chars().peekable()};
	let parser = parse(&mut lexer.peekable());
	println!("{:#?}", parser);

	//println!("{:?}", lexer.collect::<Vec<_>>());
}
