use self::super::lexer::Lexer;

/// If whitespace is parsed once, the next character might change, but doing it
/// again shouldn't change it.
#[test]
fn parse_whitespace_multiple() {
	let mut lexer = Lexer {source: "   hello_there".chars().peekable()};

	let end = lexer.parse_whitespace();
	assert_eq!(lexer.peek(), end);
	assert_eq!(lexer.peek(), end);
	assert_eq!(lexer.next(), end);
}

/// After calling, the next character should not be whitespace.
#[test]
fn parse_whitespace_not_whitespace() {
	let mut lexer = Lexer {source: "   hello_there".chars().peekable()};

	let end = lexer.parse_whitespace();
	assert_ne!(end, Some(' ')); assert_ne!(end, Some('\n'));
	assert_ne!(lexer.peek(), Some(' ')); assert_ne!(lexer.peek(), Some('\n'));
	assert_ne!(lexer.next(), Some(' ')); assert_ne!(lexer.next(), Some('\n'));
}
