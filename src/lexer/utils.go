package lexer

func GetLeftRightFromDefined(lx *Lexer) (*Token, *Token) {
	left := lx.Tokenize()
	lx.Tokenize()
	right := lx.Tokenize()
	lx.Tokenize()
	return &left, &right
}

func RemoveNumericSuffix(literal string) string {
	if len(literal) == 0 {
		return literal
	}
	last := literal[len(literal)-1]
	if last == 'f' || last == 'F' ||
		last == 'd' || last == 'D' ||
		last == 'L' || last == 'l' {
		return literal[:len(literal)-1]
	}
	return literal
}
