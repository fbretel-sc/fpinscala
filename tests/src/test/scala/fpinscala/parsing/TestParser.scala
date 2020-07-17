package fpinscala.parsing

private[parsing] object TestParserTypes {
  type Parser[+A] = A
}

private[parsing] object TestParser extends Parsers[TestParserTypes.Parser] {
}
