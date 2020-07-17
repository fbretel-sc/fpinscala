package fpinscala.parsing

import fpinscala.testing.{Gen, Prop}

import language.higherKinds

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]

  implicit def string(s: String): Parser[String]
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2) // use `self` to explicitly disambiguate reference to the `or` method on the `trait`
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many: Parser[List[A]] = self.many(p)
  }

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)
  def empty: Parser[Nothing]
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
  def int(c: Char): Parser[Int]

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  def and[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  // aka many()
  def zeroOrMore[A](p: Parser[A]): Parser[A]
  def oneOrMore[A](p: Parser[A]): Parser[A]

  def many[A](p: Parser[A]): Parser[List[A]]
  def map[A,B](a: Parser[A])(f: A => B): Parser[B]
//  def many[A](p: Parser[A]): Parser[Int] = map(many(p))(_.size)

  object Laws {
    val ls = List(
      (c: Char) => run(char(c))(c.toString) == Right(c),
      (s: String) => run(string(s))(s) == Right(s),
      () => run(or(string("abra"),string("cadabra")))("abra") == Right("abra"),
      () => run(or(string("abra"),string("cadabra")))("cadabra") == Right("cadabra"),
      () => run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad"),
      () => run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab"),
      () => run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab"),

      (c: Char) => run(int(c))(c.toString) == Right(1),
      () => run(zeroOrMore(int('a')))("aa") == Right(2),
      () => run(zeroOrMore(int('a')))("") == Right(0),
      () => run(zeroOrMore(int('a')))("b123") == Right(0),
      () => run(zeroOrMore(int('a')))("ba") == Right(0),
      () => run(oneOrMore(int('a')))("aa") == Right(2),
      () => run(oneOrMore(int('a')))("zza") == Left(ParseError(/* FIXME */ List(), List())),
      () => run(and(zeroOrMore(int('a')), oneOrMore(int('b'))))("bbb") == Right((0,3)),
      () => run(and(zeroOrMore(int('a')), oneOrMore(int('b'))))("aaaab") == Right((4,1)),
      (input: String) => run(zeroOrMore(int('a')))(input) == run(or(empty, oneOrMore(int('a'))))(input)
    )

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)

    def succeedLaw[A](a: A)(in: Gen[String]): Prop = Prop.forAll(in)(s => run(succeed(a))(s) == Right(a))
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}