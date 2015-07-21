package fpinscala.parsing

import java.util.regex._
import scala.util.matching.Regex
import fpinscala.testing._
import fpinscala.testing.Prop._

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p) // implicit conversion to enable infix ParserOps[A]
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  implicit def regex(r: Regex): Parser[String]

  /** Primitive Combinators */
  implicit def string(s: String): Parser[String] // implicit conversion from string to Parser[String]

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  // Returns the string of input parsed by parser, ignoring parser output
  def slice[A](p: Parser[A]): Parser[String]

  // unit, lifts a value A into a Parser[A]
  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  def map[A,B](p: Parser[A])(f: A => B): Parser[B] = flatMap(p)(a => succeed(f(a)))

  // Left biased: evaluates s1 before s2 (no longer associative!)
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  // Introduce a label to be returned during a ParseError
  def label[A](msg: String)(p: Parser[A]): Parser[A]

  // for nesting labels
  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  // delays committing to a parse
  def attempt[A](p: Parser[A]): Parser[A]

  /** Derived Combinators */
 // Equivalence of flatMap <=> seq
  def seq[U,A,B](f: U => Parser[A])(g: A => Parser[B]): U => Parser[B] = u => flatMap(f(u))(g)

  //def product[A,B](p1: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
  //  p1.flatMap(a => p2.flatMap(b => succeed((a,b))))
  def product[A,B](p1: Parser[A], p2: => Parser[B]): Parser[(A,B)] = for {
    a <- p1;
    b <- p2
  } yield (a,b)

  // Non strictness:
  // 1. If the first parser fails, the second won't even be run!
  // 2. Recursive arguments (e.g. map2(p, many(p))) won't infinite loop
  //def map2[A,B,C](p1: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
  //  (p1 ** p2) map(f.tupled)
  //def map2[A,B,C](p1: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
  //  (p1 ** p2).flatMap { case (a,b) => succeed(f(a,b)) }
  def map2[A,B,C](p1: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = for {
    a <- p1; // delegate to `flatMap` and `map` in `ParserOps`
    b <- p2
  } yield f(a,b)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n == 0) succeed(List[A]())
    else map2(p, listOfN(n-1,p))(_ :: _)
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) | succeed(List[A]())
  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  // parses a single character
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  /** Sequences two parsers, ignoring the result of the first.
    * We wrap the ignored half in slice, since we don't care about its result. */
  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
    map2(slice(p), p2)((_,b) => b)

  /** Sequences two parsers, ignoring the result of the second.
    * We wrap the ignored half in slice, since we don't care about its result. */
  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p, slice(p2))((a,b) => a)

  /** Parser which consumes zero or more whitespace characters. */
  def whitespace: Parser[String] = "\\s*".r

  /** Parser which consumes reluctantly until it encounters the given string. */
  def thru(s: String): Parser[String] = (".*?"+Pattern.quote(s)).r

  /** Unescaped string literals, like "foo" or "bar". */
  //def quoted: Parser[String] = string("\"") *> thru("\"").map(_.dropRight(1))
  def quoted: Parser[String] = surround("\"", "\"")(".*?".r)

  /** Unescaped or escaped string literals, like "An \n important \"Quotation\"" or "bar". */
  def escapedQuoted: Parser[String] =
    // rather annoying to write, left as an exercise
    // we'll just use quoted (unescaped literals) for now
    token(quoted label "string literal")

  /** C/Java style floating point literals, e.g .1, -1.0, 1e9, 1E-23, etc.
    * Result is left as a string to keep full precision
    */
  def doubleString: Parser[String] =
    token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)

  /** Floating point literals, converted to a `Double`. */
  def double: Parser[Double] =
    doubleString map (_.toDouble) label "double literal"

  /** Parses a token and strips whitespace */
  def token[A](p: Parser[A]): Parser[A] = attempt(p) <* whitespace

  /** Zero or more repetitions of `p`, separated by `p2`, whose results are ignored. */
  def sep[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] = // use `Parser[Any]` since don't care about result type of separator
    sep1(p,p2) or succeed(List())

  /** One or more repetitions of `p`, separated by `p2`, whose results are ignored. */
  def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    map2(p, many(p2 *> p))(_ :: _)

  /** Wraps `p` in start/stop delimiters. */
  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]) =
    start *> p <* stop

  /** A parser that succeeds when given empty input. */
  def eof: Parser[String] =
    regex("\\z".r).label("unexpected trailing characters")

  /** The root of the grammar, expects no further input following `p`. */
  def root[A](p: Parser[A]): Parser[A] =
    p <* eof

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def many: Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def **[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def product[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def *>[B](p2: => Parser[B]) = self.skipL(p, p2)
    def <*(p2: => Parser[Any]) = self.skipR(p, p2)
    def sep[B](p2: => Parser[B]) = self.sep(p, p2)
    def token = self.token(p)
    def as[B](b: B): Parser[B] = self.map(p)(_ => b)
    def scope(msg: String) = self.scope(msg)(p)
    def label(msg: String) = self.label(msg)(p)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => self.run(p1)(s) == self.run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    //// TODO: need map2 in testing.Gen first
    //def succeedLaw[A](as: Gen[String], ss: Gen[String]): Prop =
    //  forAll(as ** ss) { (a,s) =>
    //      self.run(succeed(a))(s) == Right(a)
    //  }

    // Product should respect UMP for products i.e. 
    // given f: (A,B) => A, g: (A,B) => B, we should have
    // (parser[A] ** parser[B]) map(f) == parser[A] map(f)
    // (parser[A] ** parser[B]) map(g) == parser[B] map(g)

    //// TODO: need product (**) in testing.Gen
    //def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
    //  forAll(inputs ** Gen.string) { case (input, msg) =>
    //    run(label(msg)(p))(input) match {
    //      case Left(e) => errorMessage(e) == msg
    //      case _ => true
    //    }
    //  }
  }


  // Exercise 8: Context-sensitive parser
  // Accept "<n:Int>aaa...(n-times)..."
  //def ex8: Parser[List[String]] = "[0-9]".r flatMap { n =>
  //  listOfN(n.toInt, string("a")) map (n :: _)
  //}
  def ex8: Parser[Int] = for {
    digit <- "[0-9]".r // rewritten to flatMap in for comprehension
    val n = digit.toInt
    _ <- listOfN(n.toInt, char('a'))
  } yield n // return number of characters parsed

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
