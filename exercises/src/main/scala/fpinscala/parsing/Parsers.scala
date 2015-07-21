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

  /*
   Primitive Combinators
   */
  implicit def string(s: String): Parser[String] // implicit conversion from string to Parser[String]

  def slice[A](p: Parser[A]): Parser[String]

  // unit, lifts a value A into a Parser[A]
  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  def map[A,B](p: Parser[A])(f: A => B): Parser[B]

  def product[A,B](p1: Parser[A], p2: => Parser[B]): Parser[(A,B)]

  // Left biased: evaluates s1 before s2 (no longer associative!)
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  /*
   Derived Combinators
   */

  // Non strictness:
  // 1. If the first parser fails, the second won't even be run!
  // 2. Recursive arguments (e.g. map2(p, many(p))) won't infinite loop
  def map2[A,B,C](p1: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    (p1 ** p2) map(f.tupled)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n == 0) succeed(List[A]())
    else map2(p, listOfN(n-1,p))(_ :: _)
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) | succeed(List[A]())
  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  // parses a single character
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def many: Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def **[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def product[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => self.run(p1)(s) == self.run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    //// TODO: need map2 in testing.Gen first
    //def succeedLaw[A](as: Gen[String], ss: Gen[String]): Prop =
    //  forAll(as.map2(ss)((a,s) => (a,s))) { (a,s) =>
    //      self.run(succeed(a))(s) == Right(a)
    //  }

    // Product should respect UMP for products i.e. 
    // given f: (A,B) => A, g: (A,B) => B, we should have
    // (parser[A] ** parser[B]) map(f) == parser[A] map(f)
    // (parser[A] ** parser[B]) map(g) == parser[B] map(g)
  }


  // Exercise 8: Context-sensitive parser
  // Accept "<n:Int>aaa...(n-times)..."
  //def ex8: Parser[List[String]] = "[0-9]".r flatMap { n =>
  //  listOfN(n.toInt, string("a")) map (n :: _)
  //}
  def ex8: Parser[List[String]] = for {
    digit <- "[0-9]".r // rewritten to flatMap in for comprehension
    val n = digit.toInt
    _ <- listOfN(n.toInt, char("a"))
  } yield n

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
