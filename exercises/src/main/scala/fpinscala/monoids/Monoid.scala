package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  // Exercise 10.1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }

  // Exercise 10.2. Not clear what operation we want [foudil].
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = if (a1.isDefined) a1 else a2
    override def zero: Option[A] = None
  }

  // Exercise 10.3
  def endoMonoid[A]: Monoid[A => A] =
    new Monoid[A => A] {
      override def op(a1: A => A, a2: A => A): A => A =  a1 andThen a2
      override def zero: A => A = identity
    }

  // COPIED FROM ANSWERS
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpinscala.testing._
  import Prop._

  // Exercise 10.4 - COPIED FROM ANSWERS
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
  // Associativity
    forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z))(p =>
      m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3)) &&
      // Identity
      forAll(gen)((a: A) =>
        m.op(a, m.zero) == a && m.op(m.zero, a) == a)

  // Exercise 10.4 - monoidLaws() doesn't work for functions as these are compared by value.
  def monoidFunctionLaws[A](m: Monoid[A => A], genF: Gen[A => A], genA: Gen[A]): Prop =
  // Associativity
    forAll(
      for {
        x <- genF
        y <- genF
        z <- genF
        a <- genA
      } yield (x, y, z, a)
    )(p =>
      m.op(p._1, m.op(p._2, p._3))(p._4) == m.op(m.op(p._1, p._2), p._3)(p._4)
    ) &&
      // Identity
      forAll(
        for {
          x <- genF
          a <- genA
        } yield (x, a)
      )(p =>
        m.op(p._1, m.zero)(p._2) == p._1(p._2) &&
          m.op(m.zero, p._1)(p._2) == p._1(p._2)
      )

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op) // same result with foldRight

  // Exercise 10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    // as.map(f).foldLeft(m.zero)((b, a) => m.op(a, b))
    // Improved with single pass
    as.foldLeft(m.zero)((b, a) => {
      val res = m.op(b, f(a))
//      println(s"($b, $a) res=$res")
      res
    })
  }

  // Exercise 10.6
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    // f: A => (B => B)
    def f2(a: A): B => B = f(a, _)  // f.curried
    foldMap(as, dual(endoMonoid[B]))(f2)(z)
  }

  // Exercise 10.6
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    // f: B => A => B
    def f2(a: A): B => B = f(_, a)
    foldMap(as, dual(endoMonoid[B]))(f2)(z)
  }

  // Exercise 10.7
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.isEmpty)
      m.zero
    else if (as.length == 1)
      f(as.head)
    else {
      val (left, right) = as.splitAt(as.length / 2)
      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }
  }

  // Exercise 10.8
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)
    override def zero: Par[A] = Par.unit(m.zero)
  }
  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    foldMapV(v, par(m))(Par.asyncF(f))

  // Exercise 10.9
  def ordered(ints: IndexedSeq[Int]): Boolean = if (ints.length < 2) true else {
    case class Segment(lowest: Int, highest: Int, ordered: Boolean)
    val m = new Monoid[Segment] {
      override def op(a1: Segment, a2: Segment): Segment =
        Segment(a1.lowest, a2.highest, a1.ordered && a2.ordered && a1.highest <= a2.lowest)
      override def zero: Segment = Segment(ints(0), ints(0), ordered = true)
    }
    foldMap(ints.toList, m)(i => Segment(i, i, ordered = true)).ordered
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  // Exercise 10.10
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
      case (Stub(s), Part(l, w, r)) => Part(s + l, w, r)
      case (Part(l, w, r), Stub(s)) => Part(l, w, r + s)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + (if (r1.isEmpty && l2.isEmpty) 0 else 1) + w2, r2)
    }
    override def zero: WC = Stub("")
  }

  // Exercise 10.11
  def countWords(s: String): Int = {
    // COPIED FROM ANSWERS
    def wc(c: Char): WC =
      if (c.isWhitespace)
        Part("", 0, "")
      else
        Stub(c.toString)

    def charToCount(s: String) = if (s.isEmpty) 0 else 1

    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s) => charToCount(s)
      case Part(l, w, r) => charToCount(l) + w + charToCount(r)
    }
  }

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    ???

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    ???

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    ???

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    ???
}

// Exercise 10.12
trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(a => (b: B) => f(a, b))(dual(endoMonoid[B]))(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  // Exercise 10.15
  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)
}

// Exercise 10.12
object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
  override def toList[A](as: List[A]): List[A] = as
}

// Exercise 10.12
object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    // foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
    // Better use foldMapV
    Monoid.foldMapV(as, mb)(f)
  override def toList[A](as: IndexedSeq[A]): List[A] = as.toList
}

// Exercise 10.12
object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// Exercise 10.13
object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }

  // Note the default implementations works fine.
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
    case Leaf(a) => f(z, a)
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f) // whatch out l-r order!
  }
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
    case Leaf(a) => f(a, z)
    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }
}

// Exercise 10.14
object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as.fold(mb.zero)(f)
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
    case None => z
    case Some(a) => f(z, a)
  }
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
    case None => z
    case Some(a) => f(a, z)
  }
}
