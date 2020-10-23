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
  type EndoF[A] = A => A
  def endoMonoid[A]: Monoid[EndoF[A]] =
    new Monoid[EndoF[A]] {
      override def op(a1: EndoF[A], a2: EndoF[A]): EndoF[A] =  a1 andThen a2
      override def zero: EndoF[A] = identity
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
    ???

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    ???

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    ???

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    ???

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    ???

  def ordered(ints: IndexedSeq[Int]): Boolean =
    ???

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] =
    ???

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    ???

  //  val wcMonoid: Monoid[WC] = ???

  def count(s: String): Int = ???

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    ???

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    ???

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    ???

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    ???
}

trait Foldable[F[_]] {

  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    ???

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    ???

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    ???

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    ???

  def toList[A](as: F[A]): List[A] =
    ???
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    ???

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    ???

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    ???
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    ???

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    ???

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    ???
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    ???

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    ???
}

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    ???

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    ???

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    ???
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    ???

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    ???

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    ???
}
