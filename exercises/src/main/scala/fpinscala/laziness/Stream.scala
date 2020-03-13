package fpinscala.laziness

import Stream._
sealed trait Stream[+A] {

  def toList: List[A] = // foldRight(List[A]())(_ :: _)
  {
    @scala.annotation.tailrec
    def toListAux(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => toListAux(t(), h() :: acc)
      case Empty => acc
    }
    toListAux(this, Nil).reverse
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] =
/*
    // better: put condition into match
    if (n <= 0)
      Empty
    else this match {
      case Empty => Empty
      case Cons(h, t) => cons(h(), t().take(n -1))
    }
*/
    this match {
      case Cons(h, t) if n > 0 => cons(h(), t().take(n -1))
      case _ => Empty
    }

  def drop(n: Int): Stream[A] =
    this match {
      case Cons(_, t) if n > 0 => t().drop(n -1)
      case _ => this
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) =>
        lazy val hh = h()
        if (p(hh))
          cons(h(), t().takeWhile(p))
        else
          Empty
      case Empty => Empty
    }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((a, acc) => if (p(a)) cons(a, acc) else empty)

  def forAll(p: A => Boolean): Boolean = foldRight(true)(p(_) && _)

  def headOption: Option[A] = foldRight(None: Option[A])((h, _) => Option(h)) // better: Some(h)

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, acc) => cons(f(a), acc))

/*
  Stream(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).toList
  (Stream(1,2,3,4).map(_ + 10)).filter(_ % 2 == 0).toList
  (Stream(1,2,3,4).foldRight(empty[B])((a, acc) => cons(f(a), acc))) .filter(_ % 2 == 0).toList
  (Stream(1,2,3,4) match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
   ...
                        cons(1 + 10, ?)
  (cons(1 + 10, Stream(2,3,4).map(_ + 10))) .filter(_ % 2 == 0).toList
*/

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, acc) =>
      if (p(a)) cons(a, acc)
      else acc
    )

  // should probably be named `concat`
  def append[B>:A](s: => Stream[B]): Stream[B] = foldRight(s)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, s) => f(a).append(s))

  // 5.13
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold (this) {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }
  def takeViaUnfold(n: Int): Stream[A] = unfold (this, n) { case(s, i) =>
    s match {
      case Cons(h, t) if i > 0 => Some((h(), (t(), i-1)))
      case _ => None
    }
  }
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold (this) {
      // better: case Cons(h, t) if p(h()) => Some((h(), t()))
      case Cons(h, t) =>
        if (p(h())) Some((h(), t()))
        else None
      case Empty => None
    }
  def zipWith[B,C](other: Stream[B])(f: (A, B) => C): Stream[C] =
/*
    unfold (this, other) { case(s1, s2) =>
      (s1, s2) match {
*/
    unfold (this, other) {
      case (Cons(h1,t1), Cons(h2,t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }
  def zipAll[B](other: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold (this, other) {
      // can use ArrowAssoc a -> b to build pairs
      case (Cons(h1,t1), Cons(h2,t2)) => Some( (Some(h1()), Some(h2())), (t1(),  t2())  )
      case (Empty,       Cons(h2,t2)) => Some( (None,       Some(h2())), (Empty, t2())  )
      case (Cons(h1,t1), Empty)       => Some( (Some(h1()), None),       (t1(),  Empty) )
      case _ => None
    }

  def zip[B](s2: Stream[B]): Stream[(A,B)] = zipWith(s2)((_,_))

  def startsWith[B](s: Stream[B]): Boolean =
/*
    // better: the map is actually a takeWhile
    this zipAll s map {
      case (Some(a), Some(b)) => a == b
      case (Some(_), None) => true
      case _ => false
    } forAll (_ == true)
*/
    this zipAll s takeWhile (_._2.isDefined) forAll {
/*
      // better: compare options directly
      case (Some(a), Some(b)) => a == b
*/
      case (ao, bo) => ao == bo
      case _ => false
    }

  /*
  The last element of `tails` is always the empty `Stream`, so we handle this as a special case, by appending it to the output.
  */
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)

  /*
  The function can't be implemented using `unfold`, since `unfold` generates elements of the `Stream` from left to right. It can be implemented using `foldRight` though.

  The implementation is just a `foldRight` that keeps the accumulated value and the stream of intermediate results, which we `cons` onto during each iteration. When writing folds, it's common to have more state in the fold than is needed to compute the result. Here, we simply extract the accumulated list once finished.
  */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def constant[A](a: A): Stream[A] = // cons(a, constant(a))
  {
    lazy val as: Stream[A] = cons(a, as)
    as
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def fibsAux(a: Int, b: Int): Stream[Int] = cons(a, fibsAux(b, a+b))
    fibsAux(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    // better: get rid of lazy val
    lazy val so = f(z)
    so match {
      case None => empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }
  }

  def fibsViaUnfold(): Stream[Int] =
//    unfold (0, 1) {ab => Some(ab._1, (ab._2, ab._1 + ab._2))}
    unfold (0, 1) { case (a,b) => Some(a, (b, a + b)) }
  def fromViaUnfold(n: Int): Stream[Int] =
    unfold (n) {i => Some(i, i+1)}
  def constantViaUnfold[A](a: A): Stream[A] =
    unfold (a) {_ => Some(a, a)}
  val onesViaUnfold: Stream[Int] =
    unfold (1) {_ => Some(1, 1)}
}