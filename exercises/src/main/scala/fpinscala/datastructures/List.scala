package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  /* Although we could return `Nil` when the input list is empty, we choose to throw an exception instead. This is
  a somewhat subjective choice. In our experience, taking the tail of an empty list is often a bug, and silently
  returning a value just means this bug will be discovered later, further from the place where it was introduced.

  It's generally good practice when pattern matching to use `_` for any variables you don't intend to use on the
  right hand side of a pattern. This makes it clear the value isn't relevant. */
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_, t) => if (n > 0) drop(t, n - 1) else l
    case _ => Nil
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    /*
        // WRONG this is not filter()
        def loop[A](as: List[A], acc: List[A]): List[A] = as match {
          case Cons(h, t) => if (f(h)) loop(t, acc) else loop(t, Cons(h, acc))
          case _ => Nil
        }
        loop(l, Nil)
    */
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }
  }

  def initRec[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, initRec(t))
  }

  def reverse[A](l: List[A]): List[A] = {
    def loop[A](as: List[A], acc: List[A]): List[A] = as match {
      case Nil => acc
      case Cons(h, t) => loop(t, Cons(h, acc))
    }

    loop(l, Nil)
  }

  // A bit stupid that we must traverse l twice. Hence the need for an internal temporary mutable structure (ListBuffer)
  def init[A](l: List[A]): List[A] = {
    def loop[A](as: List[A], acc: List[A]): List[A] = as match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => acc
      case Cons(h, t) => loop(t, Cons(h, acc))
    }

    reverse(loop(l, Nil))
  }

  def length[A](l: List[A]): Int = {
    /*
        def lengthAux(xs: List[A], len: Int): Int = xs match {
          case Nil => len
          case Cons(head, tail) => lengthAux(tail, len+1)
        }
        lengthAux(l, 0)
    */
    foldRight(l, 0)((_, len) => len + 1)
  }

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    /*
        // WRONG foldLeft means traversing from left to write
        val reversed = reverse(l)
        @annotation.tailrec
        def foldLeftAux[A,B](as: List[A], n: B)(f: (B, A) => B): B =
          as match {
            case Nil => n
            case Cons(h, t) => foldLeftAux(t, f(n, h))(f)
          }
        foldLeftAux(reversed, z)(f)
    */
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def sumLeft(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def productLeft(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def lengthLeft(ns: List[Int]) =
    foldLeft(ns, 0)((acc, _) => acc + 1)

  def reverseLeft[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, x) => Cons(x, acc))

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((a, b) => f(b, a))

  /*
    List(1, 3, 8).foldLeftViaFoldRight(100)(_ - _) ==
    List(1, 3, 8).foldLeftViaFoldRight(100)((acc, a) => acc - a) ==
    foldRight(List(8, 3, 1), 100)((a, b) => f(b, a)) ==
    foldRight(List(8, 3, 1), 100)((a, acc) => acc - a) ==
    f(8, foldRight(List(3, 1), 100)((a, acc) => acc - a) ==
      ...
    f(8, f(3, f(1, 100))) ((a, acc) => acc - a) ==
    ((100 - 1) - 3) - 8 == 88
  */

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverse(a1), a2)((acc, a) => Cons(a, acc))

  // called `concat` in stdlib
  def join[A](ls: List[List[A]]): List[A] =
    foldLeft(reverse(ls), List[A]())((acc, l) => appendViaFoldLeft(l, acc))

  // better: foldRight(ls, Nil:List[A])(append)

  def mapIntAddOne(ints: List[Int]): List[Int] =
    foldRight(ints, Nil: List[Int])((i, acc) => Cons(i + 1, acc))

  def mapDoubleToString(ds: List[Double]): List[String] =
    foldRight(ds, Nil: List[String])((d, acc) => Cons(d.toString, acc))

  def map[A, B](l: List[A])(f: A => B): List[B] =
  // using foldRightViaFoldLeft for stack-safety or ListBuffer
    foldRightViaFoldLeft(l, List[B]())((a, acc) => Cons(f(a), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRightViaFoldLeft(as, Nil: List[A])((a, acc) => if (f(a)) Cons(a, acc) else acc)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRightViaFoldLeft(as, List[B]())((a, acc) => appendViaFoldLeft(f(a), acc))

  // better: join(map(as)(f))

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)((a) => if (f(a)) List(a) else Nil)

  def zipAddInts(xs: List[Int], ys: List[Int]): List[Int] =
    xs match {
      case Nil => Nil
      case Cons(hx, tx) => ys match {
        case Nil => Nil
        case Cons(hy, ty) => Cons(hx + hy, zipAddInts(tx, ty))
      }
    }

  def zipWith[A](xs: List[A], ys: List[A])(f: (A, A) => A): List[A] =
  // better: match (xs,ys) directly to a pair of Cons()
    xs match {
      case Nil => Nil
      case Cons(hx, tx) => ys match {
        case Nil => Nil
        case Cons(hy, ty) => Cons(f(hx, hy), zipWith(tx, ty)(f))
      }
    }

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h,t) => hasSubsequence(t, sub)
  }
}
