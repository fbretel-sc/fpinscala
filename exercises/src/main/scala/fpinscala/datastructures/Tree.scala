package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Long = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def sizeBreadth[A](t: Tree[A]): Long = {
    @annotation.tailrec
    def sizeBreadthAux(curLevel: List[Tree[A]], sum: Long): Long =
      curLevel match { // isEmpty()
        case Nil => sum
        case Cons(_, _) =>
          val (nextLevel, curLevelCount) = List.foldLeft(curLevel, (List[Tree[A]](), sum))((acc, node) =>
            node match {
              case Leaf(_) => (acc._1, acc._2 + 1)
              case Branch(left, right) => (List.append(acc._1, List(left, right)), acc._2 + 1)
            })
          sizeBreadthAux(nextLevel, curLevelCount)
      }

    t match {
      case Leaf(_) => 1
      case Branch(left, right) =>
        sizeBreadthAux(List(left, right), 1)
    }
  }

  def maximum(t: Tree[Int]): Int = {
    // better: get rid of the aux meth and just return Leaf.value in matching
    def maximumAux(tt: Tree[Int], curMax: Int): Int =
      tt match {
        case Leaf(value) => if (value > curMax) value else curMax
        case Branch(left, right) => maximumAux(left, curMax) max maximumAux(right, curMax)
      }
    maximumAux(t, Int.MinValue)
  }

  def depth[A](t: Tree[A]): Long = t match {
    // WRONG reaching a leaf should return 0
    case Leaf(value) => 1
    case Branch(left, right) => 1 + depth(left) max depth(right)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def foldOnLeaves[A,B](t: Tree[A], z: B)(f: (A, B) => B): B =
    t match {
      case Leaf(value) => f(value, z)
      case Branch(left, right) => foldOnLeaves(right, foldOnLeaves(left, z)(f))(f)
    }

  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B =
    t match {
      case Leaf(value) => l(value)
      case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
    }

  def sizeViaFold[A](t: Tree[A]): Long = fold(t)(_ => 1)(1 + _ + _)
  def maximumViaFold(t: Tree[Int]): Int = fold(t)(i => i)(_ max _)
  def depthViaFold[A](t: Tree[A]): Long = fold(t)(_ => 0)(1 + _ max _)
  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t) (l => Leaf(f(l)): Tree[B]) ((l, r) => Branch(l, r))

}