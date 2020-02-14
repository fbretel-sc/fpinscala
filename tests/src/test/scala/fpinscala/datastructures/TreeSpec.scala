package fpinscala.datastructures

import org.specs2.mutable._
import org.specs2.specification.AllExpectations
import org.specs2.matcher.DataTables
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TreeSpec extends Specification with DataTables {
  import Tree._

  /**
   *                                 b1
   *                       ---------------------
   *                     b11                  b12
   *                ----------            -----------
   *               b111     (14)        b121        (27)
   *        --------------          ------------
   *      (12)         b1112       (0)        (1)
   *                -----------
   *               (5)       (6)
   */

  val l_11121 = Leaf(5 /*11121*/ ) // 1 node
  val b1112 = Branch(l_11121, Leaf(6 /*11122*/ ))
  val b111 = Branch(Leaf(12 /*1111*/ ), b1112)
  val b11 = Branch(b111, Leaf(14 /*112*/ ))
  val b121 = Branch(Leaf(0 /*1211*/ ), Leaf(1 /*12112*/ ))
  val b12 = Branch(b121, Leaf(27 /*21*/ ))
  val b1 = Branch(b11, b12) // 13 nodes

  "The following exercises should be correct" >> {

    "Exercise 3.25: size." in {
      (Tree.size(l_11121) must_== 1) and
        (Tree.size(b12) must_== 5) and
        (Tree.size(b1) must_== 13)
    }

    "Exercise 3.26: maximum." in {
      (Tree.maximum(l_11121) must_== 5) and
        (Tree.maximum(b1112) must_== 6) and
        (Tree.maximum(b11) must_== 14) and
        (Tree.maximum(b12) must_== 27) and
        (Tree.maximum(b1) must_== 27)
    }

    "Exercise 3.27: depth." in {
      (Tree.depth(l_11121) must_== 0) and
        (Tree.depth(b1112) must_== 1) and
        (Tree.depth(b111) must_== 2) and
        (Tree.depth(b12) must_== 2) and
        (Tree.depth(b1) must_== 4)
    }

    "Exercise 3.28: map." in {
      def f(i: Int) = s"[${i.toString()}]"
      def mapLeaf = (Tree.map(l_11121)(f) must_== Leaf("[5]"))
      def mapTree = Tree.map(b111)(f) match {
        case Branch(Leaf(l1111), Branch(Leaf(l11121), Leaf(l11122))) =>
          (l1111 must_== "[12]") and (l11121 must_== "[5]") and (l11122 must_== "[6]")
        case _ => ko
      }
      mapLeaf and mapTree

    }

    "Exercise 3.29: size via fold." in {
      (Tree.sizeViaFold(l_11121) must_== 1) and
        (Tree.size(b12) must_== 5) and
        (Tree.size(b1) must_== 13)
    }

    "Exercise 3.29: max via fold." in {
      (Tree.maximumViaFold(l_11121) must_== 5) and
        (Tree.maximumViaFold(b1112) must_== 6) and
        (Tree.maximumViaFold(b11) must_== 14) and
        (Tree.maximumViaFold(b12) must_== 27) and
        (Tree.maximumViaFold(b1) must_== 27)
    }

    "Exercise 3.29: depth with fold." in {
      (Tree.depthViaFold(l_11121) must_== 0) and
        (Tree.depthViaFold(b1112) must_== 1) and
        (Tree.depthViaFold(b111) must_== 2) and
        (Tree.depthViaFold(b12) must_== 2) and
        (Tree.depthViaFold(b1) must_== 4)
    }

    "Exercise 3.29: map with fold." in {
      def f(i: Int) = i.toString()
      (Tree.mapViaFold(l_11121)(f) must_== Leaf("5")) and
        (
          Tree.map(b111)(f) match {
            case Branch(Leaf(l1111), Branch(Leaf(l11121), Leaf(l11122))) =>
              (l1111 must_== "12") and (l11121 must_== "5") and (l11122 must_== "6")
            case _ => ko
          })
    }

  }
}