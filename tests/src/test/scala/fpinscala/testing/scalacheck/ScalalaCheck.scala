package fpinscala.testing.scalacheck

import org.junit.runner.RunWith
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ScalalaCheck extends Specification with ScalaCheck {
  "The following exercises should be correct" >> {

    "Exercise 8.1: sum: List[Int] => Int" in {
      val intList = Gen.listOf(Gen.choose(0,100))
      List[Int]().sum mustEqual 0 // zero
      val prop =
         forAll(intList)(ns => ns.reverse.sum == ns.sum) && // commutative
          forAll(intList)(ns => {
            val (l1, l2) = ns.splitAt(ns.length/2)
            l1.sum + l2.sum == ns.sum
          }) && // associative
          forAll(intList)(ns => ns.sum == ns.foldLeft(0)((s, i) => s + i))   // definition
          forAll(intList) { ns => (ns.nonEmpty && ns.forall(i => i == ns.head)) ==> (ns.sum == ns.head * ns.length) }
      prop
    }

    "Exercise 8.2: max: List[Int] => Int" in {
      def listGen(min: Int = 0, max: Int = 32) = for {
        listLen <- Gen.choose(min, max)
        l1 <- Gen.listOfN(listLen, Gen.choose(0, 100))
      } yield l1

      val intList1 = listGen(1)
      val intList2 = listGen(1)

      // The max of the empty list is unspecified and should throw an error or return `None`.
      List[Int]().max must throwA[UnsupportedOperationException] // zero
      val prop =
        forAll(listGen(1,1)) { ns => ns.max == ns.head } && // one
          forAll(intList1, intList2)((ns1, ns2) => {
            val m1 = ns1.max
            val m2 = ns2.max
            (ns1 ::: ns2).max == (if (m1 < m2) m2 else m1)
          }) &&
          // The max of a list is greater than or equal to all elements of the list.
          forAll(intList1)(ns => {
            val m1 = ns.max
            ns.forall(i => i <= m1)
          }) &&
          // The max of a list is an element of that list.
          forAll(intList1)(ns => ns.contains(ns.max))
      prop
    }

  }
}
