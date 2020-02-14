package fpinscala.gettingstarted

import org.specs2.mutable._
import org.specs2.specification.AllExpectations
import org.specs2.matcher.DataTables
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GettingStartedSpec extends Specification with DataTables {

  "The following exercises should be correct" >> {

    "Exercise 2.1 (Fibonacci)" in {
      (0 to 10).map(MyModule.fib(_)) must_== Seq(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
    }
    "Exercise 2.2 (sorted array)" in {
      "array to test" || "expected" |
        Array(1, 2, 3, 4, 5) !! true |
        Array(1, 2, 3, 4) !! true |
        Array(1, 2, 4, 3) !! false |
        Array(10, 20, 30, 29) !! false |
        Array(10, 20, 30, 40, 39) !! false |
        Array(21, 20, 30, 40) !! false |
        Array(21, 20) !! false |
        Array(1, 1, 2, 2) !! true |
        Array(1, 1, 2) !! true |
        Array(0) !! true |
        Array() !! true |
        Array(1) !! true |> {
          (array, expected) => PolymorphicFunctions.isSorted(array, (a: Int, b: Int) => a > b) must_== expected
        }
    }

    "Exercise 2.3 (currying)" in {
      val intToFrench = Map(0 -> "zéro", 1 -> "un", 2 -> "deux", 3 -> "trois", 4 -> "quatre", 5 -> "cinq", 6 -> "six", 7 -> "sept", 8 -> "huit", 9 -> "neuf")
      val intToGerman = Map(0 -> "null", 1 -> "eins", 2 -> "zwei", 3 -> "drei", 4 -> "vier", 5 -> "fünf", 6 -> "sechs", 7 -> "sieben", 8 -> "acht", 9 -> "neun")
      def f(a: Map[Int, String], b: Int): Option[String] = a.get(b)
      def curry = PolymorphicFunctions.curry(f)
      def curryFrench = curry(intToFrench)
      def curryGerman = curry(intToGerman)
      (curryFrench(2) must_== Some("deux")) and (curryGerman(2) must_== Some("zwei")) and (curry(intToFrench)(-1) must_== None)
    }

    "Exercise 2.4 (uncurrying)" in {
      val intToFrench = Map(0 -> "zéro", 1 -> "un", 2 -> "deux", 3 -> "trois", 4 -> "quatre", 5 -> "cinq", 6 -> "six", 7 -> "sept", 8 -> "huit", 9 -> "neuf")
      val intToGerman = Map(0 -> "null", 1 -> "eins", 2 -> "zwei", 3 -> "drei", 4 -> "vier", 5 -> "fünf", 6 -> "sechs", 7 -> "sieben", 8 -> "acht", 9 -> "neun")

      def f(a: Map[Int, String]) = (b: Int) => a(b)
      def uncurry = PolymorphicFunctions.uncurry[Map[Int, String], Int, String](f)
      (uncurry(intToFrench, 2) must_== "deux") and (uncurry(intToGerman, 6) must_== "sechs")
    }

    "Exercise 2.5 (compose)" in {
      case class C(val st: String)
      val intToFrench = Map(0 -> "zéro", 1 -> "un", 2 -> "deux", 3 -> "trois", 4 -> "quatre", 5 -> "cinq", 6 -> "six", 7 -> "sept", 8 -> "huit", 9 -> "neuf")
      val intToGerman = Map("zéro" -> C("null"), "un" -> C("eins"), "deux" -> C("zwei"), "trois" -> C("drei"), "quatre" -> C("vier"), "cinq" -> C("fünf"), "six" -> C("sechs"), "sept" -> C("sieben"), "huit" -> C("acht"), "neuf" -> C("neun"))

      def f(b: String):C = intToGerman(b)
      def g(a:Int): String = intToFrench(a)
      def compose = PolymorphicFunctions.compose(f,g)
      (compose(1) must_== C("eins")) and (compose(9) must_== C("neun"))
    }

  }

}