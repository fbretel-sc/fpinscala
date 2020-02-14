package fpinscala.errorhandling

import org.specs2.mutable._
import org.specs2.specification.AllExpectations
import org.specs2.matcher.DataTables
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class OptionSpec extends Specification with DataTables {

  "The following exercises should be correct" >> {

    "Exercise 4.1: map for Option." in {
      def f(i: Int) = i.toString
      (None.map(f) must_== None) and
        (Some(10).map(f) must_== Some("10"))
    }

    "Exercise 4.1: flatMap for Option (without pattern matching)." in {
      def f(i: Int): Option[String] = if (i == 0) None else Some(i.toString)
      (None.flatMap(f) must_== None) and
        (Some(0).flatMap(f) must_== None) and
        (Some(1).flatMap(f) must_== Some("1"))
    }

    "Exercise 4.1: flatMap for Option (with pattern matching)." in {
      def f(i: Int): Option[String] = if (i == 0) None else Some(i.toString)
      (None.flatMap_1(f) must_== None) and
        (Some(0).flatMap_1(f) must_== None) and
        (Some(1).flatMap_1(f) must_== Some("1"))
    }

    "Exercise 4.1: getOrElse for Option." in {
      val default = 42
      (None.getOrElse(default) must_== 42) and
        (Some(10).getOrElse(default) must_== 10)
    }

    "Exercise 4.1: orElse for Option (without pattern matching)." in {
      "option1" || "option2" || "result" |
        None !! None !! None |
        Some(1) !! Some(2) !! Some(1) |
        None !! Some(2) !! Some(2) |
        Some(1) !! None !! Some(1) |> {
          (opt1, opt2, res) => (opt1.orElse(opt2) must_== res)
        }
    }

    "Exercise 4.1: orElse for Option (with pattern matching)." in {
      "option1" || "option2" || "result" |
        None !! None !! None |
        Some(1) !! Some(2) !! Some(1) |
        None !! Some(2) !! Some(2) |
        Some(1) !! None !! Some(1) |> {
          (opt1, opt2, res) => (opt1.orElse_1(opt2) must_== res)
        }
    }

    "Exercise 4.1: filter for Option (with pattern matching)." in {
      def f(i: Int) = (i != 0)
      (None.filter(f) must_== None) and
        (Some(0).filter(f) must_== None) and
        (Some(1).filter(f) must_== Some(1))
    }

    "Exercise 4.1: filter for Option (without pattern matching)." in {
      def f(i: Int) = (i != 0)
      (None.filter_1(f) must_== None) and
        (Some(0).filter_1(f) must_== None) and
        (Some(1).filter_1(f) must_== Some(1))
    }

    "Exercise 4.2: variance." in {
      "option" || "result" |
        List[Double]() !! None |
        List[Double](1, 2, 3, 4) !! Some(1.25) |> {
          (xs, res) =>
            {
              Option.variance(xs) must_== res
            }
        }
    }

    "Exercise 4.3: map2." in {
      "a" || "b" || "res" |
        Some(1) !! Some(2) !! Some(3) |
        None !! Some(2) !! None |
        Some(1) !! None !! None |
        None !! None !! None |> {
          (a, b, res) =>
            {
              def f(a: Int, b: Int) = a + b
              (Option.map2(a, b)(f) must_== res)
            }
        }
    }

    "Exercise 4.4: sequence." in {
      "list" || "res" |
        List(Some(1), Some(2), Some(3), Some(4)) !! Some(List(1, 2, 3, 4)) |
        List(Some(1)) !! Some(List(1)) |
        List() !! Some(List()) |
        List(None, Some(2), Some(3), Some(4)) !! None |
        List(Some(2), Some(3), Some(4), None) !! None |
        List(Some(2), Some(3), None, Some(4)) !! None |> {
          (list, res) => Option.sequence(list) must_== res
        }
    }

    "Exercise 4.5: traverse (straightforward)." in {
      "list" || "res" |
        List(0, 2, 4, 6) !! Some(List("0", "2", "4", "6")) |
        List(1, 2, 4, 6) !! None |
        List(0, 2, 3, 4, 6) !! None |
        List(0, 2, 4, 6, 7) !! None |
        List(1) !! None |
        List() !! Some(List()) |
        List(2) !! Some(List("2")) |> {
          (list, res) =>
            {
              def f(i: Int): Option[String] = if (i % 2 == 0) Some(i.toString) else None

              (Option.traverse[Int, String](list)(f) must_== res)
            }
        }
    }

    "Exercise 4.5: traverse (implementation that only looks at the sequence once)." in {
      "list" || "res" |
        List(0, 2, 4, 6) !! Some(List("0", "2", "4", "6")) |
        List(1, 2, 4, 6) !! None |
        List(0, 2, 3, 4, 6) !! None |
        List(0, 2, 4, 6, 7) !! None |
        List(1) !! None |
        List() !! Some(List()) |
        List(2) !! Some(List("2")) |> {
          (list, res) =>
            {
              def f(i: Int): Option[String] = if (i % 2 == 0) Some(i.toString) else None

              (Option.traverse_1[Int, String](list)(f) must_== res)
            }
        }
    }

    "Exercise 4.5: sequence in terms of traverse." in {
      "list" || "res" |
        List(Some(1), Some(2), Some(3), Some(4)) !! Some(List(1, 2, 3, 4)) |
        List(Some(1)) !! Some(List(1)) |
        List() !! Some(List()) |
        List(None, Some(2), Some(3), Some(4)) !! None |
        List(Some(2), Some(3), Some(4), None) !! None |
        List(Some(2), Some(3), None, Some(4)) !! None |> {
          (list, res) =>
            {
              Option.sequenceViaTraverse(list) must_== res
            }
        }
    }

  }
}