package fpinscala.errorhandling

import org.specs2.mutable._
import org.specs2.specification.AllExpectations
import org.specs2.matcher.DataTables
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class EitherSpec extends Specification with DataTables {

  "The following exercises should be correct" >> {

    "Exercise 4.6: map." in {
      "either" || "result" |
        Left("left") !! Left("left") |
        Right(3) !! Right(1.5) |
        Right(1) !! Right(0.5) |> {
          (either, res) =>
            {
              def f(i: Int): Double = i.toDouble / 2
              either.map(f) must_== res
            }
        }
    }

    "Exercise 4.6: flatMap." in {
      "either" || "result" |
        Left("left") !! Left("left") |
        Right(3) !! Left("not divisible per 2") |
        Right(2) !! Right(1) |> {
          (either, res) =>
            {
              def f(i: Int): Either[String, Double] = if (i % 2 == 0) Right(i.toDouble / 2) else Left("not divisible per 2")
              either.flatMap(f) must_== res
            }
        }
    }

    "Exercise 4.6: orElse." in {
      "either" || "result" |
        Right(1) !! Right(1) |
        Left("left") !! Right(0) |> {
          (either, res) =>
            {
              val b = Right(0)
              either.orElse(b) must_== res
            }
        }
    }

    "Exercise 4.6: map2." in {
      "e1" || "e2" || "res" |
        Left("invalid int") !! Right('a') !! Left("invalid int") |
        Right(1) !! Left("invalid char") !! Left("invalid char") |
        Right(1) !! Right('a') !! Right("1a") |> {
          (e1, e2, res) =>
            {
              def f(a: Int, b: Char): String = a.toString + b
              e1.map2(e2)(f) must_== res
            }
        }
    }

    "Exercise 4.7: traverse." in {
      "list" || "res" |
        List(0, 2, 4, 6) !! Right(List(0, 1, 2, 3)) |
        List(1, 2, 4, 6) !! Left("error") |
        List(0, 2, 3, 4, 6) !! Left("error") |
        List(0, 2, 4, 6, 7) !! Left("error") |
        List(1) !! Left("error") |
        List() !! Right(List()) |
        List(2) !! Right(List(1)) |> {
          (list, res) =>
            {
              def f(i: Int): Either[String, Double] = if (i % 2 == 0) Right(i.toDouble / 2) else Left("error")
              Either.traverse[String, Int, Double](list)(f) must_== res
            }
        }
    }

    "Exercise 4.7: sequence." in {
      "list" || "res" |
        List(Right(1), Right(2), Right(3), Right(4)) !! Right(List(1, 2, 3, 4)) |
        List(Right(1)) !! Right(List(1)) |
        List() !! Right(List()) |
        List(Left("error"), Right(2), Right(3), Right(4)) !! Left("error") |
        List(Right(2), Right(3), Right(4), Left("error")) !! Left("error") |
        List(Right(2), Right(3), Left("error"), Right(4)) !! Left("error") |> {
          (list, res) =>
            {
              Either.sequence(list) must_== res
            }
        }
    }
  }
}