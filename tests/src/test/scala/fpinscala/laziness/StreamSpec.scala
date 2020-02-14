package fpinscala.laziness

import org.specs2.mutable._
import org.specs2.specification.AllExpectations
import org.specs2.matcher.DataTables
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import fpinscala.laziness.Stream.{ cons, empty }

@RunWith(classOf[JUnitRunner])
class StreamSpec extends Specification with DataTables {

  "The following exercises should be correct" >> {

    "Exercise 5.1: toList." in {
      "stream" || "list" |
        Stream.empty !! Nil |
        cons("a", Stream.empty) !! List("a") |
        cons("a", cons("b", Stream.empty)) !! List("a", "b") |
        cons("a", cons("b", cons("c", Stream.empty))) !! List("a", "b", "c") |> {
          (stream, list) =>
            (stream.toList must_== list)
        }
    }

    "Exercise 5.2: take." in {
      "stream" || "n" || "result" |
        Stream.empty !! 0 !! Stream.empty |
        Stream.empty !! 1 !! Stream.empty |
        cons("a", Stream.empty) !! 0 !! Stream.empty |
        cons("a", Stream.empty) !! 1 !! cons("a", Stream.empty) |
        cons("a", Stream.empty) !! 2 !! cons("a", Stream.empty) |
        cons("a", cons("b", cons("c", Stream.empty))) !! 0 !! Stream.empty |
        cons("a", cons("b", cons("c", Stream.empty))) !! 1 !! cons("a", Stream.empty) |
        cons("a", cons("b", cons("c", Stream.empty))) !! 2 !! cons("a", cons("b", Stream.empty)) |
        cons("a", cons("b", cons("c", Stream.empty))) !! 3 !! cons("a", cons("b", cons("c", Stream.empty))) |
        cons("a", cons("b", cons("c", Stream.empty))) !! 4 !! cons("a", cons("b", cons("c", Stream.empty))) |> {
          (stream, n, result) =>
            (stream.take(n).toList must_== result.toList)
        }
    }

    "Exercise 5.2: drop." in {
      "stream" || "n" || "result" |
        Stream.empty !! 0 !! Stream.empty |
        Stream.empty !! 1 !! Stream.empty |
        cons("a", Stream.empty) !! 0 !! cons("a", Stream.empty) |
        cons("a", Stream.empty) !! 1 !! Stream.empty |
        cons("a", Stream.empty) !! 2 !! Stream.empty |
        cons("a", cons("b", cons("c", Stream.empty))) !! 0 !! cons("a", cons("b", cons("c", Stream.empty))) |
        cons("a", cons("b", cons("c", Stream.empty))) !! 1 !! cons("b", cons("c", Stream.empty)) |
        cons("a", cons("b", cons("c", Stream.empty))) !! 2 !! cons("c", Stream.empty) |
        cons("a", cons("b", cons("c", Stream.empty))) !! 3 !! Stream.empty |
        cons("a", cons("b", cons("c", Stream.empty))) !! 4 !! Stream.empty |> {
          (stream, n, result) =>
            (stream.drop(n).toList must_== result.toList)
        }
    }

    "Exercise 5.3: takeWhile." in {
      "stream" || "result" |
        Stream.empty !! Nil |
        cons("a", Stream.empty) !! List("a") |
        cons("a", cons("b", Stream.empty)) !! List("a", "b") |
        cons("a", cons("b", cons("c", Stream.empty))) !! List("a", "b") |
        cons("c", cons("a", cons("b", Stream.empty))) !! Nil |
        cons("a", cons("c", cons("b", Stream.empty))) !! List("a") |> {
          (stream, result) =>
            def p(s: String) = (s == "a") || (s == "b")
            stream.takeWhile(p).toList must_== result
        }
    }

    "Exercise 5.4: forAll." in {
      "stream" || "result" |
        Stream.empty !! true |
        cons("a", Stream.empty) !! true |
        cons("z", Stream.empty) !! false |
        cons("a", cons("b", Stream.empty)) !! true |
        cons("a", cons("b", cons("c", Stream.empty))) !! true |
        cons("a", cons("b", cons("z", Stream.empty))) !! false |
        cons("z", cons("b", cons("a", Stream.empty))) !! false |> {
          (stream, result) =>
            def p(s: String) = (s != "z")
            stream.forAll(p) must_== result
        }
    }

    "Exercise 5.5: takeWhile via foldRight." in {
      "stream" || "result" |
        Stream.empty !! Nil |
        cons("a", Stream.empty) !! List("a") |
        cons("a", cons("b", Stream.empty)) !! List("a", "b") |
        cons("a", cons("a", cons("b", Stream.empty))) !! List("a", "a", "b") |
        cons("a", cons("a", cons("c", cons("b", Stream.empty)))) !! List("a", "a") |
        cons("a", cons("b", cons("c", Stream.empty))) !! List("a", "b") |
        cons("c", cons("a", cons("b", Stream.empty))) !! Nil |
        cons("a", cons("c", cons("b", Stream.empty))) !! List("a") |> {
          (stream, result) =>
            def p(s: String) = (s == "a") || (s == "b")
            stream.takeWhileViaFoldRight(p).toList must_== result
        }
    }

    "Exercise 5.6: headOption Via foldRight." in {
      "stream" || "result" |
        Stream.empty !! None |
        cons("a", Stream.empty) !! Some("a") |
        cons("a", cons("b", Stream.empty)) !! Some("a") |
        cons("a", cons("a", cons("b", Stream.empty))) !! Some("a") |> {
          (stream, result) =>
            stream.headOption must_== result
        }
    }

    "Exercise 5.7: map." in {
      "stream" || "result" |
        Stream.empty !! Nil |
        cons(1, Stream.empty) !! List("1") |
        cons(2, cons(1, Stream.empty)) !! List("2", "1") |
        cons(3, cons(2, cons(1, Stream.empty))) !! List("3", "2", "1") |> {
          (stream, result) =>
            stream.map(_.toString).toList must_== result
        }
    }

    "Exercise 5.7: filter." in {
      "stream" || "result" |
        Stream.empty !! Nil |
        cons("a", Stream.empty) !! List("a") |
        cons("a", cons("b", Stream.empty)) !! List("a", "b") |
        cons("a", cons("a", cons("b", Stream.empty))) !! List("a", "a", "b") |
        cons("a", cons("c", cons("a", cons("b", Stream.empty)))) !! List("a", "a", "b") |
        cons("c", cons("c", cons("b", Stream.empty))) !! List("b") |
        cons("c", cons("d", cons("e", Stream.empty))) !! Nil |
        cons("a", cons("c", cons("b", Stream.empty))) !! List("a", "b") |> {
          (stream, result) =>
            def p(s: String) = (s == "a") || (s == "b")
            stream.filter(p).toList must_== result
        }
    }

    "Exercise 5.7: append." in {
      "stream1" || "stream2" || "result" |
        Stream.empty !! Stream.empty !! Nil |
        cons("a", Stream.empty) !! Stream.empty !! List("a") |
        cons("a", cons("b", Stream.empty)) !! Stream.empty !! List("a", "b") |
        Stream.empty !! cons("a", cons("b", cons("c", Stream.empty))) !! List("a", "b", "c") |
        Stream.empty !! cons("a", Stream.empty) !! List("a") |
        cons("a", cons("b", Stream.empty)) !! cons("c", cons("d", Stream.empty)) !! List("a", "b", "c", "d") |
        cons("a", Stream.empty) !! cons("b", Stream.empty) !! List("a", "b") |> {
          (stream1, stream2, result) =>
            stream1.append(stream2).toList must_== result
        }
    }

    "Exercise 5.7: flatMap." in {
      "stream" || "result" |
        Stream.empty !! Nil |
        cons('a', Stream.empty) !! List("a", "b", "c") |
        cons('a', cons('m', Stream.empty)) !! List("a", "b", "c", "m", "n", "o") |
        cons('a', cons('m', cons('s', Stream.empty))) !! List("a", "b", "c", "m", "n", "o", "s", "t", "u") |> {
          (stream, result) =>
            def f(s: Char): Stream[String] = cons(s.toString, cons((s + 1).toChar.toString, cons((s + 2).toChar.toString, Stream.empty)))
            stream.flatMap(f).toList must_== result
        }
    }

    "Exercise 5.8: constant." in {
      Stream.constant(5).take(10).toList must_== List(5, 5, 5, 5, 5, 5, 5, 5, 5, 5)
    }

    "Exercise 5.9: from." in {
      Stream.from(5).take(10).toList must_== List(5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
    }

    "Exercise 5.10: fibs." in {
      Stream.fibs.take(10).toList must_== List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    }

    "Exercise 5.11: unfold." in {
      type State = Char
      def f(state: State): Option[(String, State)] = {
        if (state == 'f') None
        else Some((state + ":" + state, (state + 1).toChar))
      }

      Stream.unfold('a')(f).take(2).toList must_== List("a:a", "b:b")
      Stream.unfold('a')(f).toList must_== List("a:a", "b:b", "c:c", "d:d", "e:e")
    }

    "Exercise 5.12: fibs, from, constant , and ones in terms of unfold." in {
      (Stream.fibsViaUnfold.take(10).toList must_== List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)) and
        (Stream.fromViaUnfold(5).take(10).toList must_== List(5, 6, 7, 8, 9, 10, 11, 12, 13, 14)) and
        (Stream.constantViaUnfold(5).take(10).toList must_== List(5, 5, 5, 5, 5, 5, 5, 5, 5, 5)) and
        (Stream.onesViaUnfold.take(10).toList must_== List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
    }

    "Exercise 5.13: map via unfold." in {
      "Stream" || "result" |
        Stream.empty !! Nil |
        cons(41.99, Stream.empty) !! List("41.99") |
        cons(4.4, cons(3.3, cons(2.2, cons(1.1, Stream.empty)))) !! List("4.4", "3.3", "2.2", "1.1") |> {
          (stream, result) =>
            {
              def f(i: Double) = i.toString
              stream.mapViaUnfold(f).toList must_== result
            }
        }
    }

    "Exercise 5.13: take via unfold." in {
      "stream" || "n" || "result" |
        Stream.empty !! 0 !! Stream.empty |
        Stream.empty !! 1 !! Stream.empty |
        cons("a", Stream.empty) !! 0 !! Stream.empty |
        cons("a", Stream.empty) !! 1 !! cons("a", Stream.empty) |
        cons("a", Stream.empty) !! 2 !! cons("a", Stream.empty) |
        cons("a", cons("b", cons("c", Stream.empty))) !! 0 !! Stream.empty |
        cons("a", cons("b", cons("c", Stream.empty))) !! 1 !! cons("a", Stream.empty) |
        cons("a", cons("b", cons("c", Stream.empty))) !! 3 !! cons("a", cons("b", cons("c", Stream.empty))) |
        cons("a", cons("b", cons("c", Stream.empty))) !! 4 !! cons("a", cons("b", cons("c", Stream.empty))) |
        cons("a", cons("b", cons("c", Stream.empty))) !! 2 !! cons("a", cons("b", Stream.empty)) |> {
          (stream, n, result) =>
            (stream.takeViaUnfold(n).toList must_== result.toList)
        }
    }

    "Exercise 5.13: takeWhile via unfold." in {
      "stream" || "result" |
        Stream.empty !! Nil |
        cons("a", Stream.empty) !! List("a") |
        cons("a", cons("b", Stream.empty)) !! List("a", "b") |
        cons("a", cons("b", cons("c", Stream.empty))) !! List("a", "b") |
        cons("c", cons("a", cons("b", Stream.empty))) !! Nil |
        cons("a", cons("c", cons("b", Stream.empty))) !! List("a") |> {
          (stream, result) =>
            def p(s: String) = (s == "a") || (s == "b")
            stream.takeWhileViaUnfold(p).toList must_== result
        }
    }

    "Exercise 5.13: zipWith via unfold." in {
      "stream1" || "stream2" || "result" |
        Stream.empty !! Stream.empty !! List() |
        cons('a', Stream.empty) !! cons(1, Stream.empty) !! List("a:1") |
        cons('a', cons('b', Stream.empty)) !! cons(1, cons(2, Stream.empty)) !! List("a:1", "b:2") |
        cons('a', cons('b', cons('c', Stream.empty))) !! Stream.empty !! List() |
        Stream.empty !! cons(1, cons(2, cons(3, Stream.empty))) !! List() |
        cons('a', cons('b', cons('c', Stream.empty))) !! cons(1, cons(2, Stream.empty)) !! List("a:1", "b:2") |
        cons('C', cons('D', Stream.empty)) !! cons(3, cons(4, cons(5, Stream.empty))) !! List("C:3", "D:4") |
        cons('e', cons('f', cons('g', Stream.empty))) !! cons(5, cons(6, cons(7, Stream.empty))) !! List("e:5", "f:6", "g:7") |> {
          (stream1, stream2, result) =>
            def f(a: Char, b: Int) = a.toString + ":" + b.toString
            stream1.zipWith(stream2)(f).toList must_== result
        }
    }

    "Exercise 5.13: zipAll Via unfold." in {
      "stream1" || "stream2" || "result" |
        Stream.empty !! Stream.empty !! Nil |
        cons('a', Stream.empty) !! cons(1, Stream.empty) !! List((Some('a'), Some(1))) |
        cons('a', cons('b', Stream.empty)) !! cons(1, cons(2, Stream.empty)) !! List((Some('a'), Some(1)), (Some('b'), Some(2))) |
        cons('a', cons('b', cons('c', Stream.empty))) !! Stream.empty !! List((Some('a'), None), (Some('b'), None), (Some('c'), None)) |
        Stream.empty !! cons(1, cons(2, cons(3, Stream.empty))) !! List((None, Some(1)), (None, Some(2)), (None, Some(3))) |
        cons('a', cons('b', cons('c', Stream.empty))) !! cons(1, cons(2, Stream.empty)) !! List((Some('a'), Some(1)), (Some('b'), Some(2)), (Some('c'), None)) |
        cons('C', cons('D', Stream.empty)) !! cons(3, cons(4, cons(5, Stream.empty))) !! List((Some('C'), Some(3)), (Some('D'), Some(4)), (None, Some(5))) |
        cons('e', cons('f', cons('g', Stream.empty))) !! cons(5, cons(6, cons(7, Stream.empty))) !! List((Some('e'), Some(5)), (Some('f'), Some(6)), (Some('g'), Some(7))) |> {
          (stream1, stream2, result) =>
            stream1.zipAll(stream2).toList must_== result
        }
    }

    "Exercise 5.14: startWith." in {
      "stream1" || "prefix" || "result" |
        Stream.empty !! Stream.empty !! true |
        cons("a", Stream.empty) !! cons("a", Stream.empty) !! true |
        cons("a", cons("b", Stream.empty)) !! cons("a", Stream.empty) !! true |
        cons("a", cons("b", cons("c", cons("d", Stream.empty)))) !! cons("a", cons("b", cons("c", Stream.empty))) !! true |
        cons("a", Stream.empty) !! Stream.empty !! true |
        Stream.empty !! cons("a", Stream.empty) !! false |
        cons("a", cons("b", Stream.empty)) !! cons("b", Stream.empty) !! false |
        cons("a", cons("b", Stream.empty)) !! cons("a", cons("c", Stream.empty)) !! false |
        cons("a", cons("b", Stream.empty)) !! cons("a", cons("b", cons("c", Stream.empty))) !! false |> {
          (stream, prefix, result) =>
            stream.startsWith(prefix) must_== result
        }
    }

    "Exercise 5.15: tails via unfold." in {
      "stream" || "result" |
        Stream() !! Stream(Stream()) |
        Stream('a') !! Stream(Stream('a'), Stream()) |
        Stream('a', 'b') !! Stream(Stream('a', 'b'), Stream('b'), Stream()) |
        Stream('a', 'b', 'c') !! Stream(Stream('a', 'b', 'c'), Stream('b', 'c'), Stream('c'), Stream()) |
        Stream('a', 'b', 'c', 'd') !! Stream(Stream('a', 'b', 'c', 'd'), Stream('b', 'c', 'd'), Stream('c', 'd'), Stream('d'), Stream()) |> {
          (stream, result) =>
            stream.tails.toList.map(_.toList) must_== result.toList.map(_.toList)
        }
    }
    
    "Exercise 5.16 (scanRight)" in {
      "stream" || "result" |
        Stream.empty !! List("!") |
        cons('a', Stream.empty) !! List("a!", "!") |
        cons('a', cons('b', Stream.empty)) !! List("ab!", "b!", "!") |
        cons('a', cons('b', cons('c', Stream.empty))) !! List("abc!", "bc!", "c!", "!") |
        cons('a', cons('b', cons('c', cons('d', Stream.empty)))) !! List("abcd!", "bcd!", "cd!", "d!", "!") |> {
          (stream, result) =>
            def f(a: Char, b: => String): String = a.toString + b
            stream.scanRight("!")(f).toList must_== result
        }
    }

  }
}