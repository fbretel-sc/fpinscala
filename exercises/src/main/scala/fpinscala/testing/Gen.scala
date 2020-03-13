package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

package firstIteration {

  trait Prop {
    def check: Boolean

    // Exercise 8.3
    def &&(p: Prop): Prop = new Prop { def check: Boolean =  Prop.this.check && p.check }
  }

}

case class Prop(run: (TestCases,RNG) => Result) {

  def &&(p: Prop): Prop = Prop(
    (ts, rng) =>
      //    {
      //      val res1 = run(ts, rng)
      //      if (res1.isFalsified)
      //        res1
      //      else {
      //        val res2 = p.run(ts, rng)
      //        if (res2.isFalsified)
      //          res2
      //        else
      //          Passed
      //      }
      //    }
      run(ts, rng) match {
        case Passed => p.run(ts, rng)
        case falsified: Falsified => falsified
      }
  )

  def ||(p: Prop): Prop = Prop(
    (ts, rng) => run(ts, rng) match {
      case Passed => Passed
      case f1: Falsified => p.run(ts, rng) match {
        case Passed => Passed
        case Falsified(failure, successes) => Falsified(f1.failure + "\n" + failure, successes)
      }
    }
  )
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
//  type MaxSize = Int
  type TestCases = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

object Gen {
  val GenListSize = 100

  // Exercice8.4
  /** Generates an integer in the range start to stopExclusive. */
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
  //    Gen[Int](State((rng: RNG) => {
  //      val (d, rng2) = RNG.double(rng)
  //      val res = (d * (stopExclusive - start)).toInt + start
  //      (res, rng2)
  //    }))
    Gen(State(RNG.double).map(d => (d * (stopExclusive - start)).toInt + start))

  // Exercise 8.5: unit, boolean, listOfN
  def unit[A](a: => A): Gen[A] = Gen(State((a, _))) // better: Gen(State.unit(a))

  def boolean: Gen[Boolean] =
  // Ok but a little bit contrieved:
  // Gen(State(RNG.nonNegativeLessThan(2)).map(i => if (i == 1) true else false))
  // Note also we should move the logic to State.boolean().
    Gen(State(RNG.int(_) match { case (i, rng2) => (i%2==0, rng2) }))

  def listOf[A](a: Gen[A]): Gen[List[A]] = listOfN(GenListSize, a)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State { rng =>
      (1 to n).foldLeft((List[A](), rng))((acc, _) => acc match {
        case (l, r) =>
          val (a, rng2) = g.sample.run(r)
          (a :: l, rng2)
      }) match { case (l, r) => (l.reverse, r)}
    })
    // better: Gen(State.sequence(List.fill(n)(g.sample)))

  // Exercice 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = Gen.boolean.flatMap(if (_) g1 else g2)

  // Exercice 8.8
  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] =
    // Wrong: Gen(State(RNG.double)) flatMap { d => if (d < g1._2/(g1._2 + g2._2)) g1._1 else g2._1 }
    Gen(State(RNG.double)) flatMap { d => if (d < g1._2.abs/(g1._2.abs + g2._2.abs)) g1._1 else g2._1 }
}

case class Gen[+A](sample: State[RNG,A]) {
  // Exercice 8.6
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen.listOfN(n, Gen(sample)))
}

trait SGen[+A] {

}

