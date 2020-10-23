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

package secondIteration {

  case class Prop(run: (TestCases,RNG) => Result) {

    // Exercice 8.9
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
          case Passed | Proved => p.run(ts, rng)
          case falsified: Falsified => falsified
        }
    )

    // Exercice 8.9
    def ||(p: Prop): Prop = Prop(
      (ts, rng) => run(ts, rng) match {
        case f1: Falsified => p.run(ts, rng) match {
          case Falsified(failure, successes) => Falsified(f1.failure + "\n" + failure, successes)
          case x => x
        }
        case x => x
      }
    )
  }

}

case class Prop(run: (MaxSize,TestCases,RNG) => Result) {

  def &&(p: Prop): Prop = Prop(
    (max, ts, rng) =>
      run(max, ts, rng) match {
        case Passed | Proved => p.run(max, ts, rng)
        case falsified: Falsified => falsified
      }
  )

  def ||(p: Prop): Prop = Prop(
    (max, ts, rng) => run(max, ts, rng) match {
      case f1: Falsified => p.run(max, ts, rng) match {
        case Falsified(failure, successes) => Falsified(f1.failure + "\n" + failure, successes)
        case x => x
      }
      case x => x
    }
  )
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type MaxSize = Int
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
  case object Proved extends Result {
    def isFalsified = false
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def apply(f: (TestCases,RNG) => Result): Prop =
    Prop { (_,n,rng) => f(n,rng) }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop =
    Prop {
      (max,n,rng) =>
        val casesPerSize = (n + (max - 1)) / max
        val props: Stream[Prop] =
          Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
        val prop: Prop =
          props.map(p => Prop { (max, _, rng) =>
            p.run(max, casesPerSize, rng)
          }).toList.reduce(_ && _)
        prop.run(max,n,rng)
    }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case f: Falsified =>
        println(s"! Falsified after ${f.successes} passed tests:\n ${f.failure}")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed else Falsified("()", 0)
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p,p2)(_ == _)

  val S: Gen[ExecutorService] = weighted(
    choose(1,4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25) // `a -> b` is syntax sugar for `(a,b)`

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S.map2(g)((_,_))) { case (s,a) => f(a)(s).get }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  def forAllPar2[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case (s,a) => f(a)(s).get }

  def forAllPar3[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get }
}

object Gen {

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

  // Exercice 8.12
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => listOfN(n, g))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State { rng =>
      (1 to n).foldLeft((List[A](), rng))((acc, _) => acc match {
        case (l, r) =>
          val (a, rng2) = g.sample.run(r)
          (a :: l, rng2)
      }) match { case (l, r) => (l.reverse, r)}
    })
    // better: Gen(State.sequence(List.fill(n)(g.sample)))

  // Exercice 8.13 - Define listOf1 for generating nonempty lists
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => listOfN(n max 1, g))

  // Exercice 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = Gen.boolean.flatMap(if (_) g1 else g2)

  // Exercice 8.8
  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] =
    // Wrong: Gen(State(RNG.double)) flatMap { d => if (d < g1._2/(g1._2 + g2._2)) g1._1 else g2._1 }
    Gen(State(RNG.double)) flatMap { d => if (d < g1._2.abs/(g1._2.abs + g2._2.abs)) g1._1 else g2._1 }

  // Custom extractor
  object ** {
    def unapply[A,B](p: (A,B)) = Some(p)
  }

  // Exercice 8.19 - COPIED FROM ANSWERS
  def genStringFn[A](g: Gen[A]): Gen[String => A] = Gen {
    State { (rng: RNG) =>
      val (seed, rng2) = rng.nextInt // we still use `rng` to produce a seed, so we get a new function each time
      // By using hashCode(), we use the whole information of the String. Which would not be the case with length().
      val f = (s: String) => g.sample.run(RNG.Simple(seed.toLong ^ s.hashCode.toLong))._1
      (f, rng2)
    }
  }

}

case class Gen[+A](sample: State[RNG,A]) {
  // Exercice 8.6
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen.listOfN(n, Gen(sample)))

  // Exercice 8.10
  def unsized: SGen[A] = SGen(_ => this)

  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def **[B](g: Gen[B]): Gen[(A,B)] =
    (this map2 g)((_,_))
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  // Exercice 8.11
  def map[B](f: A => B): SGen[B] = SGen(n => forSize(n).map(f))
  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(n => forSize(n).flatMap(a => f(a).forSize(n)))
}
