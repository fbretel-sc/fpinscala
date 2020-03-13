package fpinscala.testing

import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.matcher.DataTables
import fpinscala.state.RNG
import fpinscala.state.State

@RunWith(classOf[JUnitRunner])
class GenSpec extends Specification with DataTables {

  val seedInt = 340707234

  type Rand[+A] = RNG => (A, RNG)

  /**
   * Generate a list of 'count' results.
   */
  private def sequenceFromTest[A](count: Int)(rng: RNG)(f: Rand[A]): (List[A], RNG) = {
    @annotation.tailrec
    def go(c: Int, acc: List[A], rng: RNG): (List[A], RNG) =
      if (c == 0)
        (acc, rng)
      else {
        val (a, r) = f(rng)
        go(c - 1, a :: acc, r)
      }
    go(count, List(), rng)
  }

  val seed = RNG.Simple(seedInt)

  "The following exercises should be correct" >> {

    "Exercise 8.3: &&" in {
      import fpinscala.testing.firstIteration.Prop
      val propFalse = new Prop { def check = false }
      val propTrue = new Prop { def check = true }

      "propA" || "proB" || "expected result" |
        propFalse !! propFalse !! false |
        propFalse !! propTrue !! false |
        propTrue !! propFalse !! false |
        propTrue !! propTrue !! true |> {
        (propA, propB, expected) =>
          (propA && propB).check must_== expected
      }
    }

    "Exercise 8.4: choose" in {
      import java.lang.Integer.{ MIN_VALUE, MAX_VALUE }
      // NB - values tight to PRNG and choose() implementations, hereafter adapted for my needs
      "start" || "stopExclusive" || "expected" |
        -2 !! 2 !! List(-1, 0, 0, -2) |
        MIN_VALUE !! MIN_VALUE + 2 !! List(-2147483648, -2147483647, -2147483647, -2147483648) |
        MAX_VALUE - 2 !! MAX_VALUE !! List(2147483645, 2147483646, 2147483646, 2147483645) |> {
          (start, stopExclusive, expected) =>
            val gen = Gen.choose(start, stopExclusive)
            val (ints1, rng1) = sequenceFromTest(4)(seed)(gen.sample.run)
            val ints2 = sequenceFromTest(1000)(rng1)(gen.sample.run)._1
            (ints1 must_== expected) and
              (ints2 forall (elt => elt >= start && elt < stopExclusive) must beTrue)
        }
    }

    "Exercise 8.5: unit" in {
      val gen = Gen.unit("unit")
      val (units1, rng) = sequenceFromTest(10)(seed)(gen.sample.run)
      val units2 = sequenceFromTest(10)(rng)(gen.sample.run)._1
      units1.union(units2) forall (_ == "unit") must beTrue
    }

    "Exercise 8.5: boolean" in {
      val gen = Gen.boolean
      val (booleans1, rng) = sequenceFromTest(3)(seed)(gen.sample.run)
      val booleans2 = sequenceFromTest(4)(rng)(gen.sample.run)._1
      (booleans1 must_== List(false, true, true)) and
        (booleans2 must_== List(true, true, false, false))
    }

    "Exercise 8.5: listOfN" in {
      val start = -2
      val stopExclusive = 2
      val listSize = 100
      val gen = Gen.listOfN(listSize, Gen.choose(start, stopExclusive))
      val (ints1, rng) = gen.sample.run(seed)
      val ints2 = gen.sample.run(rng)._1
      val ints = ints1 union ints2
      (ints must haveSize(listSize * 2)) and
        (ints.filter(_ < start) must beEmpty) and
        (ints.filter(_ >= stopExclusive) must beEmpty)
    }

    "Exercise 8.6: flatMap" in {
      val nbLetterGen = Gen.choose(1, 10)
      val f = (nbInts: Int) => Gen.listOfN(nbInts, Gen.choose(10, 15))
      val wordGen = nbLetterGen.flatMap(f)
      val (res1, rng1) = wordGen.sample.run(seed)
      val (res2, rng2) = wordGen.sample.run(rng1)
      val res3 = wordGen.sample.run(rng2)._1
      (res1 must_== List(13)) and
        (res2 must_== List(12, 13, 11, 10, 14)) and
        (res3 must_== List(13, 11))
    }

    "Exercise 8.6: listOfN" in {
      val booleanGen = Gen.boolean
      val gen = booleanGen.listOfN(Gen.choose(0, 5))
      val seed = RNG.Simple(seedInt)
      val lists = sequenceFromTest(6)(seed)(gen.sample.run)._1
      lists must_== List(List(true, true, false, false), List(false, false, false), List(false, false, false, true), List(true), List(false, false, false), List())
    }

    "Exercise 8.7: union" in {
      val nbTests = 1000
      val genF = Gen.unit(false)
      val genT = Gen.unit(true)
      val gen = Gen.union(genF, genT)
      val booleans = sequenceFromTest(nbTests)(seed)(gen.sample.run)._1
      val percentError = 2
      val margin = nbTests * percentError / 100
      val expected = nbTests / 2
      (booleans.count(_ == false) must beGreaterThan(expected - margin)) and
        (booleans.count(_ == true) must beGreaterThan(expected - margin))
    }

    "Exercise 8.8: weighted" in {
      "weightFalse" || "weightTrue" |
        1 !! 1 |
        2 !! 1 |
        1 !! 3 |
        1 !! 3 |
        1 !! 10 |
        100 !! 1 |
        1 !! 0 |
        0 !! 1 |
        10 !! 10 |> {
        (falseWeight, trueWeight) =>
          val nbTests = 1000
          val percentError = 3
          val margin = nbTests * percentError / 100
          val falseGen = Gen.unit(false)
          val trueGen = Gen.unit(true)
          val weightedGen = Gen.weighted((falseGen, falseWeight), (trueGen, trueWeight))
          val booleans = sequenceFromTest(nbTests)(seed)(weightedGen.sample.run)._1
          val falseNb = booleans.count(_ == false)
          val trueNb = booleans.count(_ == true)
          val expectedFalseNb = falseWeight * nbTests / (falseWeight + trueWeight)
          val expectedTrueNb = trueWeight * nbTests / (falseWeight + trueWeight)

          (falseNb must beGreaterThan(expectedFalseNb - margin)) and
            (falseNb must beLessThan(expectedFalseNb + margin)) and
            (trueNb must beGreaterThan(expectedTrueNb - margin)) and
            (trueNb must beLessThan(expectedTrueNb + margin))
      }
    }

    /*
    "Exercise 8.9: &&" in {
          import fpinscala.testing.Prop._
          def runFalsified1(maxSize: MaxSize, testCases: TestCases, rng: RNG): Result = Falsified("test1 ko", 1)
          def runFalsified2(maxSize: MaxSize, testCases: TestCases, rng: RNG): Result = Falsified("test2 ko", 2)
          def runPassed(maxSize: MaxSize, testCases: TestCases, rng: RNG): Result = Passed

          val propFalse1 = new Prop(runFalsified1)
          val propFalse2 = new Prop(runFalsified2)
          val propTrue = new Prop(runPassed)

          "propA" || "proB" || "expected result" |
            propFalse1 !! propFalse2 !! propFalse1 |
            propFalse2 !! propFalse1 !! propFalse2 |
            propFalse1 !! propTrue !! propFalse1 |
            propTrue !! propFalse1 !! propFalse1 |
            propTrue !! propTrue !! propTrue |> {
              (propA, propB, expected) =>
                (propA && propB).run(1, 1, seed) must_== expected.run(1, 1, seed)
            }
        }

        "Exercise 8.9: ||" in {
          import fpinscala.testing.Prop._
          def runFalsified1(maxSize: MaxSize, testCases: TestCases, rng: RNG): Result = Falsified("test1 ko", 1)
          def runFalsified2(maxSize: MaxSize, testCases: TestCases, rng: RNG): Result = Falsified("test2 ko", 2)
          def runFalsified1Or2(maxSize: MaxSize, testCases: TestCases, rng: RNG): Result = Falsified("test1 ko\ntest2 ko", 2)
          def runPassed(maxSize: MaxSize, testCases: TestCases, rng: RNG): Result = Passed

          val propFalse1 = new Prop(runFalsified1)
          val propFalse2 = new Prop(runFalsified2)
          val propFalse1Or2 = new Prop(runFalsified1Or2)
          val propTrue = new Prop(runPassed)

          "propA" || "proB" || "expected result" |
            propFalse1 !! propFalse2 !! propFalse1Or2 |
            propFalse1 !! propTrue !! propTrue |
            propTrue !! propFalse1 !! propTrue |
            propTrue !! propTrue !! propTrue |> {
              (propA, propB, expected) =>
                (propA || propB).run(1, 1, seed) must_== expected.run(1, 1, seed)
            }
        }

        "Exercise 8.10: unsized" in {
          val gen = Gen.boolean
          val sgen = gen.unsized
          (sgen(0) must_== gen) and (sgen(42) must_== gen)
        }

        "Exercise 8.11: convenience functions (map)" in {
          def g(i: Int) = Gen.choose(1, i)
          val sgen = SGen(g)
          val stringSGen = sgen.map(i => "x" * i)
          val maxLength = 6
          val xes = sequenceFromTest(1000)(seed)(stringSGen(maxLength + 1).sample.run)._1
          xes.forall { string => (string.toSet == Set('x')) && (string.size <= maxLength) } must beTrue
        }

        "Exercise 8.11: convenience functions (flatMap)" in {
          val minListLength = 6
          val maxListLength = 12
          def g(i: Int) = Gen.choose(minListLength, i)
          val sgen = SGen(g)
          def f(i: Int) = Gen.listOfN(i, Gen.choose(10, 20))
          val listOfLists = sequenceFromTest(1000)(seed)(sgen.flatMap(f)(maxListLength + 1).sample.run)._1
          (listOfLists.forall(_.size >= minListLength) must beTrue) and
            (listOfLists.forall(_.size <= maxListLength) must beTrue) and
            (listOfLists.forall(_.forall(elt => (elt >= 10) && (elt < 20))) must beTrue)
        }

        "Exercise 8.12: listOf" in {
          val sgen = Gen.listOf(Gen.unit(42))
          sgen(4).sample.run(seed)._1 must_== List(42, 42, 42, 42)
        }

        "Exercise 8.13: listOf1" in {
          val sgen1 = Gen.listOf1(Gen.choose(10, 20))
          val listOfLists = sequenceFromTest(10)(seed)(sgen1(0).sample.run)._1
          println(listOfLists)
          listOfLists.forall(_.size == 1) must beTrue
        }

        // The validity of maxProp1 is not really tested here!!
        "Exercise 8.13: maxProp with listOf1" in {
          val prop = Gen.maxProp1
          val result = prop.run(100, 100, seed)
          result.isFalsified must beFalse
        }

        // The validity of sortedProp is not really tested here!!
        "Exercise 8.14: sortedProp" in {
          val prop = Gen.sortedProp
          val result = prop.run(100, 100, seed)
          result.isFalsified must beFalse
        }
    */

  }
}