package fpinscala.monoids

import java.util.concurrent.Executors

import fpinscala.monoids.Monoid.{monoidFunctionLaws, monoidLaws}
import fpinscala.state.RNG
import fpinscala.testing.Gen
import org.junit.runner.RunWith
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Prop => SCProp}
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import org.specs2.ScalaCheck

@RunWith(classOf[JUnitRunner])
class MonoidSpec extends Specification with ScalaCheck {

  import fpinscala.testing.Prop
  import Prop._

  def checkProp(prop: Prop, testCases: TestCases = 100): MatchResult[Boolean] = {
    val result = prop.run(100, testCases, RNG.Simple(0))
    result.isFalsified must beFalse
  }

  // Lifted from fpinscala-muc/fpinscala-skasinsk
  def stringN(n: Int): Gen[String] =
    Gen.listOfN(n, Gen.choose(0, 127)).map(_.map(_.toChar).mkString)

  val IntGenMax = 100

  def intGen(max: Int): Gen[MaxSize] = Gen.choose(0, max)

  def listGen[A](gen: Gen[A]): Gen[List[A]] = gen.listOfN(intGen(10))

  val stringGen: Gen[String] = intGen(10) flatMap stringN

  def optGen[A](gen: Gen[A]): Gen[Option[A]] =
    for {
      b <- Gen.boolean
      a <- gen
    } yield if (b) Some(a) else None

  // Exercise 10.4
  private def checkMonoidLaws[A](m: Monoid[A], gen: Gen[A]) = {
    val prop = monoidLaws(m, gen)
    val result = prop.run(100, 100, RNG.Simple(0))
    result.isFalsified must beFalse
  }

  "The following exercises should be correct" >> {

    "Exercise 10.1: Monoid instances for integer addition and multiplication as well as the Boolean" in {
      checkMonoidLaws(Monoid.intAddition, intGen(IntGenMax))
      checkMonoidLaws(Monoid.intMultiplication, intGen(IntGenMax))
      checkMonoidLaws(Monoid.booleanOr, Gen.boolean)
      checkMonoidLaws(Monoid.booleanAnd, Gen.boolean)
    }

    "Exercise 10.1.1: Monoid laws do not hold for integer subtraction" in {
      val intSubtraction: Monoid[Int] = new Monoid[Int] {
        override def op(a1: Int, a2: Int): Int = a1 - a2

        override def zero: Int = 0
      }
      val prop = monoidLaws(intSubtraction, intGen(IntGenMax))
      val result = prop.run(100, 100, RNG.Simple(0))
      result.isFalsified must beTrue
    }

    "Exercise 10.2: Monoid instance for combining Option values" in {
      checkMonoidLaws(Monoid.optionMonoid[Int], optGen(intGen(IntGenMax)))
      checkMonoidLaws(Monoid.optionMonoid[Boolean], optGen(Gen.boolean))
      checkMonoidLaws(Monoid.optionMonoid[String], optGen(stringGen))
    }

    "Exercise 10.3: Monoid instance for endofunctions" in {
      val boolEndoGen: Gen[Boolean => Boolean] =
        Gen.boolean.map(p => if (p) { x: Boolean => !x } else identity[Boolean])
      checkProp(monoidFunctionLaws(Monoid.endoMonoid[Boolean], boolEndoGen, Gen.boolean))
    }

    "Exercise 10.5: Monoid.foldMap" in {
      checkProp(Prop.forAll(listGen(intGen(IntGenMax))) { ints: List[Int] =>
        val intsAsStrings = ints map (_.toString)
        Monoid.foldMap(intsAsStrings, Monoid.intAddition)(_.toInt) == ints.sum
      })
    }

    "Exercise 10.6: Monoid.foldRight" in {
      checkProp(Prop.forAll(listGen(intGen(IntGenMax))) { ints: List[Int] =>
        Monoid.foldRight(ints)(0)(_ - _) == ints.foldRight(0)(_ - _)
      })
    }

    "Exercise 10.6: Monoid.foldLeft" in {
      checkProp(Prop.forAll(listGen(intGen(IntGenMax))) { ints: List[Int] =>
        Monoid.foldLeft(ints)(0)(_ - _) == ints.foldLeft(0)(_ - _)
      })
    }

    "Exercise 10.7: Monoid.foldMapV" in {
      checkProp(Prop.forAll(listGen(intGen(IntGenMax))) { ints: List[Int] =>
        val intsAsStrings = ints.map(_.toString).toIndexedSeq
        Monoid.foldMapV(intsAsStrings, Monoid.intAddition)(_.toInt) == ints.sum
      })
    }

    "Exercise 10.8: parallel Monoid.foldMap" in {
      import fpinscala.parallelism.Nonblocking.Par
      val es = Executors.newFixedThreadPool(4)
      checkProp(Prop.forAll(listGen(intGen(IntGenMax))) { ints: List[Int] =>
        val intsAsStrings = ints.map(_.toString).toIndexedSeq
        val parSum = Monoid.parFoldMap(intsAsStrings, Monoid.intAddition)(_.toInt)
        Par.run(es)(parSum) == ints.sum
      })
    }

    "Exercise 10.9: ordered" in {
      assert(Monoid.ordered(IndexedSeq()))
      assert(Monoid.ordered(IndexedSeq(1)))
      assert(Monoid.ordered(IndexedSeq(-2, 0, 1, 3, 5)))
      assert(!Monoid.ordered(IndexedSeq(-2, 0, 3, 1, 6)))
      checkProp(Prop.forAll(listGen(intGen(IntGenMax))) { ints: List[Int] =>
        Monoid.ordered(ints.toIndexedSeq) == (ints == ints.sorted)
      })
    }

    "Exercise 10.10: wcMonoid" in {
      val stubGen = stringGen map Monoid.Stub
      val partGen = for {
        lStub <- stringGen
        words <- intGen(10)
        rStub <- stringGen
      } yield {
        val part = Monoid.Part(lStub, words, rStub)
        //        println(part)
        part
      }
      val wcGen: Gen[Monoid.WC] = Gen.union(stubGen, partGen)
      val laws = Monoid.monoidLaws(Monoid.wcMonoid, wcGen)
      checkProp(laws)
    }

    def oneOf[T](xs: Iterable[T]): Gen[T] = {
      val vector = xs.toVector
      Gen.choose(0, vector.size - 1).map(vector(_))
    }

    "Exercise 10.11: countWords" in {
      val strGen: Gen[String] = {
        val whitespaceCharGen: Gen[Char] = oneOf(List(9, 10, 32).map(_.toChar))
        val nonWhitespaceCharGen: Gen[Char] = oneOf(List(33, 127).map(_.toChar))
        val charGen = Gen.weighted((whitespaceCharGen, 1), (nonWhitespaceCharGen, 9))

        def strGen(n: Int) = Gen.listOfN(n, charGen).map(_.mkString)

        intGen(10) flatMap strGen
      }

      def wordCount(s: String) = {
        val s1 = s.trim
        if (s1 == "") 0 else s1.split("""\s+""").length
      }

      checkProp(Prop.forAll(strGen) { s: String => Monoid.countWords(s) == wordCount(s) })
    }

    val plus = (_: Int) + (_: Int)

    def testFoldable[F[_]](foldable: Foldable[F], f: List[Int] => F[Int]) = {
      checkProp(Prop.forAll(listGen(intGen(IntGenMax))) { ints: List[Int] =>
        val intsF = f(ints)
        val sum = ints.sum

        foldable.foldRight(intsF)(0)(plus) == sum &&
          foldable.foldLeft(intsF)(0)(plus) == sum &&
          foldable.foldMap(intsF)(_.toString)(Monoid.stringMonoid) ==
            ints.map(_.toString).fold("")(_ + _) &&
          foldable.concatenate(intsF)(Monoid.intAddition) == sum &&
          foldable.toList(intsF) == ints
      })
    }

    "Exercise 10.12: ListFoldable" in {
      testFoldable(ListFoldable, identity[List[Int]])
    }

    "Exercise 10.12: IndexedSeqFoldable" in {
      testFoldable(IndexedSeqFoldable, _.toIndexedSeq)
    }

    "Exercise 10.12: StreamFoldable" in {
      testFoldable(StreamFoldable, _.toStream)
    }


    implicit def arbTree[T](implicit ev: Arbitrary[T]): Arbitrary[Tree[T]] = {
      import org.scalacheck.Gen

      val maxDepth = 10 // to prevent StackOverflows

      def createLeaf: Gen[Tree[T]] = arbitrary[T] map (Leaf(_))

      def createBranch(depth: Int): Gen[Tree[T]] = {
        for {
          lIsLeaf <- arbitrary[Boolean]
          rIsLeaf <- arbitrary[Boolean]
          l <- createTree(lIsLeaf, depth)
          r <- createTree(rIsLeaf, depth)
        } yield Branch(l, r)
      }

      def createTree(isLeaf: Boolean, depth: Int): Gen[Tree[T]] =
        if (isLeaf || depth >= maxDepth) createLeaf else createBranch(depth + 1)

      Arbitrary {
        arbitrary[Boolean] flatMap {
          createTree(_, 0)
        }
      }
    }

    def treeList[A](as: Tree[A]): List[A] = as match {
      case Leaf(a) => List(a)
      case Branch(l, r) => treeList(l) ::: treeList(r)
    }

    "Exercise 10.13: TreeFoldable" in {
      def treeSum(ints: Tree[Int]): Int = ints match {
        case Leaf(i) => i
        case Branch(l, r) => treeSum(l) + treeSum(r)
      }

      val foldable = TreeFoldable
      SCProp.forAll(arbTree[Int].arbitrary) { ints: Tree[Int] =>
        val sum = treeSum(ints)
        foldable.foldRight(ints)(0)(plus) == sum &&
          foldable.foldLeft(ints)(0)(plus) == sum &&
          foldable.foldMap(ints)(identity)(Monoid.intAddition) == sum &&
          foldable.concatenate(ints)(Monoid.intAddition) == sum &&
          foldable.toList(ints) == treeList(ints)
      }
    }

    "Exercise 10.14: OptionFoldable" in {
      val foldable = OptionFoldable
      checkProp(Prop.forAll(optGen(intGen(IntGenMax))) { ints: Option[Int] =>
        val sum = ints.fold(0)(_ + 0)
        foldable.foldRight(ints)(0)(plus) == sum &&
          foldable.foldLeft(ints)(0)(plus) == sum &&
          foldable.foldMap(ints)(_.toInt)(Monoid.intAddition) == sum &&
          foldable.concatenate(ints)(Monoid.intAddition) == sum &&
          foldable.toList(ints) == ints.fold(List[Int]())(List(_))
      })
    }

    "Exercise 10.15: Foldable.toList" in {
      checkProp(Prop.forAll(listGen(intGen(IntGenMax))) { ints: List[Int] =>
        ListFoldable.toList(ints) == ints
        IndexedSeqFoldable.toList(ints.toIndexedSeq) == ints
        StreamFoldable.toList(ints.toStream) == ints
      })
    }
    "Exercise 10.15: TreeFoldable.toList" in {
      SCProp.forAll(arbTree[Int].arbitrary) { ints: Tree[Int] =>
        TreeFoldable.toList(ints) == treeList(ints)
      }
    }
    "Exercise 10.15: OptionFoldable.toList" in {
      checkProp(Prop.forAll(optGen(intGen(IntGenMax))) { ints: Option[Int] =>
        OptionFoldable.toList(ints) == ints.fold(List[Int]())(List(_))
      })
    }

    "Exercise 10.16: productMonoid" in {
      val tuple2Gen: Gen[(Int, Int)] = for {
        a <- intGen(IntGenMax)
        b <- intGen(IntGenMax)
      } yield (a, b)

      val pMonoid = Monoid.productMonoid(Monoid.intAddition, Monoid.intMultiplication)
      checkMonoidLaws[(Int, Int)](pMonoid, tuple2Gen)
    }

    "Exercise 10.17: functionMonoid" in {
      val fMonoid = Monoid.functionMonoid[Int, String](Monoid.stringMonoid)
      val functionGen =
        oneOf[Int => String](List({ x: Int => x.toString }, { x: Int => x.toString + "x" }))
      checkProp(monoidFunctionLaws(fMonoid, functionGen, intGen(IntGenMax)))
    }

    "Exercise 10.18: bag" in {
      Monoid.bag(Vector("a", "rose", "is", "a", "rose")) shouldEqual Map("a" -> 2, "rose" -> 2, "is" -> 1)

      val words = "Yesterday all my troubles seemed so far away".split(" ").toSeq
      val wordGen = oneOf(words)
      val sentenceGen = Gen.listOf(wordGen)
      checkProp(Prop.forAll(sentenceGen) { as: List[String] =>
        val wordBag = as.groupBy(identity).mapValues(_.size)
        Monoid.bag(as.toIndexedSeq) == wordBag
      })
    }

  }

}
