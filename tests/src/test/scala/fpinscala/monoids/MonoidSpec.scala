package fpinscala.monoids

import fpinscala.monoids.Monoid.{monoidFunctionLaws, monoidLaws}
import fpinscala.state.RNG
import fpinscala.testing.Gen
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MonoidSpec extends Specification {

  import fpinscala.testing.Prop
  import Prop._

  def runProp(prop: Prop, testCases: TestCases): Result = {
    import fpinscala.state.RNG.Simple
    prop.run(100, testCases, Simple(0))
  }

  // Lifted from fpinscala-muc/fpinscala-skasinsk
  def stringN(n: Int): Gen[String] =
    Gen.listOfN(n, Gen.choose(0, 127)).map(_.map(_.toChar).mkString)

  val IntGenMax = 100

  def intGen(max: Int) = Gen.choose(0, max)

  def listGen[A](gen: Gen[A]) = gen.listOfN(intGen(10))

  val stringGen = intGen(10) flatMap (stringN)

  def optionGen[A](gen: Gen[A]): Gen[Option[A]] =
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

    "Exercise 10.2: Monoid instance for combining Option values." in {
      checkMonoidLaws(Monoid.optionMonoid[Int], optionGen(intGen(IntGenMax)))
      checkMonoidLaws(Monoid.optionMonoid[Boolean], optionGen(Gen.boolean))
      checkMonoidLaws(Monoid.optionMonoid[String], optionGen(stringGen))
    }

    "Exercise 10.3: Monoid instance for endofunctions." in {
      val boolEndoGen: Gen[Boolean => Boolean] =
        Gen.boolean.map(p => if (p) { x: Boolean => !x } else identity[Boolean])
      val prop = monoidFunctionLaws(Monoid.endoMonoid[Boolean], boolEndoGen, Gen.boolean)
      val result = prop.run(100, 100, RNG.Simple(0))
      result.isFalsified must beFalse
    }

  }

  /*

    behavior of "10.5 foldMap"
    it should "work" in {
      forAll("ints") { ints: List[Int] =>
        val intsAsStrings = ints map(_.toString)
        assert(Monoid.foldMap(intsAsStrings, Monoid.intAddition)(_.toInt) == ints.sum)
      }
    }

    behavior of "10.6.1 foldRight"
    it should "work" in {
      val plus = (_:Int) + (_:Int)
      forAll("ints") { ints: List[Int] =>
        assert(Monoid.foldRight(ints)(0)(plus) == ints.sum)
      }
    }

    behavior of "10.6.2 foldLeft"
    it should "work" in {
      val plus = (_:Int) + (_:Int)
      forAll("ints") { ints: List[Int] =>
        assert(Monoid.foldLeft(ints)(0)(plus) == ints.sum)
      }
    }

    behavior of "10.7 foldMapV"
    it should "work" in {
      forAll("ints") { ints: List[Int] =>
        val intsAsStrings = ints.map(_.toString).toIndexedSeq
        assert(Monoid.foldMapV(intsAsStrings, Monoid.intAddition)(_.toInt) == ints.sum)
      }
    }

    behavior of "10.8 parFoldMap"
    it should "work" in {
      import fpinscala.parallelism.Nonblocking.Par
      val es = Executors.newFixedThreadPool(4)
      forAll("ints") { ints: List[Int] =>
        val intsAsStrings = ints.map(_.toString).toIndexedSeq
        val parSum = Monoid.parFoldMap(intsAsStrings, Monoid.intAddition)(_.toInt)
        assert(Par.run(es)(parSum) == ints.sum)
      }
    }

    behavior of "10.9 ordered"
    it should "work" in {
      assert(Monoid.ordered(IndexedSeq()))
      assert(Monoid.ordered(IndexedSeq(1)))
      assert(Monoid.ordered(IndexedSeq(-2, 0, 1, 3, 5)))
      assert(Monoid.ordered(IndexedSeq(-2, 0, 3, 1, 6)) == false)
      forAll("ints") {ints: Seq[Int] =>
        assert(Monoid.ordered(ints.toIndexedSeq) == (ints == ints.sorted))
      }
    }

    behavior of "10.10 wcMonoid"
    it should "obey the monoid laws" in {
      import fpinscala.testing.Gen
      import fpinscala.testing.Prop.Passed
      def intGen(max: Int) = Gen.choose(0, max)
      def listGen[A](gen: Gen[A]) = gen.listOfN(intGen(10))
      val stringGen = intGen(10) flatMap(Gen.stringN)
      val stubGen = stringGen map(Monoid.Stub(_))
      val partGen = for {
        lStub <- stringGen
        words <- intGen(10)
        rStub <- stringGen
      } yield Monoid.Part(lStub, words, rStub)
      val wcGen: Gen[Monoid.WC] = Gen.union(stubGen, partGen)
      val laws = Monoid.monoidLaws(Monoid.wcMonoid, wcGen)
      assert(runProp(laws, 10) == Passed)
    }

    behavior of "10.11 countWords"
    it should "work" in {
      val strGen: Gen[String] = {
        val whitespaceCharGen = Gen.oneOf(9.toChar, 10.toChar, 32.toChar)
        val nonWhitespaceCharGen = Gen.choose(33.toChar, 127.toChar)
        val charGen = Gen.frequency((1, whitespaceCharGen), (9, nonWhitespaceCharGen))
        Gen.listOf(charGen).map(_.mkString)
      }
      def wordCount(s: String) = {
        val s1 = s.trim
        if (s1 == "") 0 else s1.split("""\s+""").size
      }
      forAll(strGen label "s") { s: String =>
        assert(Monoid.countWords(s) == wordCount(s))
      }
    }

    val plus = (_:Int) + (_:Int)
    private def testFoldable[F[_]](foldable: Foldable[F], f: List[Int] => F[Int]) = {
      forAll("ints") { ints: List[Int] =>
        val intsF = f(ints)
        val sum = ints.sum
        assert(foldable.foldRight(intsF)(0)(plus) == sum)
        assert(foldable.foldLeft(intsF)(0)(plus) == sum)
        assert(foldable.foldMap(intsF)(_.toString)(Monoid.stringMonoid) ==
          ints.map(_.toString).fold("")(_ + _))
        assert(foldable.concatenate(intsF)(Monoid.intAddition) == sum)
  //      assert(foldable.toList(intsF) == ints)
      }
    }

    behavior of "10.12.1 ListFoldable"
    it should "work" in {
      testFoldable(ListFoldable, identity)
    }

    behavior of "10.12.2 IndexedSeqFoldable"
    it should "work" in {
      testFoldable(IndexedSeqFoldable, _.toIndexedSeq)
    }

    behavior of "10.12.3 StreamFoldable"
    it should "work" in {
      testFoldable(StreamFoldable, _.toStream)
    }

    private implicit def arbTree[T](implicit ev: Arbitrary[T]): Arbitrary[Tree[T]] = {
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
        arbitrary[Boolean] flatMap { createTree(_, 0) }
      }
    }

    private def treeList[A](as: Tree[A]): List[A] = as match {
      case Leaf(a) => List(a)
      case Branch(l,r) => treeList(l) ::: treeList(r)
    }

    behavior of "10.13 TreeFoldable"
    it should "work" in {
    def treeSum(ints: Tree[Int]): Int = ints match {
      case Leaf(i) => i
      case Branch(l,r) => treeSum(l) + treeSum(r)
    }

    val foldable = TreeFoldable
    forAll("ints") { ints: Tree[Int] =>
        val sum = treeSum(ints)
        assert(foldable.foldRight(ints)(0)(plus) == sum)
        assert(foldable.foldLeft(ints)(0)(plus) == sum)
        assert(foldable.foldMap(ints)(_.toInt)(Monoid.intAddition) == sum)
        assert(foldable.concatenate(ints)(Monoid.intAddition) == sum)
  //      assert(foldable.toList(ints) == treeList(ints))
      }
    }

    behavior of "10.14 OptionFoldable"
    it should "work" in {
      val foldable = OptionFoldable
      forAll("ints") { ints: Option[Int] =>
        val sum = ints.fold(0)(_ + 0)
        assert(foldable.foldRight(ints)(0)(plus) == sum)
        assert(foldable.foldLeft(ints)(0)(plus) == sum)
        assert(foldable.foldMap(ints)(_.toInt)(Monoid.intAddition) == sum)
        assert(foldable.concatenate(ints)(Monoid.intAddition) == sum)
  //      assert(foldable.toList(ints) == ints.fold(List[Int]())(List(_)))
      }
    }

    behavior of "10.15 Foldable.toList"
    it should "work" in {
      forAll("ints") { ints: List[Int] =>
        assert(ListFoldable.toList(ints) == ints)
        assert(IndexedSeqFoldable.toList(ints.toIndexedSeq) == ints)
        assert(StreamFoldable.toList(ints.toStream) == ints)
      }
      forAll("ints") { ints: Tree[Int] =>
        assert(TreeFoldable.toList(ints) == treeList(ints))
      }
      forAll("ints") { ints: Option[Int] =>
        assert(OptionFoldable.toList(ints) == ints.fold(List[Int]())(List(_)))
      }
    }

    behavior of "10.16 productMonoid"
    it should "work" in {
      val pMonoid = Monoid.productMonoid(Monoid.intAddition, Monoid.intAddition)
      checkMonoidLaws[(Int,Int)](pMonoid, Arbitrary.arbTuple2[Int,Int].arbitrary)
    }

    behavior of "10.17 functionMonoid"
    it should "obey the monoid laws" in {
      val fMonoid = Monoid.functionMonoid[Int,String](Monoid.stringMonoid)
      val functionGen =
        Gen.oneOf[Int => String]({x: Int => x.toString}, {x: Int => x.toString + "x"})
      checkMonoidLaws[Int => String](fMonoid, functionGen, isEqual[Int,String] _)
    }

    behavior of "10.18 bag"
    it should "work" in {
      assert(Monoid.bag(Vector("a", "rose", "is", "a", "rose")) ==
        Map("a" -> 2, "rose" -> 2, "is" -> 1))

      val words = "Yesterday all my troubles seemed so far away".split(" ").toSeq
      val wordGen = Gen.oneOf(words)
      val sentenceGen = Gen.listOf(wordGen)
      forAll(sentenceGen label "as") { as: List[String] =>
        val wordBag = as.groupBy(identity).mapValues(_.size)
        assert(Monoid.bag(as.toIndexedSeq) == wordBag)
      }
    }
  */
}
