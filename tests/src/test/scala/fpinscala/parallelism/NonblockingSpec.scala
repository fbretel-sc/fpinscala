package fpinscala.parallelism

import org.specs2.mutable._
import org.specs2.specification.AllExpectations
import org.specs2.matcher.DataTables
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import java.util.concurrent._

import org.specs2.matcher.Matcher
import fpinscala.parallelism.Utils._

@RunWith(classOf[JUnitRunner])
class NonblockingSpec extends Specification with DataTables {
  // Run the tests sequentially as these tests assert also the time it takes to run them.
  sequential

  val maxNbOfThreadsInTests = 8
  val es = Executors.newFixedThreadPool(maxNbOfThreadsInTests)

  // Test if a duration is in an interval.
  def be_around(duration: Long, dp: Double = durationPrecision): Matcher[Long] = {
    val i = (duration * dp).toLong
    be_>(duration - i) and be_<(duration + i)
  }

  def makeComputationA(durationMillis: Long, value: String = ""): CA = {
    Thread.sleep(durationMillis)
    CA(s"${value}-${durationMillis}ms")
  }

  def makeComputationB(durationMillis: Long, value: String = ""): CB = {
    Thread.sleep(durationMillis)
    CB(s"${value}-${durationMillis}ms")
  }


  "The following exercises should be correct" >> {

    "Exercise 7.11: choiceN." in {
      val n = Nonblocking.Par.delay({
        Thread.sleep(d1); 1
      })
      val parA = Nonblocking.Par.delay(makeComputationA(d3, "A"))
      val parB = Nonblocking.Par.delay(makeComputationA(d2, "B"))
      val parC = Nonblocking.Par.delay(makeComputationA(d3, "C"))
      val parD = Nonblocking.Par.delay(makeComputationA(d3, "D"))

      val t0 = System.currentTimeMillis()
      val parChoiceN = Nonblocking.Par.choiceN(n)(List(parA, parB, parC, parD))
      val res = Nonblocking.Par.run(es)(parChoiceN)
      val t1 = System.currentTimeMillis()
      val duration = t1 - t0
      (res must_== CA(s"B-${d2}ms")) and (duration must be_around(d1 + d2))
    }

    "Exercise 7.11: choice and terms of choiceN." in {
      val p = Nonblocking.Par.delay({ Thread.sleep(d1); false })
      val par1 = Nonblocking.Par.delay(makeComputationA(d3, "C"))
      val par2 = Nonblocking.Par.delay(makeComputationA(d2, "D"))

      val t0 = System.currentTimeMillis()
      val parChoice = Nonblocking.Par.choiceViaChoiceN(p)(par1, par2)
      val res = Nonblocking.Par.run(es)(parChoice)
      val t1 = System.currentTimeMillis()
      val duration = t1 - t0
      (res must_== CA(s"D-${d2}ms")) and (duration must be_around(d1 + d2))
    }

    "Exercise 7.12: choiceMap." in {
      val p = Nonblocking.Par.delay({ Thread.sleep(d1); 1 })
      val map = Map(
        (0, Nonblocking.Par.delay(makeComputationA(d3, "C"))),
        (1, Nonblocking.Par.delay(makeComputationA(d2, "D")))
      )

      val t0 = System.currentTimeMillis()
      val parChoice = Nonblocking.Par.choiceMap(p)(map)
      val res = Nonblocking.Par.run(es)(parChoice)
      val t1 = System.currentTimeMillis()
      val duration = t1 - t0
      (res must_== CA(s"D-${d2}ms")) and (duration must be_around(d1 + d2))
    }

    "Exercise 7.13: chooser." in {
      val pa = Nonblocking.Par.delay({ Thread.sleep(d2); CA("a") })
      def choices(ca: CA): Nonblocking.Par[CB] = Nonblocking.Par.delay(makeComputationB(d3, ca.a))

      val t0 = System.currentTimeMillis()
      val parChooser = Nonblocking.Par.chooser(pa)(choices)
      val res = Nonblocking.Par.run(es)(parChooser)
      val t1 = System.currentTimeMillis()
      (res must_== CB(s"a-${d3}ms")) and ((t1 - t0) must be_around(d2 + d3))
    }

    "Exercise 7.13: choiceNViaChooser." in {
      val n = Nonblocking.Par.delay({ Thread.sleep(d1); 1 })
      val parA = Nonblocking.Par.delay(makeComputationA(d3, "A"))
      val parB = Nonblocking.Par.delay(makeComputationA(d2, "B"))
      val parC = Nonblocking.Par.delay(makeComputationA(d3, "C"))
      val parD = Nonblocking.Par.delay(makeComputationA(d3, "D"))

      val t0 = System.currentTimeMillis()
      val parChoiceN = Nonblocking.Par.choiceNViaChooser(n)(List(parA, parB, parC, parD))
      val res = Nonblocking.Par.run(es)(parChoiceN)
      val t1 = System.currentTimeMillis()
      val duration = t1 - t0
      (res must_== CA(s"B-${d2}ms")) and (duration must be_around(d1 + d2))
    }

    "Exercise 7.13: choiceViaChooser." in {
      val p = Nonblocking.Par.delay({ Thread.sleep(d1); false })
      val par1 = Nonblocking.Par.delay(makeComputationA(d3, "C"))
      val par2 = Nonblocking.Par.delay(makeComputationA(d2, "D"))

      val t0 = System.currentTimeMillis()
      val parChoice = Nonblocking.Par.choiceViaChoiceN(p)(par1, par2)
      val res = Nonblocking.Par.run(es)(parChoice)
      val t1 = System.currentTimeMillis()
      val duration = t1 - t0
      (res must_== CA(s"D-${d2}ms")) and (duration must be_around(d1 + d2))
    }

    "Exercise 7.14: join." in {
      val parPar = Nonblocking.Par.delay({
        Thread.sleep(d2);
        Nonblocking.Par.delay(makeComputationA(d3, "A"))
      })

      val t0 = System.currentTimeMillis()
      val parJoin = Nonblocking.Par.join(parPar)
      val res = Nonblocking.Par.run(es)(parJoin)
      val t1 = System.currentTimeMillis()
      val duration = t1 - t0
      (res must_== CA(s"A-${d3}ms")) and (duration must be_around(d2 + d3))
    }

    "Exercise 7.14: joinViaFlatMap." in {
      val parPar = Nonblocking.Par.delay({
        Thread.sleep(d1);
        Nonblocking.Par.delay(makeComputationA(d2, "A"))
      })

      val t0 = System.currentTimeMillis()
      val parJoin = Nonblocking.Par.joinViaFlatMap(parPar)
      val res = Nonblocking.Par.run(es)(parJoin)
      val t1 = System.currentTimeMillis()
      val duration = t1 - t0
      (res must_== CA(s"A-${d2}ms")) and (duration must be_around(d1 + d2))
    }

    "Exercise 7.14: flatMapViaJoin." in {
      val n = Nonblocking.Par.delay({ Thread.sleep(d1); 1 })
      val parA = Nonblocking.Par.delay(makeComputationA(d3, "A"))
      val parB = Nonblocking.Par.delay(makeComputationA(d2, "B"))
      val parC = Nonblocking.Par.delay(makeComputationA(d3, "C"))
      val parD = Nonblocking.Par.delay(makeComputationA(d3, "D"))

      val t0 = System.currentTimeMillis()
      val parFlatMap = Nonblocking.Par.flatMapViaJoin(n)(List(parA, parB, parC, parD))
      val res = Nonblocking.Par.run(es)(parFlatMap)
      val t1 = System.currentTimeMillis()
      (res must_== CA(s"B-${d2}ms")) and ((t1 - t0) must be_around(d1 + d2))
    }

  }

}