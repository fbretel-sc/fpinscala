package fpinscala.parallelism

import java.util.concurrent._


/**
 * Utility classes and constants for ParSpec and NonBlockingSpec
 */
object Utils {

  // Concrete classes for the generic types A, B and C.
  sealed class ConcreteType(a: String) {}
  case class CA(val a: String) extends ConcreteType(a)
  case class CB(val b: String) extends ConcreteType(b)
  case class CC(val c: String) extends ConcreteType(c)

  case class CallableCA(durationMillis: Long, value: String = "") extends Callable[CA] {
    def call(): CA = {
      Thread.sleep(durationMillis)
      CA(s"${value}${durationMillis}ms")
    }
  }

  case class CallableCB(durationMillis: Long, value: String = "") extends Callable[CB] {
    def call(): CB = {
      Thread.sleep(durationMillis)
      CB(s"${value}${durationMillis}ms")
    }
  }

  // Predefined durations
  val durationPrecision = 0.2 // 20%. 
  val d1 = 100L
  val d2 = d1 * 2
  val d3 = d2 * 2
  val tooLongDuration = d3 * 1000000 // For threads that are not supposed to run. 

  def safeMaxFor(duration: Long) = (duration * (1 + durationPrecision)).toLong
  def safeMinFor(duration: Long) = (duration * (1 - durationPrecision)).toLong

}