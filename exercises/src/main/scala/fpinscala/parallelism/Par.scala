package fpinscala.parallelism

import java.util.concurrent._

import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.
  
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit) = get // Note: doesn't respect timeout!
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }
  
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es) 
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  // Cheated: lifted from answers
  private case class Map2Future[A,B,C](a: Future[A], b: Future[B],
                                       f: (A,B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None
    def isDone = cache.isDefined
    def isCancelled = a.isCancelled || b.isCancelled
    def cancel(evenIfRunning: Boolean) =
      a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
    def get = compute(Long.MaxValue)
    def get(timeout: Long, units: TimeUnit): C =
      compute(TimeUnit.NANOSECONDS.convert(timeout, units))

    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.nanoTime
        val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime;val aTime = stop-start
        val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
    }
  }

  // Exercises 7.3: map2 that respects the contract of timeouts on Future.
  def map2WithTimeOut[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    val futA = a(es)
    val futB = b(es)
    Map2Future(futA, futB, f)
  }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      // Exercise 7.8
      // « Note that we’re submitting the Callable first, and within that Callable, we’re submit-
      // ting another Callable to the ExecutorService and blocking on its result (recall that
      // a(es) will submit a Callable to the ExecutorService and get back a Future). »
      // More explicitly, a(es) returns a Future, which can only be created by es.submit(Callable) in the implementation:
      // i.e. you pass a Par to fork() that looks like: es => es.submit()
      //
      // Exercise 7.9
      // Any fixed-size thread pool can deadlock, since we can potentially do sufficient nested forks (recursion) to
      // deadlock. For ex. parMap() uses sequence() which can be implemented recursively.
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = 
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  // Exercise 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
/* WRONG: need be reversed
    def go[A](ps: List[Par[A]], acc: Par[List[A]]): Par[List[A]] = ps match {
      case Nil => acc
      case head :: tail => go(tail, map2(head, acc)((a, ls) => a :: ls))
    }
    go(ps, unit(List[A]()))
*/
//    ps match {
//      case Nil => unit(List[A]())
//      case head :: tail => map2(head, sequence(tail))((a, l) => a :: l)
//    }
     ps.foldRight(unit(List[A]()))((a, as) => map2(a, as)(_ :: _))
  }

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  // Exercise 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
/* WRONG: not parallel!
    as.foldRight(unit(List[A]()))((p, ps) => if (f(p)) { map2(unit(p) , ps)(_ :: _) } else ps)
*/
    fork  {
      val pars = parMap(as)(a => if (f(a)) { List(a) } else { List() })
      map(pars)(_.flatten)
    }

  /* Exercise 7.7
  Given map(y)(id) == y, it’s a free theorem that map(map(y)(g))(f) == map(y)(f compose g).
  map(map(y)(g))(f) == map(y)(f compose g)
  map(map(y)(g))(f) == map(y)(f(g))
  map(map(y)(g))(id) == map(y)(id(g))
  map(map(y)(g))(id) == map(y)(g)
  map(Y)(id) == Y
  */

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => 
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  // Exercise 7.11: runs `p`, and then uses that to select a parallel computation from `choices` (i.e. take the Nth)
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val ind = run(es)(n).get
      run(es)(choices(ind))
    }
  def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    choiceN(map(a)(if (_) 0 else 1))(choices = List(ifTrue, ifFalse))

  // Exercise 7.12: chooses from Map of Par's
  def choiceMap[K,V](p: Par[K])(ps: Map[K,Par[V]]): Par[V] =
    es => {
      val k = run(es)(p).get
      run(es)(ps(k))
    }

  // Exercise 7.13: choose generalized
  def chooser[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
    es => {
      val a = run(es)(p).get
      run(es)(f(a))
    }

  // Exercise 7.13: choice via chooser
  def choiceViaChooser[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
    chooser(p)(if (_) t else f)

  // Exercise 7.13: choiceN via chooser
  def choiceNViaChooser[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(p)(choices(_))

  def flatMap[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
    this.chooser(p)(f)

  // Exercise 7.14
  def join[A](p: Par[Par[A]]): Par[A] =
    es => {
      val a = run(es)(p).get
      run(es)(a)
    }
  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(identity)
  def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
    join(map(p)(f))

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}
