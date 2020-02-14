package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  @scala.annotation.tailrec
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    if (i == Int.MinValue)
      nonNegativeInt(rng2)
    else (if (i < 0) -i else i, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    (i.toDouble/Int.MaxValue, rng2)
  }

  def doubleViaMap: Rand[Double] = map(nonNegativeInt)(_.toDouble/Int.MaxValue)

  // instructions not clear, but we guess from double3 that we should not reuse rng's
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1,d2,d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
/*
    if (count <= 0)
      (Nil, rng)
    else {
      val (i, r) = rng.nextInt
      val (l, rn) = ints(count - 1)(r)
      (i :: l, rn)
    }
*/
  {
    @annotation.tailrec
    def intsAux(n: Int, acc: List[Int], r: RNG): (List[Int], RNG) =
      if (n <= 0)
        (acc, r)
      else {
        val (i, rn) = r.nextInt
        intsAux(n-1, i :: acc, rn)
      }
    intsAux(count, Nil, rng) match {
      case (ints, rng) => (ints.reverse, rng)
    }
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  // test with sequence(List(int, int, int))(Simple(1)) == ints(3)(Simple(1))
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    def sequenceAux(l: List[Rand[A]], acc: List[A]): Rand[List[A]] = rng1 => l match {
      case ::(hd, tl) =>
        val (a, rng2) = hd(rng1)
        sequenceAux(tl, a :: acc)(rng2)
      case Nil => (acc, rng1)
    }
    rng => sequenceAux(fs, Nil)(rng) match { case (la,rn) => (la.reverse, rn) }
  }

  def sequenceViaFold[A](fs: List[Rand[A]]): Rand[List[A]] =
/*
    // the trick is to bootstrap with unit() !!!
    rng => {
      fs.foldLeft((List[A](), rng)) { case ((acc, r1), ra) =>
        val (a, r2) = ra(r1)
        (a :: acc, r2)
      } match { case (l, rn) => (l.reverse, rn) }
    }
*/
    // The crazy thing here is that we never pass any RNG
    fs.foldLeft(unit(List[A]()))((acc, ra) => map2(ra, acc)(_ :: _))

  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r1) = f(rng)
    g(a)(r1)
  }

  // Can't just modulo since Int.MaxInt might not be a multiple of n.
  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)(i =>
/*
    // better expressed with unit() !!
    rng =>  {
      val mod = i % n
      if (i + (n - 1) - mod >= 0) (mod, rng)
      else nonNegativeLessThan(n)(rng)
    }
*/
  {
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod)
    else nonNegativeLessThan(n)
  }
  )

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))
  def map2ViaFlapMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a,b)))
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap (a => State.unit(f(a)))
/*
    State { s =>
      val (a, s2) = run(s)
      (f(a), s2)
    }
*/

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a,b)))
/*
    State { s =>
      val (a, s2) = run(s)
      val (b, s3) = sb.run(s2)
      (f(a, b), s3)
    }
*/

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, s2) = run(s)
    f(a).run(s2)
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.reverse.foldLeft(unit[S, List[A]](List[A]()))((acc, s) => s.map2(acc)(_ :: _))

  type Rand[A] = State[RNG, A]

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

object Machine  {
  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) => Machine(locked = false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) => Machine(locked = true, candy - 1, coin)
    }

  def simulate(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- State.sequence(inputs map (State.modify[Machine] _ compose update))
    s <- State.get
  } yield (s.coins, s.candies)
}

