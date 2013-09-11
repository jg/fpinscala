package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG)

  def positiveInt(rng: RNG): (Int, RNG) = {
    val (int: Int, r: RNG) = rng.nextInt
    if (int == Int.MinValue)
      positiveInt(r)
    else
      (int.abs, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (result, r) = positiveInt(rng)
    ((result / (Int.MaxValue.toDouble+1)), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (r1, rng1) = nextInt
    val (r2, rng2) = double(rng1)
    ((r1, r2), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0)
      (List[Int](), rng)
    else {
      val (i1, rng1) = rng.nextInt
      val (lst, rng2) = ints(count - 1)(rng1)
      (i1 :: lst, rng2)
    }
  }

  type State[S, +A] = S => (A, S)
  type Rand[A] = State[RNG, A]

  def int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng)
    }
  }
  

  /*
   def map_v2[A, B](s: Rand[A])(f: A => B): Rand[B] =
   flatMap(s)(a => unit(f(a)))
   */

  /*
   def map2_v2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
   flatMap(ra)(a => map(rb)(b => f(a,b))
   */

  def positiveInt_v2: Rand[Int] =
    flatMap(int) { i =>
      if ( i != Int.MinValue)
        unit(i.abs) else positiveInt_v2
    }


  def positiveMax(n: Int): Rand[Int] =
    map(positiveInt)(_ % (n+1))

  def double_v2(rng: RNG): (Double, RNG) =
    map(positiveInt)(_ / (Int.MaxValue.toDouble+1))(rng)

  def intDouble_v2(rng: RNG): Rand[(Int, Double)] =
    map2(_.nextInt, double)((_, _))

  def doubleInt_v2(rng: RNG): Rand[(Double, Int)] =
    map2(double, _.nextInt)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((el, acc) => map2(el, acc)(_ :: _))

  def ints_v2(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(int))(rng)


  /*
   sealed trait Input
   case object Coin extends Input
   case object Turn extends Input
   case class Machine(locked: Boolean, candies: Int, coints: Int) {
   def simulateMachine(inputs: List[Input]): State[Machine, Int]

   }
   */

}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) &
      ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int],
       simple(seed2))
    }
  }
}

