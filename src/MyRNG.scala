sealed trait MyRNG {
  def nextInt: (Int, MyRNG)
}

object MyRNG {

  def nonNegativeInt(rng: MyRNG): (Int, MyRNG) = {
    val (n, newRng) = rng.nextInt
    if (n >= 0) (n, newRng)
    else if (n == Int.MinValue) nonNegativeInt(newRng)
    else (-n, newRng)
  }

  def double(rng: MyRNG): (Double, MyRNG) = {
    val (n, newRng) = nonNegativeInt(rng)
    ((n min (Int.MaxValue - 1)).toDouble / Int.MaxValue, newRng)
  }

  def intDouble(rng: MyRNG): ((Int, Double), MyRNG) = {
    val (n, _newRng) = rng.nextInt
    val (d, newRng) = double(_newRng)
    ((n, d), newRng)
  }

  def doubleInt(rng: MyRNG): ((Double, Int), MyRNG) = {
    val (n, _newRng) = rng.nextInt
    val (d, newRng) = double(_newRng)
    ((d, n), newRng)
  }

  def double3(rng: MyRNG): ((Double, Double, Double), MyRNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: MyRNG): (List[Int], MyRNG) = {

    def go(count: Int, _rng: MyRNG, xs: List[Int]): (List[Int], MyRNG) =
      if (count == 0) (xs, _rng)
      else {
        val (x, _rng2) = _rng.nextInt
        go(count - 1, _rng2, x :: xs)
      }

    go(count, rng, Nil)
  }

  type Rand[+A] = MyRNG => (A, MyRNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, _rng) = s(rng)
      (f(a), _rng)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, _rng) = ra(rng)
      val (b, _rng1) = rb(_rng)
      (f(a, b), _rng1)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](ras: List[Rand[A]]): Rand[List[A]] =
    ras.foldRight(unit(List[A]()))(
      (r1, r2) => map2(r1, r2)(_ :: _)
    )
}

final case class MySimpleRNG(seed: Long) extends MyRNG {

  def nextInt: (Int, MyRNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = MySimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
