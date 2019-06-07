case class MyState[S, +A](run: S => (A, S)) {

  def flatMap[B](f: A => MyState[S, B]) = MyState[S, B](
    s => {
      val (a, _s) = run(s)
      f(a).run(_s)
    }
  )

  def map[B](f : A => B): MyState[S, B] = flatMap(a => MyState.unit(f(a)))

  def map2[B, C](sb: MyState[S, B])(f: (A, B) => C): MyState[S, C] =
    for {
      sa <- this
      _sb <- sb
    } yield f(sa, _sb)
}

object MyState {

  def unit[S, A](a: A) = MyState[S, A](s => (a, s))

  def sequence[S, A](sas: List[MyState[S, A]]): MyState[S, List[A]] =
    sas.foldRight(unit[S, List[A]](Nil))((sa, acc) => sa.map2(acc)(_ :: _))

  def get[S]: MyState[S, S] = MyState(s => (s, s))

  def set[S](s: S): MyState[S, Unit] = MyState(_ => ((), s))

  def modify[S](f: S => S): MyState[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {

  def processInput(i: Input)(m: Machine): Machine =
    (m, i) match {

      case (Machine(_, 0, _), _)
         | (Machine(true, _, _), Turn)
         | (Machine(false, _, _), Coin) => m

      case (Machine(true, candies, coins), Coin) => Machine(false, candies, coins + 1)

      case (Machine(false, candies, coins), Turn) => Machine(true, candies - 1, coins)
    }

  import MyState._
  def simulateMachine(inputs: List[Input]): MyState[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs map (modify[Machine] _ compose processInput)) // flatMapping on a MyState[Machine, List[Unit]]
      s <- get // Now we get a MyState[Machine, Machine], note that _ above expresses how get ignores 2nd type param of MyState
    } yield(s.coins, s.candies) // On which we map s => (Int, Int). We get a MyState[Machine, (Int, Int)]
}