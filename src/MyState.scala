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

  def apply[S, A](run: S => (A, S)) = new MyState(run)

  def unit[S, A](a: A) = MyState[S, A](s => (a, s))

  def sequence[S, A](sas: List[MyState[S, A]]): MyState[S, List[A]] = ???

}