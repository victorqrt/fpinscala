sealed trait MyEither[+E, +A] {

  def map[B](f: A => B): MyEither[E, B] =
    this match {
      case MyLeft(e) => MyLeft(e)
      case MyRight(a) => MyRight(f(a))
    }

  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] =
    this match {
      case MyLeft(e) => MyLeft(e)
      case MyRight(a) => f(a)
    }

  def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] =
    this match {
      case MyLeft(_) => b
      case MyRight(a) => MyRight(a)
    }

  def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
}

final case class MyLeft[+E](value: E) extends MyEither[E, Nothing]
final case class MyRight[+A](value: A) extends MyEither[Nothing, A]

object MyEither {

  def traverse[E, A, B](as: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] =
    as match {
      case Nil => MyRight(Nil)
      case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
    }

  def sequence[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] =
    traverse(es)(x => x)
}
