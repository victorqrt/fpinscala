sealed trait MyOption[+A] {

  def getOrElse[B >: A](default: => B): B =
    this match {
      case MyNone => default
      case MySome(get) => get
    }

  def map[B](f: A => B): MyOption[B] =
    this match {
      case MyNone => MyNone
      case MySome(get) => MySome(f(get))
    }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = map(f) getOrElse MyNone

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = this map (MySome(_)) getOrElse ob

  def filter(f: A => Boolean): MyOption[A] = flatMap(a => if (f(a)) MySome(a) else MyNone)
}

final case class MySome[+A](get: A) extends MyOption[A]
final case object MyNone extends MyOption[Nothing]

object MyOption {

  def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def sequence[A](as: List[MyOption[A]]): MyOption[List[A]] =
    as.foldRight[MyOption[List[A]]](MySome(Nil))((x, y) => map2(x, y)(_ :: _))

  def traverse[A, B](as: List[A])(f: A => MyOption[B]): MyOption[List[B]] =
    as match {
      case Nil => MySome(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }

  def seqWithTraverse[A](as: List[MyOption[A]]): MyOption[List[A]] =
    traverse[MyOption[A], A](as)(x => x)
}
