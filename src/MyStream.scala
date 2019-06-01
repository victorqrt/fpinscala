sealed trait MyStream[+A] {

  def headOption: Option[A] =
    this match {
      case MyEmpty => None
      case MyCons(h, t) => Some(h())
    }

  // Not stack-safe
  def toList: List[A] =
    this match {
      case MyEmpty => Nil
      case MyCons(h, t) => h() :: t().toList
    }

  def safeToList: List[A] = {
    def go(stream: MyStream[A], acc: List[A]): List[A] =
      stream match {
        case MyEmpty => acc
        case MyCons(h, t) => go(t(), h() :: acc)
      }

    go(this, Nil).reverse
  }

  def take(n: Int): MyStream[A] =
    this match {
      case MyCons(h, t) if (n > 1) => MyStream.cons(h(), t().take(n - 1))
      case MyCons(h, _) if (n == 1) => MyStream.cons(h(), MyStream.empty)
      case _ => MyStream.empty
    }

  def drop(n: Int): MyStream[A] =
    this match {
      case MyCons(h, t) if (n > 0) => t().drop(n - 1)
      case _ if (n == 0) => this
      case _ => MyStream.empty
    }

  def takeWhile(p: A => Boolean): MyStream[A] =
    this match {
      case MyCons(h, t) if (p(h())) => MyStream.cons(h(), t() takeWhile p)
      case _ => MyStream.empty
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case MyCons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  // Not stack-safe: what if all elements evaluate to false
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileWithFoldRight(p: A => Boolean): MyStream[A] =
    foldRight(MyStream.empty[A])(
      (a, b) => if (p(a)) MyStream.cons(a, b) else MyStream.empty
    )

  def headOptionWithFoldRight: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): MyStream[B] =
    foldRight(MyStream.empty[B])((a, b) => MyStream.cons(f(a), b))

  def filter(f: A => Boolean): MyStream[A] =
    foldRight(MyStream.empty[A])((a, b) => if (f(a)) MyStream.cons(a, b) else b)

  def append[B >: A](a: B): MyStream[B] =
    foldRight(MyStream(a))(MyStream.cons(_, _))

  def append[B >: A](sb: MyStream[B]): MyStream[B] =
    foldRight(sb)(MyStream.cons(_, _))

  def flatMap[B](f: A => MyStream[B]): MyStream[B] =
    foldRight(MyStream.empty[B])((a, b) => f(a) append b)
}

case object MyEmpty extends MyStream[Nothing]
final case class MyCons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]

object MyStream {

  def cons[A](head: => A, tail: => MyStream[A]): MyStream[A] = {
    lazy val hd = head
    lazy val tl = tail
    MyCons(() => hd, () => tl)
  }

  def empty[A]: MyStream[A] = MyEmpty

  def apply[A](as: A*): MyStream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): MyStream[A] = MyStream.cons(a, constant(a))

  // TODO def from(n: Int): MyStream[Int] = MyStream.cons(
}