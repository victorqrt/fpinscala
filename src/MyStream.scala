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

  def mapViaUnfold[B](f: A => B): MyStream[B] =
    MyStream.unfold(this) {
      case MyCons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): MyStream[A] =
    MyStream.unfold((this, n)) {
      case (MyCons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean) =
    MyStream.unfold(this) {
      case MyCons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B, C](bs: MyStream[B])(f: (A, B) => C) =
    MyStream.unfold((this, bs)) {
      case (MyCons(h1, t1), MyCons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](bs: MyStream[B]) =
    MyStream.unfold((this, bs)) {
      case (MyCons(h1, t1), MyCons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (MyCons(h, t), _) => Some((Some(h()), None), (t(), MyStream.empty))
      case (_, MyCons(h, t)) => Some((None, Some(h())), (MyStream.empty, t()))
      case _ => None
    }

  def startsWith[A](s: MyStream[A]): Boolean = zipWith(s)(_ == _).forAll(_ == true)

  def tails: MyStream[MyStream[A]] =
    MyStream.unfold(this) {
      case s @ MyCons(_, t) => Some((s, t()))
      case _ => None
    }

  def hasSubsequence[A](s: MyStream[A]): Boolean = tails exists (_ startsWith s)

  // We are yielding a pair of (B, MyStream[B]) as we want the fold to yield a stream
  // but we also keep the previous result to avoid recomputing it
  def scanRight[B](z: B)(f: (A, => B) => B): MyStream[B] =
    foldRight((z, MyStream(z)))(
      (a, _b) => {
        lazy val b = _b
        val result = f(a, b._1)
        (result, MyStream.cons(result, b._2))
      }
    )._2
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

  def from(n: Int): MyStream[Int] = MyStream.cons(n, from(n + 1))

  val fibs = {
    def go(f0: Int, f1: Int): MyStream[Int] = MyStream.cons(f0, go(f1, f0 + f1))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): MyStream[A] =
    f(z) match {
      case Some((a, s)) => MyStream.cons(a, unfold(s)(f))
      case None => MyStream.empty[A]
    }

  def fibsViaUnfold = unfold[Int, (Int, Int)](0, 1) {
    case (f0, f1) => Some((f0, (f1, f0 + f1)))
  }

  def fromViaUnfold(n: Int) = unfold[Int, Int](n)(x => Some(x, x + 1))

  def constantViaUnfold[A](a: A) = unfold[A, A](a)(Some(a, _))
}
