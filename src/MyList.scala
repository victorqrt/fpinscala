sealed trait MyList[+A]
final case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]
final case object MyNil extends MyList[Nothing]

object MyList {

  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) MyNil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: MyList[A]): MyList[A] =
    as match {
      case Cons(_, t) => t
      case _ => MyNil
    }

  def setHead[A](h: A, as: MyList[A]): MyList[A] = Cons(h, tail(as))

  def drop[A](as: MyList[A], n: Int): MyList[A] =
    if (n == 0) as
    else drop(tail(as), n - 1)

  def dropWhile[A](as: MyList[A])(f: A => Boolean): MyList[A] =
    as match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => as
    }

  def init[A](as: MyList[A]): MyList[A] =
    as match {
      case MyNil => MyNil
      case Cons(_, MyNil) => MyNil
      case Cons(h, t) => Cons(h, init(t))
    }

  // Not stack-safe
  def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B =
    as match {
      case MyNil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

  def product(ints: MyList[Int]) = foldRight(ints, 1)(_ * _)

  def sum(ints: MyList[Int]) = foldRight(ints, 0)(_ + _)

  // Stack-safe
  def foldLeft[A, B](as: MyList[A], z: B)(f: (B, A) => B): B =
    as match {
      case MyNil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def stackSafeProduct(ints: MyList[Int]) = foldLeft(ints, 1)(_ * _)

  def stackSafeSum(ints: MyList[Int]) = foldLeft(ints, 0)(_ + _)

  // -1 accounts for MyNil
  def length(ints: MyList[Int]) = foldLeft(ints, -1)((a, b) => 1 + b)

  def reverse[A](as: MyList[A]): MyList[A] = foldLeft(as, MyNil: MyList[A])((x, y) => Cons(y, x))

  def foldLeftInTermsOfRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B =
    foldRight(reverse(as), z)(f)

  def foldRightInTermsOfLeft[A, B](as: MyList[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def append[A](as: MyList[A], x: A): MyList[A] =
    foldRightInTermsOfLeft(as, Cons(x, MyNil): MyList[A])((a, b) => Cons(a, b))

  def flattenMyListOfMyLists[A](als: MyList[MyList[A]]): MyList[A] =
    als match {
      case MyNil => MyNil
      case Cons(MyNil, l) => flattenMyListOfMyLists(l)
      case Cons(Cons(h, t), t2) => Cons(h, flattenMyListOfMyLists(Cons(t, t2)))
    }

  def map[A, B](as: MyList[A])(f: A => B): MyList[B] =
    foldRight(as, MyNil: MyList[B])(
      (a, b) => Cons(f(a), b)
    )

  def filter[A](as: MyList[A])(f: A => Boolean): MyList[A] =
    foldRight(as, MyNil: MyList[A])(
      (a, b) => if (f(a)) Cons(a, b) else b
    )

  def flatMap[A, B](as: MyList[A])(f: A => MyList[B]): MyList[B] = flattenMyListOfMyLists(map(as)(f))

  def filterUsingFlatMap[A](as: MyList[A])(f: A => Boolean): MyList[A] =
    flatMap(as)(a => if (f(a)) MyList(a) else MyNil)

  def zipWith[A, B, C](as: MyList[A], bs: MyList[B])(f: (A, B) => C): MyList[C] =
    (as, bs) match {
      case (MyNil, _) => MyNil
      case (_, MyNil) => MyNil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

  def add1ToAll(ints: MyList[Int]): MyList[Int] =
    MyList.foldRight(ints, MyNil: MyList[Int])(
      (a, b) => Cons(1 + a, b)
    )

  def vectorSum(l1: MyList[Int], l2: MyList[Int]): MyList[Int] =
    (l1, l2) match {
      case (MyNil, MyNil) => MyNil
      case (MyNil, Cons(h, t)) => Cons(h, vectorSum(t, MyNil))
      case (Cons(h, t), MyNil) => Cons(h, t)
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, vectorSum(t1, t2))
    }

  import collection.immutable.List
  def hasSubSequence[A](l1: List[A], l2: List[A]): Boolean =
    (l1, l2) match {
      case (Nil, Nil) => true
      case (Nil, _) => false
      case (_, Nil) => true
      case (h1 :: t1, h2 :: t2) => {
        if (h1 == h2) hasSubSequence(t1, t2)
        else hasSubSequence(t1, h2 :: t2)
      }
    }
}