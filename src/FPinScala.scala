import MyList._

object FPinScala extends App {

  def totalOrdered[A](arr: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def go(h: A, arrT: Array[A]): Boolean =
      if (arrT.length == 0) true
      else if (ordered(h, arrT.head)) go(arrT.head, arrT.tail)
      else false

    go(arr.head, arr.tail)
  }

  def curry[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

  val l = MyList(1, 2, 3, 4, 5)

  println(totalOrdered(Array(21, 42, 55), (x: Int, y: Int) => x <= y))

  // Reconstruct the list, foldRight is kind of the inverse of Cons: it unpacks
  println(MyList.foldRight(l, MyNil: MyList[Int])(Cons(_, _)))

  println(MyList.length(l))
  println(MyList.reverse(l))

  println(MyList.foldLeftInTermsOfRight(l, MyNil: MyList[Int])(Cons(_, _)))
  println(MyList.foldRightInTermsOfLeft(l, MyNil: MyList[Int])(Cons(_, _)))

  println(MyList.append(l, 7))

  val ls = MyList(MyNil: MyList[MyList[Int]], MyList(1, 2), MyList(3, 4))
  println(MyList.flattenMyListOfMyLists(ls))

  println(add1ToAll(l))

  val dl = MyList(1.1, 2.2, 3.14)
  // println(stringifyDoubleMyList(dl))
  println(MyList.map(dl)(2 * _))

  println(MyList.filter(l)(_ % 2 == 0))

  println(MyList.flatMap(l)(i => MyList(i, i)))

  println(MyList.filterUsingFlatMap(l)(_ % 2 == 0))

  val l2 = MyList(5, 4, 3, 2)
  println(vectorSum(l, l2))

  println(MyList.zipWith(l, dl)(_ * _))

  println(hasSubSequence(List(1, 2, 3, 4, 5, 6), List(2, 3, 4)))

  sealed trait Tree[+A]
  final case class Leaf[A](value: A) extends Tree[A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {

    def size[A](t: Tree[A]): Int =
      t match {
        case Leaf(_) => 1
        case Branch(left, right) => 1 + size(left) + size(right)
      }

    def maximum(t: Tree[Int]): Int =
      t match {
        case Leaf(value) => value
        case Branch(left, right) => maximum(left) max maximum(right)
      }

    def depth[A](t: Tree[A]): Int =
      t match {
        case Leaf(_) => 1
        case Branch(left, right) => (1 + depth(left)) max (1 + depth(right))
      }

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
      t match {
        case Leaf(value) => Leaf(f(value))
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      }

    def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
      t match {
        case Leaf(value) => f(value)
        case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
      }

    def sizeWithFold[A](t: Tree[A]): Int =
      fold(t)(x => 1)(1 + _ + _)

    def maxWithFold(t: Tree[Int]): Int =
      fold(t)(x => x)(_ max _)

    def depthWithFold[A](t: Tree[A]): Int =
      fold(t)(x => 1)((x, y) => 1 + (x max y))

    def mapWithFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
      fold(t)(x => Leaf(f(x)): Tree[B])(Branch(_, _))
  }

  val myTree: Tree[Int] =
    Branch(
      Branch(
        Leaf(0),
        Branch(
          Leaf(5),
          Leaf(42)
        )
      ),
      Leaf(1)
    )

  // println(Tree.size(myTree))
  // println(Tree.maximum(myTree))
  // println(Tree.depth(myTree))
  // println(Tree.map(myTree)(_ * 2))

  // println(Tree.sizeWithFold(myTree))
  // println(Tree.maxWithFold(myTree))
  // println(Tree.depthWithFold(myTree))
  // println(Tree.mapWithFold(myTree)(_ * 2))

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

  def mean(xs: Seq[Double]): MyOption[Double] = if (xs.length == 0) MyNone else MySome(xs.sum / xs.length)

  def variance(xs: Seq[Double]): MyOption[Double] = mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  val v1 = variance(Seq(1.0, 1.0, 15.0, 9.0))
  val v2 = variance(Seq(3))

  // println(MyOption.map2(v1, v2)(_ max _))
  // println(MyOption.sequence(List(v1, v2)))
  // println(MyOption.seqWithTraverse(List(v1, v2)))

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

  def meanEither(xs: Seq[Double]): MyEither[String, Double] = if (xs.isEmpty) MyLeft("Empty list") else MyRight(xs.sum / xs.length)

  def MyTry[A](a: => A): MyEither[Exception, A] =
    try MyRight(a)
    catch { case e: Exception => MyLeft(e) }

  // val mE1 = meanEither(Seq(0))
  // val mE2 = meanEither(Seq(3, 45, 88, 2.1))
  // println(mE1.map2(mE2)(_ + _) orElse MyRight(44))

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

  val s = MyStream(1, 2, 3, 4)

  // println(s.take(3).drop(1).safeToList)
  // println(s.takeWhile(_ < 3).safeToList)

  // println(s.forAll(_ < 9))
  // println(s.headOptionWithFoldRight)

  // println(s.map(2 * _).safeToList)
  // println(s.filter(_ % 2 == 0).safeToList)
  // println(s.append(5).safeToList)

  // println(s.flatMap(x => MyStream((1 to x): _*)).safeToList)

  println(MyStream.constant(3).take(4).safeToList)
}