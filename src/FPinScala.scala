import MyEither._
import MyList._
import MyOption._
import MyRNG._
import MyStream._
import Tree._

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

  /*
  * EXERCISES
  */

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
  println(vectorSum(l, l2))

  println(MyList.zipWith(l, dl)(_ * _))
  println(hasSubSequence(List(1, 2, 3, 4, 5, 6), List(2, 3, 4)))

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

  println(Tree.size(myTree))
  println(Tree.maximum(myTree))
  println(Tree.depth(myTree))
  println(Tree.map(myTree)(_ * 2))
  println(Tree.sizeWithFold(myTree))
  println(Tree.maxWithFold(myTree))
  println(Tree.depthWithFold(myTree))
  println(Tree.mapWithFold(myTree)(_ * 2))

  def mean(xs: Seq[Double]): MyOption[Double] = if (xs.length == 0) MyNone else MySome(xs.sum / xs.length)

  def variance(xs: Seq[Double]): MyOption[Double] = mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  val v1 = variance(Seq(1.0, 1.0, 15.0, 9.0))
  val v2 = variance(Seq(3))

  println(MyOption.map2(v1, v2)(_ max _))
  println(MyOption.sequence(List(v1, v2)))
  println(MyOption.seqWithTraverse(List(v1, v2)))

  def meanEither(xs: Seq[Double]): MyEither[String, Double] = if (xs.isEmpty) MyLeft("Empty list") else MyRight(xs.sum / xs.length)

  def MyTry[A](a: => A): MyEither[Exception, A] =
    try MyRight(a)
    catch { case e: Exception => MyLeft(e) }

  val mE1 = meanEither(Seq(0))
  val mE2 = meanEither(Seq(3, 45, 88, 2.1))
  println(mE1.map2(mE2)(_ + _) orElse MyRight(44))

  val s = MyStream(1, 2, 3, 4)

  println(s.take(3).drop(1).safeToList)
  println(s.takeWhile(_ < 3).safeToList)

  println(s.forAll(_ < 9))
  println(s.headOptionWithFoldRight)

  println(s.map(2 * _).safeToList)
  println(s.filter(_ % 2 == 0).safeToList)
  println(s.append(5).safeToList)

  println(s.flatMap(x => MyStream((1 to x): _*)).safeToList)

  println(MyStream.constant(3).take(4).safeToList)
  println(MyStream.from(3).take(4).safeToList)
  println(MyStream.fibs.take(10).safeToList)

  println(MyStream.fibsViaUnfold.take(10).safeToList)
  println(MyStream.fromViaUnfold(3).take(4).safeToList)
  println(MyStream.constantViaUnfold(3).take(4).safeToList)

  println(s.mapViaUnfold(2 * _).safeToList)
  println(s.takeViaUnfold(3).safeToList)
  println(s.takeWhileViaUnfold(_ < 3).safeToList)

  println(s.zipWith(s)(_ * _).safeToList)
  println(
    MyStream.fibs.take(5)
    .zipAll(MyStream.fibs.take(3))
    .safeToList
  )

  println(s.startsWith(s.take(3)))
  println(s.tails.map(_.safeToList).safeToList)
  println(s hasSubsequence s.drop(1))

  println(s.scanRight(0)(_ + _).safeToList)

  val rng = new MySimpleRNG(-42)
  println(MyRNG.ints(5)(rng))

  val randSeq = MyRNG.sequence(List(MyRNG.int, MyRNG.nonNegativeEven))
  println(randSeq(rng))
}
