import java.util.concurrent.TimeUnit

object MyPar {

  type MyPar[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def isCancelled = false
    def get(timeout: Long, units: TimeUnit): A = get
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def unit[A](a: A): MyPar[A] = es => UnitFuture(a)

  def map2[A, B, C](a: MyPar[A], b: MyPar[B])(f: (A, B) => C): MyPar[C] =
    (es: ExecutorService) => {
      val (af, bf) = (a(es), b(es))
      UnitFuture(f(af.get, bf.get))
    }

  def map[A, B](a: MyPar[A])(f: A => B): MyPar[B] =
    map2(a, unit(()))((a, _) => f(a))

  def fork[A](a: => MyPar[A]): MyPar[A] =
    es => es.submit(
      new Callable[A] { def call = a(es).get }
    )

  def lazyUnit[A](a: => A): MyPar[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => MyPar[B] = a => lazyUnit(f(a))

  def sortPar(pints: MyPar[List[Int]]) = map(pints)(_.sorted)

  def sequence[A](ps: List[MyPar[A]]): MyPar[List[A]] =
    ps.foldRight(unit(Nil: List[A]))(map2(_, _)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): MyPar[List[B]] = {
    val fbs: List[MyPar[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](ps: List[A])(f: A => Boolean): MyPar[List[A]] = {
    val filteredPs = ps map (asyncF(a => if (f(a)) List(a) else Nil))
    map(sequence(filteredPs))(_.flatten)
  }

}

/*
 * Mocking the actual underlying implementation
 */

trait Callable[A] { def call: A }

trait Future[A] {
  def get: A
  def get(timeout: Long, units: TimeUnit): A
  def isDone: Boolean
  def isCancelled: Boolean
  def cancel(evenIfRunning: Boolean): Boolean
}

class ExecutorService {
  def submit[A](a: Callable[A]): Future[A] =
    MyPar.unit(a.call)(this)
}
