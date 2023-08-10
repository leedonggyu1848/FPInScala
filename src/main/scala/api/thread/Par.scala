package api.thread

import api.thread.{Callable, ExecutorService, Future}

import java.util.concurrent.TimeUnit


opaque type Par[A] = ExecutorService => Future[A]

object Par {
  def unit[A](a: A): Par[A] =
    es => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit): A = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  extension [A](a: Par[A]) {
    def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] =
      es => {
        val af = a(es)
        val bf = b(es)
        UnitFuture(f(af.get, bf.get))
      }
  }

  extension[A] (a: Par[A]) {
    def fork: Par[A] =
      (es: ExecutorService) => es.submit(new Callable[A] {
        def call: A = a(es).get
      })
  }

  extension [A](a: Par[A]) {
    def map[B](f: A => B): Par[B] =
      a.map2(unit(()))((a, _) => f(a))
  }

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    parList.map(_.sorted)

  // 7.3
  extension [A](a: Par[A]) {
    def map2WithTimeout[B, C](b: Par[B])(f: (A, B) => C): Par[C] =
      es => {
        new Future[C]:
          private val futureA = a(es)
          private val futureB = b(es)
          @volatile private var cache: Option[C] = None

          override def get: C =
            cache getOrElse {
              val c = f(futureA.get, futureB.get)
              cache = Some(c)
              c
            }

          override def get(timeout: Long, unit: TimeUnit): C =
            cache getOrElse {
              val timeoutNano = TimeUnit.NANOSECONDS.convert(timeout, unit)
              val start = System.nanoTime()
              val a = futureA.get(timeoutNano, TimeUnit.NANOSECONDS)
              val elapsed = System.nanoTime() - start
              val b = futureB.get(timeoutNano - elapsed, TimeUnit.NANOSECONDS)
              val c = f(a, b)
              cache = Some(c)
              c
            }

          override def cancel(evenIfRunning: Boolean): Boolean =
            futureA.cancel(evenIfRunning) || futureB.cancel(evenIfRunning)

          override def isDone: Boolean =
            cache.isDefined || futureA.isDone && futureB.isDone

          override def isCancelled: Boolean =
            futureA.isCancelled || futureB.isCancelled
      }
  }

  // 7.4
  def asyncF[A, B](f: A => B): A => Par[B] =
    es => lazyUnit(f(es))

  // 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List.empty[A]))((a, acc) => a.map2(acc)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork:
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(ps.map(asyncF(f)))

  // 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    as.foldRight(unit(List.empty[A]))((a, acc) => if f(a) then acc.map(a :: _) else acc)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get
    

}


