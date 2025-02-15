package ex7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

type Par[A] = ExecutorService => Future[A]

extension [A](p: Par[A])
  def run(s: ExecutorService): Future[A] = p(s)

  def map(f: A => A): Par[A] = Par.map(p)(f)
  def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] = Par.map2(p, b)(f)


object Par:
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A]:
    override def isDone: Boolean = true
    override def get(timeout: Long, units: TimeUnit): A = get
    override def isCancelled: Boolean = false
    override def cancel(evenIfRunning: Boolean): Boolean = false

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def map2[A,B,C](a: Par[A], b: Par[B])(f:(A,B) => C): Par[C] = es =>
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))

  def fork[A](a: => Par[A]): Par[A] = es =>
    es.submit(() => a(es).get)

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] =
    sequence(ps.map(asyncF(f)))
    
  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get
    
  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  // 7.3
  def map2WithTimeout[A,B,C](a: Par[A], b: Par[B])(f:(A,B) => C): Par[C] = es =>
    new Future[C]:
      val af: Future[A] = a(es)
      val bf: Future[B] = b(es)
      @volatile var cache: Option[C] = None

      override def cancel(mayInterruptIfRunning: Boolean): Boolean =
        af.cancel(mayInterruptIfRunning) || bf.cancel(mayInterruptIfRunning)

      override def isCancelled: Boolean =
        af.isCancelled || bf.isCancelled

      override def isDone: Boolean = true
//        cache.isDefined

      override def get(): C =
        get(Long.MaxValue, TimeUnit.NANOSECONDS)

      override def get(timeout: Long, unit: TimeUnit): C =
        val timeoutNano = TimeUnit.NANOSECONDS.convert(timeout, unit)
        val start = System.nanoTime
        val a = af.get(timeoutNano, TimeUnit.NANOSECONDS)
        val elapsed = System.nanoTime - start
        val b = bf.get(timeoutNano - elapsed, TimeUnit.NANOSECONDS)
        val c = f(a, b)
        cache = Some(c)
        c

  // 7.4
  def asyncF[A,B](f: A => B): A => Par[B] =
    es => lazyUnit(f(es))

  // 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List.empty[A]))((a, b) => map2(a, b)(_ :: _))

  // 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    as.foldRight(unit(List.empty[A]))((a, acc) => if f(a) then acc.map(a :: _) else acc)
