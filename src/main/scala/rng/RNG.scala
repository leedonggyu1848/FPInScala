package rng

import rng.State.unit
import scala.annotation.tailrec

sealed trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0XFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

// 6.10
case class State[S,+A](run: S => (A,S)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, next) = run(s)
      f(a).run(next)
    })
  def map[B](f: A => B): State[S, B] =
      flatMap(e => unit(f(e)))
  def map2[B, C](state: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => state.map(b => f(a, b)))

}

object State {
  def unit[S,A](a: A): State[S,A] =
    State(state => (a, state))
  def sequence[S, A](states: List[State[S, A]]): State[S, List[A]] =
    states.foldRight(unit[S, List[A]](List.empty))((v, acc) => v.map2(acc)(_ :: _))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  // 6.1
  @tailrec
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRng) = rng.nextInt
    if (n >= 0) (n, nextRng) else nonNegativeInt(nextRng)
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def int: Rand[Int] =
    _.nextInt

  // 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRng) = RNG.nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), nextRng)
  }

  // 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = r1.nextInt
    ((d, i), r2)
  }
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  // 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0)
      (List.empty, rng)
    else {
      val (n, nextRng) = rng.nextInt
      val (tail, retRng) = ints(count - 1)(nextRng)
      (n :: tail, retRng)
    }
  }

  // 6.5
  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  // 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    (rng: RNG) => {
      val (rav, rar) = ra(rng)
      val (rbv, rbr) = rb(rar)
      (f(rav, rbv), rbr)
    }
  }
  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  def randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  // 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A]))((v, acc) => map2(v, acc)(_ :: _))

  def intsViaSequence(count: Int)(rng: RNG): (List[Int], RNG) = {
    sequence(List.fill(count)(int))(rng)
  }

  // 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, nextRng) = f(rng)
      g(a)(nextRng)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    })

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(e => unit(f(e)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => mapViaFlatMap(rb)(b => f(a, b)))

  def bothViaFlatMap[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2ViaFlatMap(ra, rb)((_, _))

  def randIntDoubleViaFlatMap: Rand[(Int, Double)] =
    bothViaFlatMap(int, double)

  def randDoubleIntViaFlatMap: Rand[(Double, Int)] =
    bothViaFlatMap(double, int)
}
