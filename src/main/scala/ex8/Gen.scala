package ex8

import ex6.*

type Gen[A] = State[RNG, A]

object Gen:
  // 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    RNG.nonNegativeInt.map(n => start + n % (stopExclusive - start))

  // 8.5
  def unit[A](a: => A): Gen[A] =
    State.unit(a)
    
  def boolean: Gen[Boolean] =
    RNG.nonNegativeInt.map(n => n % 2 == 0)
    
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    State.sequence(List.fill(n)(g))

  // 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = ???

  //8.8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = ???

  // 8.6
  extension [A](g: Gen[A])
    def flatMap[B](f: A => Gen[B]): Gen[B] = rng =>
      val (a, nextRng) = g(rng)
      f(a)(nextRng)

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap(e => Gen.listOfN(e, g))