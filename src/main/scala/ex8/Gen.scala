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