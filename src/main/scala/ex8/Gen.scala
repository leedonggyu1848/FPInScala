package ex8

import ex6.*

case class Gen[A](sample: State[RNG, A])

object Gen:
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(RNG.nonNegativeInt.map(n => start + n % (stopExclusive - start)))
  
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))
    
  def boolean: Gen[Boolean] =
    Gen(RNG.nonNegativeInt.map(n => n % 2 == 0))
    
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))