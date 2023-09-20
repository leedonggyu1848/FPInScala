package ex2

object Solution:
  // ex 2.1
  def fib(n: Int): Int =
    ???

  // ex 2.2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean =
    ???


  // ex 2.3
  def curry[A,B,C](f: (A,B) => C): A => (B => C) =
    ???

  // ex 2.4
  def uncurry[A,B,C](f: A => B => C): (A,B) => C =
    ???


  // ex 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    ???
