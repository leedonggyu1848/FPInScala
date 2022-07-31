import annotation.tailrec

object Solution {
  // 2.1
  @tailrec
  def fib(n: Int, first: Int = 0, second: Int = 1): Int =
    if (n <= 1) first
    else fib(n-1, second, first + second)

  // 2.2
  @tailrec
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean) :Boolean =
    if (as.isEmpty) true
    else if (as.tail.isEmpty) true
    else if (!ordered(as.head, as.tail.head)) false
    else isSorted(as.tail, ordered)

  // 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  // 2.4
  def uncurry[A,B,C](f:A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

}
