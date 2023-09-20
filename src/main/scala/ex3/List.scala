package ex3

enum List[+A]:
  case Nil extends List[Nothing]
  case Cons(head: A, tail: List[A])

object List:
  def sum(ints: List[Int]): Int = ints match
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)

  def product(ds: List[Double]): Double = ds match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)

  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  // ex 3.2
  def tail[A](l: List[A]): List[A] =
    ???

  // ex 3.3
  def setHead[A](l: List[A], h: A): List[A] =
    ???

  // ex 3.4
  def drop[A](l: List[A], n: Int): List[A] =
    ???

  // ex 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    ???

  // ex 3.6
  def init[A](l: List[A]): List[A] =
    ???

  // ex 3.9
  def length[A](l: List[A]): Int =
    ???

  // ex 3.10
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    ???

  // ex 3.11
  def sumViaFoldLeft(l: List[Int]): Int =
    ???

  def productViaFoldLeft(l: List[Double]): Double =
    ???

  // ex 3.12
  def reverse[A](l: List[A]): List[A] =
    ???

  // ex 3.13
  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    ???

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    ???

  // ex 3.14
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    ???

  // ex 3.15
  def concat[A](l: List[List[A]]): List[A] =
    ???

  // ex 3.16
  def addOneAllElements(l: List[Int]): List[Int] =
    ???

  // ex 3.17
  def doubleToString(l: List[Double]): List[String] =
    ???

  // ex 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] =
    ???

  // ex 3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    ???

  // ex 3.20
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    ???

  // ex 3.21
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    ???

  // ex 3.22
  def addPairwise(a: List[Int], b: List[Int]): List[Int] =
    ???

  // ex 3.23
  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] =
    ???
    
  // ex 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    ???