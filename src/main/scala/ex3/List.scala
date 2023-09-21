package ex3

import scala.annotation.tailrec

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

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))

  def append[A](l: List[A], r: List[A]): List[A] = l match
    case Nil => r
    case Cons(h, t) => Cons(h, append(t, r))

  // ex 3.2
  def tail[A](l: List[A]): List[A] = l match
    case Nil => Nil
    case Cons(_, t) => t

  // ex 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t)

  // ex 3.4
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if n == 0 then l
    else l match
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)

  // ex 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match
    case Cons(h, t) => if f(h) then dropWhile(t, f) else l
    case Nil => Nil

  // ex 3.6
  def init[A](l: List[A]): List[A] = l match
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))

  // ex 3.9
  def length[A](l: List[A]): Int = l match
    case Nil => 0
    case Cons(_, t) => 1 + length(t)

  // ex 3.10
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)


  // ex 3.11
  def sumViaFoldLeft(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def productViaFoldLeft(l: List[Double]): Double =
    foldLeft(l, 1.0)(_ * _)

  // ex 3.12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, emt) => Cons(emt, acc))

  // ex 3.13
  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, idf) => b => idf(f(b, a)))(z)

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((idf, a) => b => idf(f(a, b)))(z)

  // ex 3.14
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  // ex 3.15
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)

  // ex 3.16
  def addOneAllElements(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

  // ex 3.17
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  // ex 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  // ex 3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((h, t) => if f(h) then Cons(h, t) else t)

  // ex 3.20
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  // ex 3.21
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if f(a) then List(a) else Nil)

  // ex 3.22
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))

  // ex 3.23
  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a, b) match
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))

  // ex 3.24
  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(h1, t1), Cons(h2, t2)) =>
      if h1 == h2 then hasSubsequence(t1, t2)
      else hasSubsequence(t1, sub)

