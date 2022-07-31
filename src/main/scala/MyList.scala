import scala.annotation.tailrec

sealed trait MyList[+A]
case object MyNil extends MyList[Nothing]
case class MyCons[+A] (head: A, tail: MyList[A]) extends MyList[A]

object MyList {
  def sum(ints: MyList[Int]): Int = ints match {
    case MyNil => 0
    case MyCons(x, xs) => x + sum(xs)
  }

  def product(ds: MyList[Double]): Double = ds match {
    case MyNil => 1.0
    case MyCons(0.0, _) => 0.0
    case MyCons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) MyNil
    else MyCons(as.head, apply(as.tail: _*))

  // 3.2
  def tail[A](lst: MyList[A]): MyList[A] = lst match {
    case MyNil => MyNil
    case MyCons(_, xs) => xs
  }

  // 3.3
  def setHead[A](lst: MyList[A], head: A): MyList[A] = lst match {
    case MyNil => MyCons(head, MyNil)
    case MyCons(_, xs) => MyCons(head, xs)
  }

  // 3.4
  def drop[A](l: MyList[A], n: Int): MyList[A] =
    if (n == 0) l
    else l match {
      case MyNil => MyNil
      case MyCons(_, xs) => drop(xs, n-1)
    }

  // 3.5
  def dropWhile[A](l: MyList[A], f: A => Boolean): MyList[A] = l match {
    case MyNil => MyNil
    case MyCons(h, t) => if(f(h)) dropWhile(t, f) else l
  }


  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
    a1 match {
      case MyNil => a2
      case MyCons(h, t) => MyCons(h, append(t, a2))
    }

  // 3.6
  def init[A](l: MyList[A]): MyList[A] = l match {
    case MyNil => MyNil
    case MyCons(_, MyNil) => MyNil
    case MyCons(h, t) => MyCons(h, init(t))
  }

  def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B =
    as match {
      case MyNil => z
      case MyCons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: MyList[Int]): Int =
    foldRight(ns, 0)(_ + _)

  def product2(ns: MyList[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  // 3.10
  @tailrec
  def foldLeft[A, B](as: MyList[A], z: B)(f: (B,A) => B): B =
    as match {
      case MyNil => z
      case MyCons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
}
