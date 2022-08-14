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
  @tailrec
  def drop[A](l: MyList[A], n: Int): MyList[A] =
    if (n == 0) l
    else l match {
      case MyNil => MyNil
      case MyCons(_, xs) => drop(xs, n-1)
    }

  // 3.5
  @tailrec
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

  // 3.11
  def sum3(ns: MyList[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: MyList[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  // 3.12
  def reverse[A](as: MyList[A]): MyList[A] =
    foldLeft(as, MyList[A]())((lst: MyList[A], emt: A) => MyCons(emt, lst))

  // 3.13
  def foldLeftViaFoldRight[A, B](as: MyList[A], z: B)(f: (B, A) => B): B =
   foldRight(as, (i: B) => i)( (a, idf) => b => idf( f(b, a)) )(z)

  def foldRightViaFoldLeft[A, B](as: MyList[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, (i: B) => i)( (idf, a) => b => idf(f(a, b)) )(z)

  // 3.14
  def appendViaFold[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
    foldLeft(MyList.reverse(a1), a2)((as, e) => MyCons(e, as))

  // 3.15
  def concat[A](lst: MyList[MyList[A]]): MyList[A] =
    foldRight(lst, MyNil: MyList[A])(append)

  // 3.16
  def addOneEach(lst: MyList[Int]): MyList[Int] =
    foldRight(lst, MyNil: MyList[Int])((emt, lst) => MyCons(emt+1, lst))

  // 3.17
  def doubleToString(lst: MyList[Double]): MyList[String] =
    foldRight(lst, MyNil: MyList[String])((emt, lst) => MyCons(emt.toString, lst))

  // 3.18
  def map[A, B](as: MyList[A])(f: A => B): MyList[B] =
    foldRight(as, MyNil: MyList[B])((emt, lst) => MyCons(f(emt), lst))

  // 3.19
  def filter[A](as: MyList[A])(f: A => Boolean): MyList[A] =
    foldRight(as, MyNil: MyList[A])((emt, lst) => if (f(emt)) MyCons(emt, lst) else lst)

  // 3.20
  def flatMap[A, B](as: MyList[A])(f: A => MyList[A]): MyList[A] =
    concat(map(as)(f))

  // 3.21
  def filterViaFlatMap[A](as: MyList[A])(f: A => Boolean): MyList[A] = {
    flatMap(as)(a => if(f(a)) MyList(a) else MyNil)
  }

  // 3.22
  def addZip(a1: MyList[Int], a2: MyList[Int]): MyList[Int] = (a1, a2) match {
    case (_, MyNil) => MyNil
    case (MyNil, _) => MyNil
    case (MyCons(h1, t1), MyCons(h2, t2)) => MyCons(h1+h2, addZip(t1, t2))
  }

  // 3.23
  def zipWith[A, B, C](a1: MyList[A], a2: MyList[B])(f: (A, B) => C): MyList[C] = (a1, a2) match {
    case (_, MyNil) => MyNil: MyList[C]
    case (MyNil, _) => MyNil: MyList[C]
    case(MyCons(h1, t1), MyCons(h2, t2)) => MyCons(f(h1, h2), zipWith(t1, t2)(f))
  }

  // 3.24
  @tailrec
  def isStartedWith[A](sup: MyList[A], sub: MyList[A]): Boolean = (sup, sub) match {
    case (_, MyNil) => true
    case (MyCons(h1, t1), MyCons(h2, t2)) if h1 == h2 => isStartedWith(t1, t2)
    case _ => false
  }
  @tailrec
  def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean = sup match {
    case MyNil => sub == MyNil
    case _ if isStartedWith(sup, sub) => true
    case MyCons(h, t) => hasSubsequence(t, sub)

  }
}
