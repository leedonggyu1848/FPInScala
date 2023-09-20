package ex3

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])


object Tree:
  // ex 3.25
  def size[A](t: Tree[A]): Int =
    ???

  // ex 3.26
  def maximum(t: Tree[Int]): Int =
    ???

  // ex 3.27
  def depth[A](t: Tree[A]): Int =
    ???

  // ex 3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    ???

  // ex 3.29
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
    ???

  def sizeViaFold[A](t: Tree[A]): Int =
    ???

  def maximumViaFold(t: Tree[Int]): Int =
    ???

  def depthViaFold[A](t: Tree[A]): Int =
    ???

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    ???