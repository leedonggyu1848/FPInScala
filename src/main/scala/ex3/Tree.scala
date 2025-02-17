package ex3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree:
  // ex 3.25
  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)

  // ex 3.26
  def maximum(t: Tree[Int]): Int = t match
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)

  // ex 3.27
  def depth[A](t: Tree[A]): Int = t match
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))

  // ex 3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))

  // ex 3.29
  def fold[A, B](t: Tree[A])(f: A => B, g: (B, B) => B): B = t match
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f, g), fold(r)(f, g))


  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1, _ + _)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(identity, _ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0, (l, r) => 1 + (l max r))

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(v => Leaf(f(v)): Tree[B], (a, b) => Branch(a, b))