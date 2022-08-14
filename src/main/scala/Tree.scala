sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // 3.25
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right)
  }

  // 3.26
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  // 3.27
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) => (depth(left) max depth(right)) + 1
  }

  // 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  // 3.29
  def fold[A, B](tree: Tree[A])(f: A => B, g: (B, B) => B): B = tree match {
    case Leaf(v) => f(v)
    case Branch(left, right) => g( fold(left)(f, g), fold(right)(f, g))
  }

  def maximumViaFold(tree: Tree[Int]): Int =
    fold[Int, Int](tree)(x => x, _ max _)

  def depthViaFold[A](tree: Tree[A]): Int =
    fold[A, Int](tree)(_=>0, (x, y) => (x max y) + 1)

  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](tree)(x => Leaf(f(x)), (x, y) => Branch(x, y))

}