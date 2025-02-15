package ex3

import ex3.Tree
import ex3.Tree.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class TreeTest extends AnyFlatSpecLike with Matchers {
  val tree1: Tree[Int] = Branch(Leaf(1), Leaf(2))
  val tree2: Tree[Int] = Leaf(1)
  val tree3: Tree[Int] = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))

  behavior of "size"
  it should "return the number of nodes in a tree" in {
    Tree.size(tree1) shouldBe 2
    Tree.size(tree2) shouldBe 1
    Tree.size(tree3) shouldBe 3
  }

  behavior of "maximum"
  it should "return the maximum element in a tree" in {
    Tree.maximum(tree1) shouldBe 2
    Tree.maximum(tree2) shouldBe 1
    Tree.maximum(tree3) shouldBe 3
  }

  behavior of "depth"
  it should "return the maximum path length from the root of a tree to any leaf" in {
    Tree.depth(tree1) shouldBe 1
    Tree.depth(tree2) shouldBe 0
    Tree.depth(tree3) shouldBe 2
  }

  behavior of "map"
  it should "apply a function to each element in a tree" in {
    Tree.map(tree1)(_ + 1) shouldBe Branch(Leaf(2), Leaf(3))
    Tree.map(tree2)(_ + 1) shouldBe Leaf(2)
    Tree.map(tree3)(_ + 1) shouldBe Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))
  }

  behavior of "fold"
  it should "behave like size" in {
    Tree.fold(tree1)(_ => 1, _ + _ + 1) shouldBe 3
    Tree.fold(tree2)(_ => 1, _ + _ + 1) shouldBe 1
    Tree.fold(tree3)(_ => 1, _ + _ + 1) shouldBe 5
  }

  behavior of "method via fold"
  it should "work same as the original method" in {
    Tree.sizeViaFold(tree1) shouldBe 2
    Tree.sizeViaFold(tree2) shouldBe 1
    Tree.sizeViaFold(tree3) shouldBe 3

    Tree.maximumViaFold(tree1) shouldBe 2
    Tree.maximumViaFold(tree2) shouldBe 1
    Tree.maximumViaFold(tree3) shouldBe 3

    Tree.depthViaFold(tree1) shouldBe 1
    Tree.depthViaFold(tree2) shouldBe 0
    Tree.depthViaFold(tree3) shouldBe 2
  }
}
