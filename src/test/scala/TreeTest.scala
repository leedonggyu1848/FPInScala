import org.scalatest.funsuite.AnyFunSuite

class TreeTest extends AnyFunSuite{
  private def fixtures = new {
    val tree1: Branch[Int] = Branch(Leaf(1), Leaf(2))
    val tree2: Leaf[Int] = Leaf(1)
    val tree3: Branch[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
  }
  test("3.25 size") {

    assert(Tree.size(fixtures.tree1) == 2)
    assert(Tree.size(fixtures.tree2) == 1)
    assert(Tree.size(fixtures.tree3) == 3)
  }

  test("3.26 maximum") {
    assert(Tree.maximum(fixtures.tree1) == 2)
    assert(Tree.maximum(fixtures.tree2) == 1)
    assert(Tree.maximum(fixtures.tree3) == 3)
  }

  test("3.27 depth") {
    assert(Tree.depth(fixtures.tree1) == 1)
    assert(Tree.depth(fixtures.tree2) == 0)
    assert(Tree.depth(fixtures.tree3) == 2)
  }

  test("3.28 map") {
    assert(Tree.map(fixtures.tree1)(_+1) == Branch(Leaf(2), Leaf(3)))
    assert(Tree.map(fixtures.tree2)(_+1) == Leaf(2))
    assert(Tree.map(fixtures.tree3)(_+1) == Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)) )
  }

  test("3.29 method reconstruct via fold") {
    assert(Tree.maximumViaFold(fixtures.tree1) == 2)
    assert(Tree.maximumViaFold(fixtures.tree2) == 1)
    assert(Tree.maximumViaFold(fixtures.tree3) == 3)

    assert(Tree.depthViaFold(fixtures.tree1) == 1)
    assert(Tree.depthViaFold(fixtures.tree2) == 0)
    assert(Tree.depthViaFold(fixtures.tree3) == 2)

    assert(Tree.mapViaFold(fixtures.tree1)(_ + 1) == Branch(Leaf(2), Leaf(3)))
    assert(Tree.mapViaFold(fixtures.tree2)(_ + 1) == Leaf(2))
    assert(Tree.mapViaFold(fixtures.tree3)(_ + 1) == Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))
  }

}
