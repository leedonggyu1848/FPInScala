package ex3

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class ListTest extends AnyFlatSpecLike with Matchers {
  val fiveElements: ex3.List[Int] = ex3.List(1, 2, 3, 4, 5)
  val oneElement: ex3.List[Int] = ex3.List(1)
  val noElement: ex3.List[Nothing] = ex3.Nil

  behavior of "tail"
  it should "return the tail of a list" in {
    ex3.List.tail(ex3.List(1,2,3)) shouldBe ex3.List(2, 3)
  }
  it should "return Nil if the list is empty" in {
    ex3.List.tail(noElement) shouldBe ex3.Nil
  }
  it should "return Nil if the list has only one element" in {
    ex3.List.tail(oneElement) shouldBe ex3.Nil
  }

  behavior of "setHead"
  it should "replace head of a list" in {
    ex3.List.setHead(ex3.List(1,2,3), 4) shouldBe ex3.List(4, 2, 3)
  }
  it should "replace list of one element" in {
    ex3.List.setHead(oneElement, 4) shouldBe ex3.List(4)
  }
  it should "replace Nil" in {
    ex3.List.setHead(noElement, 4) shouldBe ex3.List(4)
  }

  behavior of "drop"
  it should "remove two elements" in {
    ex3.List.drop(ex3.List(1,2,3,4), 2) shouldBe ex3.List(3,4)
  }
  it should "remove elements of a list with n = length" in {
    ex3.List.drop(fiveElements, 5) shouldBe ex3.Nil
  }
  it should "remove elements of a list with n = 0" in {
    ex3.List.drop(fiveElements, 0) shouldBe fiveElements
  }
  it should "return Nil when dropping from Nil" in {
    ex3.List.drop(noElement, 2) shouldBe ex3.Nil
  }

  behavior of "dropWhile"
  it should "remove elements of a list" in {
    ex3.List.dropWhile(ex3.List(1,2,3,4,5), (x: Int) => x < 3) shouldBe ex3.List(3,4,5)
  }
  it should "remove all elements" in {
    ex3.List.dropWhile(fiveElements, _ => true) shouldBe ex3.Nil
  }

  behavior of "init"
  it should "return all elements but the last" in {
    ex3.List.init(ex3.List(1,2,3,4,5)) shouldBe ex3.List(1,2,3,4)
  }
  it should "return Nil when the list has only one element" in {
    ex3.List.init(oneElement) shouldBe ex3.Nil
  }
  it should "return Nil when the list is empty" in {
    ex3.List.init(noElement) shouldBe ex3.Nil
  }

  behavior of "length"
  it should "return the length of a list" in {
    ex3.List.length(fiveElements) shouldBe 5
  }
  it should "return 0 when the list is empty" in {
    ex3.List.length(noElement) shouldBe 0
  }
  it should "return 1 when the list has only one element" in {
    ex3.List.length(oneElement) shouldBe 1
  }

  behavior of "foldLeft"
  it should "return the sum of a list" in {
    ex3.List.foldLeft(ex3.List(1,2,3,4,5), 0)(_ + _) shouldBe 15
  }
  it should "return the product of a list" in {
    ex3.List.foldLeft(ex3.List(1,2,3,4,5), 1)(_ * _) shouldBe 120
  }
  it should "return the length of a list" in {
    ex3.List.foldLeft(ex3.List(1,2,3,4,5), 0)((acc, _) => acc + 1) shouldBe 5
  }
  it should "return the sum of a list of chars" in {
    ex3.List.foldLeft(ex3.List('a','b','c'), "")(_ + _) shouldBe "abc"
  }

  behavior of "sum and product"
  it should "return the sum of a list" in {
    ex3.List.sumViaFoldLeft(ex3.List(1,2,3,4,5)) shouldBe 15
    ex3.List.productViaFoldLeft(ex3.List(1,2,3,4,5)) shouldBe 120
  }

  behavior of "reverse"
  it should "return the reverse of a list" in {
    ex3.List.reverse(ex3.List(1,2,3,4,5)) shouldBe ex3.List(5,4,3,2,1)
  }

  behavior of "foldLeft and foldRight via eachother"
  it should "work" in {
    ex3.List.foldLeftViaFoldRight(ex3.List(1,2,3,4,5), 0)(_ + _) shouldBe 15
    ex3.List.foldRightViaFoldLeft(ex3.List(1,2,3,4,5), 0)(_ + _) shouldBe 15
  }

  behavior of "append"
  it should "append two lists" in {
    ex3.List.append(ex3.List(1,2,3), ex3.List(4,5)) shouldBe ex3.List(1,2,3,4,5)
  }

  behavior of "concat"
  it should "concat a list of lists" in {
    ex3.List.concat(ex3.List(ex3.List(1,2,3), ex3.List(4,5), ex3.List(6))) shouldBe ex3.List(1,2,3,4,5,6)
  }

  behavior of "addOneAllElements"
  it should "add one to each element of a list" in {
    ex3.List.addOneAllElements(ex3.List(1,2,3)) shouldBe ex3.List(2,3,4)
  }

  behavior of "doubleToString"
  it should "convert each element of a list to a string" in {
    ex3.List.doubleToString(ex3.List(1.0,2.0,3.0)) shouldBe ex3.List("1.0","2.0","3.0")
  }

  behavior of "map"
  it should "add one to each element of a list" in {
    ex3.List.map(ex3.List(1,2,3))(_ + 1) shouldBe ex3.List(2,3,4)
  }

  behavior of "filter"
  it should "remove odd numbers" in {
    ex3.List.filter(ex3.List(1,2,3,4,5))(_ % 2 == 0) shouldBe ex3.List(2,4)
  }

  behavior of "flatMap"
  it should "flatmap a list" in {
    ex3.List.flatMap(ex3.List(1,2,3))(i => ex3.List(i,i)) shouldBe ex3.List(1,1,2,2,3,3)
  }

  behavior of "filterViaFlatMap"
  it should "remove odd numbers" in {
    ex3.List.filterViaFlatMap(ex3.List(1,2,3,4,5))(_ % 2 == 0) shouldBe ex3.List(2,4)
  }

  behavior of "addPairwise"
  it should "add two lists" in {
    ex3.List.addPairwise(ex3.List(1,2,3), ex3.List(4,5,6)) shouldBe ex3.List(5,7,9)
  }

  behavior of "zipWith"
  it should "zip two lists" in {
    ex3.List.zipWith(ex3.List(1,2,3), ex3.List(4,5,6))(_ + _) shouldBe ex3.List(5,7,9)
    ex3.List.zipWith(ex3.List('a', 'b', 'c'), ex3.List('d', 'e', 'f'))(_.toString + _.toString) shouldBe ex3.List("ad", "be", "cf")
  }

  behavior of "hasSubsequence"
  it should "return true if the list contains the subsequence" in {
    ex3.List.hasSubsequence(ex3.List(1,2,3,4,5), ex3.List(2,3)) shouldBe true
    ex3.List.hasSubsequence(ex3.List(1,2,3,4,5), ex3.List(1,2,3,4,5)) shouldBe true
    ex3.List.hasSubsequence(ex3.List(1,2,3,4,5), ex3.List(5)) shouldBe true
  }
}
