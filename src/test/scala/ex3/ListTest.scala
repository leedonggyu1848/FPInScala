package ex3

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class ListTest extends AnyFlatSpecLike with Matchers {
  val fiveElements: List[Int] = List(1, 2, 3, 4, 5)
  val oneElement: List[Int] = List(1)
  val noElement: List[Nothing] = List.Nil

  behavior of "tail"
  it should "return the tail of a list" in {
    List.tail(List(1,2,3)) shouldBe List(2, 3)
  }
  it should "return Nil if the list is empty" in {
    List.tail(noElement) shouldBe List.Nil
  }
  it should "return Nil if the list has only one element" in {
    List.tail(oneElement) shouldBe List.Nil
  }

  behavior of "setHead"
  it should "replace head of a list" in {
    List.setHead(List(1,2,3), 4) shouldBe List(4, 2, 3)
  }
  it should "replace list of one element" in {
    List.setHead(oneElement, 4) shouldBe List(4)
  }
  it should "replace Nil" in {
    List.setHead(noElement, 4) shouldBe List(4)
  }

  behavior of "drop"
  it should "remove two elements" in {
    List.drop(List(1,2,3,4), 2) shouldBe List(3,4)
  }
  it should "remove elements of a list with n = length" in {
    List.drop(fiveElements, 5) shouldBe List.Nil
  }
  it should "remove elements of a list with n = 0" in {
    List.drop(fiveElements, 0) shouldBe fiveElements
  }
  it should "return Nil when dropping from Nil" in {
    List.drop(noElement, 2) shouldBe List.Nil
  }

  behavior of "dropWhile"
  it should "remove elements of a list" in {
    List.dropWhile(List(1,2,3,4,5), (x: Int) => x < 3) shouldBe List(3,4,5)
  }
  it should "remove all elements" in {
    List.dropWhile(fiveElements, _ => true) shouldBe List.Nil
  }

  behavior of "init"
  it should "return all elements but the last" in {
    List.init(List(1,2,3,4,5)) shouldBe List(1,2,3,4)
  }
  it should "return Nil when the list has only one element" in {
    List.init(oneElement) shouldBe List.Nil
  }
  it should "return Nil when the list is empty" in {
    List.init(noElement) shouldBe List.Nil
  }

  behavior of "length"
  it should "return the length of a list" in {
    List.length(fiveElements) shouldBe 5
  }
  it should "return 0 when the list is empty" in {
    List.length(noElement) shouldBe 0
  }
  it should "return 1 when the list has only one element" in {
    List.length(oneElement) shouldBe 1
  }

  behavior of "foldLeft"
  it should "return the sum of a list" in {
    List.foldLeft(List(1,2,3,4,5), 0)(_ + _) shouldBe 15
  }
  it should "return the product of a list" in {
    List.foldLeft(List(1,2,3,4,5), 1)(_ * _) shouldBe 120
  }
  it should "return the length of a list" in {
    List.foldLeft(List(1,2,3,4,5), 0)((acc, _) => acc + 1) shouldBe 5
  }
  it should "return the sum of a list of chars" in {
    List.foldLeft(List('a','b','c'), "")(_ + _) shouldBe "abc"
  }

  behavior of "sum and product"
  it should "return the sum of a list" in {
    List.sumViaFoldLeft(List(1,2,3,4,5)) shouldBe 15
    List.productViaFoldLeft(List(1,2,3,4,5)) shouldBe 120
  }

  behavior of "reverse"
  it should "return the reverse of a list" in {
    List.reverse(List(1,2,3,4,5)) shouldBe List(5,4,3,2,1)
  }

  behavior of "foldLeft and foldRight via eachother"
  it should "work" in {
    List.foldLeftViaFoldRight(List(1,2,3,4,5), 0)(_ + _) shouldBe 15
    List.foldRightViaFoldLeft(List(1,2,3,4,5), 0)(_ + _) shouldBe 15
  }

  behavior of "append"
  it should "append two lists" in {
    List.append(List(1,2,3), List(4,5)) shouldBe List(1,2,3,4,5)
  }

  behavior of "concat"
  it should "concat a list of lists" in {
    List.concat(List(List(1,2,3), List(4,5), List(6))) shouldBe List(1,2,3,4,5,6)
  }

  behavior of "addOneAllElements"
  it should "add one to each element of a list" in {
    List.addOneAllElements(List(1,2,3)) shouldBe List(2,3,4)
  }

  behavior of "doubleToString"
  it should "convert each element of a list to a string" in {
    List.doubleToString(List(1.0,2.0,3.0)) shouldBe List("1.0","2.0","3.0")
  }

  behavior of "map"
  it should "add one to each element of a list" in {
    List.map(List(1,2,3))(_ + 1) shouldBe List(2,3,4)
  }

  behavior of "filter"
  it should "remove odd numbers" in {
    List.filter(List(1,2,3,4,5))(_ % 2 == 0) shouldBe List(2,4)
  }

  behavior of "flatMap"
  it should "flatmap a list" in {
    List.flatMap(List(1,2,3))(i => List(i,i)) shouldBe List(1,1,2,2,3,3)
  }

  behavior of "filterViaFlatMap"
  it should "remove odd numbers" in {
    List.filterViaFlatMap(List(1,2,3,4,5))(_ % 2 == 0) shouldBe List(2,4)
  }

  behavior of "addPairwise"
  it should "add two lists" in {
    List.addPairwise(List(1,2,3), List(4,5,6)) shouldBe List(5,7,9)
  }

  behavior of "zipWith"
  it should "zip two lists" in {
    List.zipWith(List(1,2,3), List(4,5,6))(_ + _) shouldBe List(5,7,9)
    List.zipWith(List('a', 'b', 'c'), List('d', 'e', 'f'))(_.toString + _.toString) shouldBe List("ad", "be", "cf")
  }

  behavior of "hasSubsequence"
  it should "return true if the list contains the subsequence" in {
    List.hasSubsequence(List(1,2,3,4,5), List(2,3)) shouldBe true
    List.hasSubsequence(List(1,2,3,4,5), List(1,2,3,4,5)) shouldBe true
    List.hasSubsequence(List(1,2,3,4,5), List(5)) shouldBe true
  }
}
