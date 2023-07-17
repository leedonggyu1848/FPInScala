import org.scalatest.funsuite.AnyFunSuite

class MyListTest extends AnyFunSuite {

  private def fixtures = new {
    val fiveElements: MyList[Int] = MyList(1,2,3,4,5)
    val oneElement: MyList[Int] = MyList(1)
    val myNil: MyList[Nothing] = MyList()
  }

  test("sum test") {
    assert(MyList.sum(fixtures.fiveElements) == 15)
    assert(MyList.sum(fixtures.oneElement) == 1)
    assert(MyList.sum(fixtures.myNil) == 0)
  }

  test("product test") {
    val doubleTypeElements: MyList[Double] = MyList(1,2,3)
    val doubleTypeElement: MyList[Double] = MyList(2)

    assert(MyList.product(doubleTypeElements) == 6.0)
    assert(MyList.product(doubleTypeElement) == 2.0)
    assert(MyList.product(fixtures.myNil) == 1.0)
  }

  test("3.2 tail test") {
    assert(MyList.tail(fixtures.fiveElements) == MyList(2,3,4,5))
    assert(MyList.tail(fixtures.oneElement) == MyNil)
    assert(MyList.tail(fixtures.myNil) == MyNil)
  }

  test("3.3 setHead test") {
    assert(MyList.setHead(fixtures.fiveElements, 0) == MyList(0,2,3,4,5))
    assert(MyList.setHead(fixtures.oneElement, 10) == MyList(10))
    assert(MyList.setHead(fixtures.myNil, 0) == MyList(0))
  }

  test("3.4 drop test") {
    assert(MyList.drop(fixtures.fiveElements, 3) == MyList(4,5))
    assert(MyList.drop(fixtures.oneElement, 10) == MyNil)
    assert(MyList.drop(fixtures.myNil, 4) == MyNil)
    assert(MyList.drop(fixtures.fiveElements, 100) == MyNil)
    assert(MyList.drop(fixtures.fiveElements, 0) == fixtures.fiveElements)
  }

  test("3.5 dropWhile test") {
    assert(MyList.dropWhile(fixtures.fiveElements, (x:Int) => x <= 3) == MyList(4,5))
    assert(MyList.dropWhile(fixtures.myNil, (x:Int) => true) == MyNil);
  }

  test("3.6 init test") {
    assert(MyList.init(fixtures.fiveElements) == MyList(1,2,3,4))
    assert(MyList.init(fixtures.myNil) == MyNil)
    assert(MyList.init(fixtures.oneElement) == MyNil)
  }

  test("3.10 foldLeft test") {
    val lst1 = MyList(1,2,3,4,5)
    val lst2 = MyList('a','b','c','d','e')

    assert(MyList.foldLeft(lst1, 0)((acc:Int, emt: Int) => acc + emt) == 15)
    assert(MyList.foldLeft(lst2, "")(_ appended _) == "abcde")
    assert(MyList.foldRight(lst2, "")((emt: Char, acc: String) => acc.appended(emt)) == "edcba")
  }

  test("3.12 reverse") {
    assert(MyList.reverse(MyList(1,2,3,4,5)) == MyList(5,4,3,2,1))
  }

  test("3.13 fold Left & Right") {
    val lst2 = MyList('a','b','c','d','e')

    assert(MyList.foldLeftViaFoldRight(lst2, "")(_ appended _) == "abcde")
    assert(MyList.foldRightViaFoldLeft(lst2, "")((emt: Char, acc: String) => acc.appended(emt)) == "edcba")
  }

  test("3.14 append via fold") {
    val lst1 = MyList(1,2,3)
    val lst2 = MyList(4,5,6)
    assert(MyList.appendViaFold(lst1, lst2) == MyList(1,2,3,4,5,6))
  }

  test("3.15 concat") {
    val lst1 = MyList(1,2,3)
    val lst2 = MyList(4,5,6)
    val lst3 = MyList(7,8,9)
    assert(MyList.concat(MyList(lst1, lst2, lst3)) == MyList(1,2,3,4,5,6,7,8,9))
  }

  test("3.16 addOne") {
    val lst = MyList(1,2,3)
    assert(MyList.addOneEach(lst) == MyList(2,3,4))
  }

  test("3.17 doubleToString") {
    val lst = MyList(1.1, 1.2, 1.3)
    assert(MyList.doubleToString(lst) == MyList("1.1", "1.2", "1.3"))
  }

  test("3.18 map") {
    val lst = MyList(1,2,3,4,5)
    assert(MyList.map(lst)(e => e*e) == MyList(1,4,9,16,25))
  }

  test("3.19 filter") {
    val lst = MyList(1,2,3,4,5,6,7,8,9,10)
    assert(MyList.filter(lst)(e => e % 2 == 0) == MyList(2,4,6,8,10))
  }

  test("3.20 flatMap") {
    val lst = MyList(1,2,3)
    assert(MyList.flatMap(lst)(i => MyList(i, i)) == MyList(1,1,2,2,3,3))
  }

  test("3.21 filterViaFlatMap") {
    val lst = MyList(1,2,3,4,5,6,7,8,9,10)
    assert(MyList.filterViaFlatMap(lst)(e => e % 2 == 0) == MyList(2,4,6,8,10))
  }

  test("3.22 addZip") {
    val lst1 = MyList(1,2,3)
    val lst2 = MyList(4,5,6)
    val longLst = MyList(1,2,3,4,5)
    assert(MyList.addZip(lst1, lst2) == MyList(5,7,9))
    assert(MyList.addZip(lst2, longLst) == MyList(5,7,9))
  }

  test("3.23 zipWith") {
    val lst1 = MyList('a', 'b', 'c')
    val lst2 = MyList('d', 'e', 'f')
    val lst3 = MyList('d', 'e', 'f', 'a', 'b')
    assert(MyList.zipWith(lst1, lst2)((a, b) => a.toString+b.toString) == MyList("ad", "be", "cf"))
    assert(MyList.zipWith(lst1, lst3)((a, b) => a.toString+b.toString) == MyList("ad", "be", "cf"))
  }

  test("3.24 hasSubsequence") {
    val sup = MyList(1,2,3,4)
    val sub1 = MyList(1,2)
    val sub2 = MyList(2,3)
    val sub3 = MyList(4)
    val sub4 = MyList(2,3,4,5)
    assert(MyList.hasSubsequence(sup, sub1))
    assert(MyList.hasSubsequence(sup, sub2))
    assert(MyList.hasSubsequence(sup, sub3))
    assert(!MyList.hasSubsequence(sup, sub4))
  }
}
