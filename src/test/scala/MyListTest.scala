import org.scalatest.funsuite.AnyFunSuite

class MyListTest extends AnyFunSuite{

  private def fixtures = new {
    val fiveElements: MyList[Int] = MyList(1,2,3,4,5)
    val oneElement: MyList[Int] = MyList(1)
    val myNil: MyList[Nothing] = MyList()
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
    // assert(MyList.init)
    //TODO
  }
}
