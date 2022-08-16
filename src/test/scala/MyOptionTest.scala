import org.scalatest.funsuite.AnyFunSuite

class MyOptionTest extends AnyFunSuite {
  def fixtures = new {
    val some:MySome[Int] = MySome(10)
    val none: MyOption[Nothing] = MyNone
  }
  test("4.1") {
    //map
    assert(fixtures.some.map(_ + 1) == MySome(11))

    //getOrElse
    assert(fixtures.some.getOrElse(0) == 10)
    assert(fixtures.none.getOrElse(0) == 0)

    //flatMap
    assert(fixtures.some.flatMap(MySome(_)) == MySome(10))
    assert(fixtures.none.flatMap(MySome(_)) == MyNone)

    // orElse
    assert(fixtures.some.orElse(MyNone) == MySome(10))
    assert(fixtures.none.orElse(MyNone) == MyNone)

    //filter
    assert(fixtures.some.filter(x => true) == MySome(10))
    assert(fixtures.some.filter(x => false) == MyNone)
    assert(fixtures.none.filter(x=>true) == MyNone)
  }

   test("4.2 variance") {
    assert(MyOption.variance(Seq(1,2,3,4,5)).getOrElse(MyNone) == 2.0)
    assert(MyOption.variance(Seq()).getOrElse(MyNone) == MyNone)
  }

  test("4.3 map2") {
    assert(MyOption.map2(MySome(1), MySome(2))((x, y) => x+y) == MySome(3))
    assert(MyOption.map2[Int, Int, Int](MyNone, MySome(2))((x, y) => x+y) == MyNone)
    assert(MyOption.map2[Int, Int, Int](MySome(1), MyNone)((x, y) => x+y) == MyNone)
  }

  test("4.4 sequence") {
    val lst1: List[MyOption[Int]] = List(MySome(1),MySome(1),MySome(1),MySome(1))
    val lst2: List[MyOption[Int]] = List(MySome(1),MySome(1),MyNone,MySome(1))
    assert(MyOption.sequence(lst1).getOrElse(Nil).length == 4)
    assert(MyOption.sequence(lst2).getOrElse(Nil).isEmpty)
  }

  test("4.5 traverse") {
    val lst: List[Int] = List(1,1,1)
    assert(MyOption.traverse(lst)(x => MySome(x+1)) == MySome(List(2,2,2)))

    val lst1: List[MyOption[Int]] = List(MySome(1),MySome(1),MySome(1),MySome(1))
    val lst2: List[MyOption[Int]] = List(MySome(1),MySome(1),MyNone,MySome(1))
    assert(MyOption.sequenceViaTraverse(lst1).getOrElse(Nil).length == 4)
    assert(MyOption.sequenceViaTraverse(lst2).getOrElse(Nil).isEmpty)
  }
}
