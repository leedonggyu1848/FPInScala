package ex2
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class SolutionTest extends AnyFlatSpecLike with Matchers {
  def fiboTest: Unit = {
    it should "return 0 for 1" in {
      Solution.fib(1) shouldBe 0
    }
    it should "return 1 for 2" in {
      Solution.fib(2) shouldBe 1
    }
    it should "return fibonacci number for any number" in {
      Solution.fib(3) shouldBe 1
      Solution.fib(4) shouldBe 2
      Solution.fib(5) shouldBe 3
      Solution.fib(6) shouldBe 5
    }
  }
  behavior of "fib"
  it should behave like fiboTest

  def isSortedTest: Unit = {
    val cmp = (a: Int, b: Int) => a <= b
    it should "return true for empty array" in {
      Solution.isSorted(Array.empty[Int], cmp) shouldBe true
    }
    it should "return true for array with one element" in {
      Solution.isSorted(Array(1), cmp) shouldBe true
    }
    it should "return true for sorted array" in {
      Solution.isSorted(Array(1, 2, 3, 4, 5), cmp) shouldBe true
    }
    it should "return false for unsorted array" in {
      Solution.isSorted(Array(1, 5, 2, 3, 4), cmp) shouldBe false
    }
    it should "return true for array with uniformed" in {
      Solution.isSorted(Array(1, 1, 1, 1, 1), cmp) shouldBe true
    }
  }

  behavior of "isSorted with simple compare function"
  it should behave like isSortedTest

  def uncurryTest(f: Int => Int => Int)(a: Int, b: Int): Unit = {
    val result = f(a)(b)
    it should s"return $result for $a and $b" in {
      Solution.uncurry(f)(a, b) shouldBe result
    }
  }
  behavior of "uncurry with add"
  {
    val addTest = uncurryTest(a => b => a + b)
    it should behave like addTest(1, 2)
    it should behave like addTest(-1, 2)
    it should behave like addTest(0, 0)
  }
  behavior of "uncurry with mul"
  {
    val mulTest = uncurryTest(a => b => a * b)
    it should behave like mulTest(3, 2)
    it should behave like mulTest(-3, 2)
    it should behave like mulTest(0, 0)
  }

  def curryTest(f: (Int, Int) => Int)(a: Int, b: Int): Unit = {
    val result = f(a, b)
    it should s"return $result for $a and $b" in {
      Solution.curry(f)(a)(b) shouldBe result
    }
  }
  behavior of "curry with add"
  {
    val addTest = curryTest(_ + _)
    it should behave like addTest(1, 2)
    it should behave like addTest(-1, 2)
    it should behave like addTest(0, 0)
  }
  behavior of "curry with mul"
  {
    val mulTest = curryTest(_ * _)
    it should behave like mulTest(3, 2)
    it should behave like mulTest(-3, 2)
    it should behave like mulTest(0, 0)
  }

  def composeTest(f: Int => Int, g: Int => Int)(a: Int): Unit = {
    val result = f(g(a))
    it should s"return $result for $a" in {
      Solution.compose(f, g)(a) shouldBe result
    }
  }
  behavior of "compose with inverse function"
  it should "return same value as input" in {
    val intToString: Int => String = i => i.toString
    val stringToInt: String => Int = s => s.toInt
    Solution.compose(intToString, stringToInt)("123") shouldBe "123"
    Solution.compose(stringToInt, intToString)(123) shouldBe 123
  }
}

