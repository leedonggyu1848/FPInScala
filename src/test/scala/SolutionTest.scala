import org.scalatest.funsuite.AnyFunSuite

class SolutionTest extends AnyFunSuite {
  test("problem 2.1 fibonacci") {
    assert(Solution.fib(1) == 0)
    assert(Solution.fib(2) == 1)
    assert(Solution.fib(3) == 1)
    assert(Solution.fib(20) == 4181)
    assert(Solution.fib(25) == 46368)
  }

  test("problem 2.2 sorted") {
    val sortedArr: Array[Int] = Array(1,2,3,4,5)
    val unsortedArr: Array[Int] = Array(1,2,5,2,1)
    val uniformedArr: Array[Int] = Array(1,1,1,1,1)
    val emptyArr: Array[Int] = Array()
    val onceArr: Array[Int] = Array(1)
    val cmp: (Int, Int) => Boolean = (a, b) => a <= b
    assert(Solution.isSorted(sortedArr, cmp))
    assert(!Solution.isSorted(unsortedArr, cmp))
    assert(Solution.isSorted(uniformedArr, cmp))
    assert(Solution.isSorted(emptyArr, cmp))
    assert(Solution.isSorted(onceArr, cmp))
  }

  test("problem 2.3 curry") {
    val add: (Int, Int) => Int = (a, b) => a + b
    val mul: (Double, Int) => Double = (a, b) => a * b
    val curryAdd = Solution.curry(add)
    val curryMul = Solution.curry(mul)
    val addOne = curryAdd(1)
    assert(addOne(2) == 3)
    assert(curryMul(2.4)(2) == 4.8)
  }

  test("problem 2.4 uncurry") {
    val add: Int => Int => Int = a => b => a + b
    val mul: Double => Int => Double = a => b => a * b
    val uncurryAdd = Solution.uncurry(add)
    val uncurryMul = Solution.uncurry(mul)
    assert(uncurryAdd(1, 2) == 3)
    assert(uncurryMul(2.4, 2) == 4.8)
  }

  test("problem 2.5 compose") {
    val intToString: Int => String = i => i.toString
    val stringToInt: String => Int = s => s.toInt
    val stringToString:String => String =
      Solution.compose(intToString, stringToInt)
    val intToInt: Int => Int =
      Solution.compose(stringToInt, intToString)
    assert(stringToString("123") == "123")
    assert(intToInt(123) == 123)
  }
}
