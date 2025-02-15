package ex7

import ex7.Par
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.{Executors, Future, TimeUnit, TimeoutException}

class ParTest extends AnyFlatSpecLike with Matchers {
  behavior of "map2WithTimeout"
  it should "return a timeout if the computation takes too long" in {
    val f1: Par[Int] = es => es.submit(() => { Thread.sleep(100); 1 })
    val f2: Par[Int] = es => es.submit(() => { Thread.sleep(100); 2 })

    the[TimeoutException] thrownBy
      Par.map2WithTimeout(f1, f2) {
        _ + _
      }.run(Executors.newSingleThreadExecutor)
        .get(1, TimeUnit.NANOSECONDS)
  }

  behavior of "sequence"
  it should "return Par of List when given List of Par" in {
    val f1: Par[Int] = es => es.submit(() => { 1 })
    val f2: Par[Int] = es => es.submit(() => { 2 })
    val f3: Par[Int] = es => es.submit(() => { 3 })

    Par.sequence(ex3.List(f1, f2, f3)) shouldBe a [Par[ex3.List[_]]]
  }

  behavior of "parFilter"
  it should "return Par of List when given List of Par" in {
    Par.parFilter(ex3.List(1, 2, 3, 4))(_ % 2 == 0)
      .run(Executors.newSingleThreadExecutor).get() shouldBe ex3.List(2, 4)
  }

}