package effects.instance

import effects.Functor.*
import effects.FunctorMap
import effects.instance.IterableInstances.*
import org.scalatest.funsuite.AnyFunSuite

class SeqTest extends AnyFunSuite {

  val addOne: Int => Int = _ + 1

  test("map as Seq functor using map and infix") {
    val s = Seq(1, 2, 3, 4)

    IterableTypeClasses(s)
    
    assert(s.map(addOne) == Seq(2, 3, 4, 5))
    assert(s <\> addOne == Seq(2, 3, 4, 5))
  }


  test("map as Seq functor using liftF") {
    val s = Seq(1, 2, 3, 4)
    val addOneF: FunctorMap[Int, Int] = liftF(addOne)
    val sf = addOneF(s)

    assert(sf == Seq(2, 3, 4, 5))
  }

  test("map as List functor using liftF (Inheritance") {
    val s = List(1, 2, 3, 4)
    val addOne: Int => Int = _ + 1
    val addOneF: FunctorMap[Int, Int] = liftF(addOne)
    val sf = addOneF(s)

    assert(sf == List(2, 3, 4, 5))
  }


  test("ap as applicative") {
    val s = Seq(1, 2, 3, 4)
    val fs: Seq[Int => Int] = Seq(_ + 1, _ * 3)
    val expected = Seq(2, 3, 4, 5, 3, 6, 9, 12)
    assert(s <*> fs == expected)
    assert(s.ap(fs) == expected)
  }

  test("monad on Seq") {
    val res = for {
      x <- Seq(1, 2, 3, 4)
      y <- Seq(100, 200)
    } yield x + y

    assert(res == List(101, 201, 102, 202, 103, 203, 104, 204))
  }
}
