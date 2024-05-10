package effects.instance

import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.{Await, Future}
import effects.All.*

import scala.concurrent.duration.Duration

class FutureTest extends AnyFunSuite {

  test("future test") {
    implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

    val f1 = Future {
      Thread.sleep(1000)
      "hello one"
    }

    val f2 = Future {
      Thread.sleep(1000)
      "hello two"
    }

//    val x = Await.ready (
      val x = f1.zipWith(f2)((a, b) => a ++ b)
//    )

  }
}
