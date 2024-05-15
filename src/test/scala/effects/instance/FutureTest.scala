package effects.instance

import org.scalatest.funsuite.AnyFunSuite
import effects.All.*

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class FutureTest extends AnyFunSuite {

  test("future test") {
    implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

    val f1 = Future {
      for {
        _ <- ThreadIO.sleep(1000)
      } yield "First 1000"
    }

    val f2 = Future {
      for {
        _ <- ThreadIO.sleep(1000)
      } yield "Second 1000"
    }

    Await.ready (
       f1.zipWith(f2)((a, b) =>  b <*> (a <\> (x => x.concat(_)))), atMost =  Duration.Inf
    ).onComplete (v => {
      val effect = v.get >>= (res => println(res))
      IO.runEffect(effect)
    })
  }


  test("io future test") {
//    ThreadIO.sleep(1000).future.z
  }
}
