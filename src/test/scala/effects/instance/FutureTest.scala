package effects.instance

import org.scalatest.funsuite.AnyFunSuite
import effects.All.*
import effects.utils.DateIO
import effects.{Monad, Zip}

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
    implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

    IO.runEffect(DateIO.date.map(d => d.getTime) >>= println)

    val asFuture:Int => Future[Int] = (x: Int) => ThreadIO.sleep(1000).map(const(x)).future

    val f: Seq[Future[Int]] = (1 to 50).map(asFuture)
    val x: Future[Seq[Int]] =  Monad.sequence(f)
//    Await.ready (x, atMost =  Duration.Inf).onComplete(x => {
//      IO.runEffect(println(x.get))
//    })

//    x.onComplete(t => IO.runEffect(println(t.get)))

    x.onComplete(t => IO.runEffect (t.asIO >>= println))

    IO.runEffect(ThreadIO.sleep(5000))


  }
}
