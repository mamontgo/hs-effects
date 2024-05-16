package effects

import effects.All.*
import effects.instance.IO
import org.scalatest.funsuite.AnyFunSuite

import scala.io.BufferedSource
import scala.util.{Try, Using}

class Sandbox  extends AnyFunSuite {


  def foldEither(e: Either[String, Int]): Int = {
    e.fold[Int](_=>0 , identity)
  }

  def mapEither(e:Either[String, Int]): Either[String, Int] = {
    e.map(_+1)
  }

  test("fold either") {
    
    val x = Try {
      "hello"
    }
    
    IO.runEffect {
      println(foldEither(Left("Error")))
    }

    def fst[A, B](a:A, b:B):A = a
    def snd[A, B](a:A, b:B):B = b
  }



  def appendWorld(x: String): String = {
    println(s"appending world to $x")
    x ++ "world"
  }

  def lazyFunction[A,B](f: A => B): A => B = {
    (a:A) => {
      lazy val b = f(a)
      b
    }

  }



  test("inheritance for functions") {

    def getEmpty[F[_], A](implicit empty: Empty[F, A]): F[A] = {
      empty()
    }

    val res:IO[Seq[Int]] = getEmpty[IO, Seq[Int]]
    val m = res.map(s => s ++ Seq(1,2))
    val e = IO.runEffect{
      m
    }

    println(e)

  }


  test("thing?") {
    class Thing[A](thunk: => A) {

      val job = () => thunk

      def run: A = {
        job()
      }
    }

    val tuple = (123, "HELLO")


    val x =  Thing { "Hello" }

    x.run
    println("we did ex")

  }

  test("testing string") {
    val s:String = "Hello"
    println(s)
  }

  test("test append") {
    println("start")
    lazy val x = appendWorld("hello")
    println("post")
    println(s"done: $x")
  }

  test("test append lazy function") {
    println("start")
    val x = lazyFunction(appendWorld)("hello")
    println("post")
    println(s"done: $x")
  }

}
