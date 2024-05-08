package effects

import org.scalatest.funsuite.AnyFunSuite

import java.io.{File, FileWriter}
import scala.io.BufferedSource
import scala.util.{Try, Using}

class Sandbox  extends AnyFunSuite {

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



  trait CircleCirc:
    def circumference: Double

  case class Circle(x: Double, y: Double, radius: Double)

  extension (c: Circle)
    def circumference: Double = c.radius * math.Pi * 2

  object CircleHelpers:
    def circumference(c: Circle): Double = c.radius * math.Pi * 2

  test("implicit trait>") {
    val x = Circle(4,4,4)
//    val v: Circ = x
//val fileWriter = new FileWriter(new File("./src/test/resources/hello.txt"))
//    fileWriter.write("hello there")
//    fileWriter.close()


    val in: Try[BufferedSource] = Using(scala.io.Source.fromFile("src/test/resources/test.txt")) {
      result => result
    }

    //    val data = .getLines().toSeq
    println(x.circumference)
  }

  test("thing?") {
    class Thing[A](thunk: => A) {

      val job = () => thunk

      def run: A = {
        job()
      }
    }




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
