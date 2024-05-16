package effects.instance

import effects.All.*
import org.scalatest.funsuite.AnyFunSuite

import scala.runtime.Nothing$
import scala.util.Using

class FileTest extends AnyFunSuite {
  val fileName = "src/test/resources/test.txt"

  test("read file") {
    val x: IO[Seq[String]] = FileRead.read(fileName).map(_.getLines)
    val printLines = x.flatMap(s => {
      IO.create {
        s.map(line => {
          println(line)
        })
      }
    })

    IO.runEffect(printLines).foreach(IO.runEffect)
  }


  test("file testing") {
    Using(scala.io.Source.fromFile(fileName)) {
      result => {
        result.getLines().foreach(x => println(x))
        result
      }
    }
  }

  test("file testing2") {
    val x = Using(scala.io.Source.fromFile(fileName)) {
      result => {
        result
      }
    }

    x.map(s => {
      Using(s) {
        result => {
          result.getLines().foreach(x => println(x))
        }
      }
    })

    x.map(s => s.getLines().foreach(x => println(x)))
  }
}
