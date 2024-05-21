package effects.instance

import org.scalatest.funsuite.AnyFunSuite
import effects.All._

class WriterTest extends AnyFunSuite{

  test("simple log") {
    def logNumber(x: Int) = Writer((x, Seq(s"You got $x")))

    val x = for {
      x <- logNumber(3)
      y <- logNumber(5)
      _ <- Writer.tell(Seq("Multiply numbers"))
    } yield x * y

    assert(x.runWriter._1 == 15)
    assert(x.runWriter._2 == Seq("You got 3", "You got 5", "Multiply numbers"))

    IO.runEffect(println(x.runWriter))

  }

}
