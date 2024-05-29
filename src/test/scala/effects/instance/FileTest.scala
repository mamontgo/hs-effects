package effects.instance

import effects.All.*
import effects.Monad
import org.scalatest.funsuite.AnyFunSuite

import scala.runtime.Nothing$
import scala.util.Using

class FileTest extends AnyFunSuite {
  val fileName = "src/test/resources/test.txt"

  test("read file") {
    val fileLinesIO: IO[Seq[String]] = FileRead.read(fileName).map(_.getLines)


    val printLines:IO[Seq[IO[Unit]]] = fileLinesIO.map(_.map(println))

    IO.runEffect({
      val sequenced: IO[IO[Seq[Unit]]] = printLines.map(effects.Traversable.sequenceA)
      val joined: IO[Seq[Unit]] = Monad.join(sequenced)
      joined
    })

//    Monad.join(fileLinesIO.map(x => effects.Traversable.sequenceA(x.map(println))))

    IO.runEffect(printLines).foreach(IO.runEffect)
  }




}
