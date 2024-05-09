package effects.instance

import effects.Functor
import scala.io.BufferedSource
import scala.util.{Failure, Success, Try, Using}

object FileRead {
  def read(fileName: String): IO[FileRead] = IO.create(FileRead(scala.io.Source.fromFile(fileName)))
}

case class FileRead(source: BufferedSource) extends Functor[Seq, Char] {

  override def map[B](f: Char => B): Seq[B] = {
    val x:Try[Iterator[B]] = Using(source) {
      resource => resource.map(f)
    }
    x match
      case Success(value) => value.toSeq
      case Failure(exception) => throw exception
  }

  def getLines: Seq[String] = {
    source.getLines().toSeq
  }

  override def inst: Seq[Char] = source.toSeq
}

