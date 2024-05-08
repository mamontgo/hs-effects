package effects.instance

trait Console {

  def println(x: Any): IO[Unit] = IO.create(scala.Console.println(x))

  def print(x: Any): IO[Unit] = IO.create(scala.Console.print(x))

  def readline(): IO[String] = IO.create {
    scala.io.StdIn.readLine()
  }

}

object Console extends Console