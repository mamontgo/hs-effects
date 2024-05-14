package effects.instance

trait ThreadIO {

  def sleep(milliseconds:Long): IO[Unit] = IO.create(Thread.sleep(milliseconds))

}

object ThreadIO extends ThreadIO