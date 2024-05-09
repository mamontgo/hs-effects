import effects.instance.All.*
import effects.Functor.*
import effects.instance.IO



@main
def main(): Unit = {

  val example:IO[Unit] = println("hello world".replace("hello", "other"))

  val in:IO[String] = getConsoleLine("Enter your name: ")
  val res:IO[Unit] = in
    .map("goodbye " ++ _) >>= (println(_))

  IO.runEffect(res)
}

def asSeq(s:String): Seq[Char] = s.toSeq

def getNames(): Unit = {
  val firstname = getConsoleLine("Enter firstname: ").map(asSeq)
  val surname = getConsoleLine("Enter surname: ").map(asSeq)
//  firstname.combine(surname)


}

def getConsoleLine(s: String): IO[String] = {

  println(s) >> readline()

//  for {
//    _ <- println(s)
//    in <- readline()
//  } yield in
}

def funtorFunction(): Unit = {
  val addOne =(_:Int)+1
  val addTwo =(_:Int)+2
  val addThree = addOne `map` addTwo
  println(addThree(3))
}

def printFmap(): Unit = {
  val s:Seq[Int] = Seq(1, 2, 3)


}

def optionTesting(): Unit = {
  val x:Option[Int] = Some(4)
  val y: Option[Int] = None

  val test = (_: Option[Int]) match
    case Some(r) => s"You got some ${r}"
    case None => "You got none"

  println(test(x))
  println(test(y))

}


def liftExample(): Unit = {
  val s = Seq(1, 2, 3)
  val addOne = liftF ((_:Int)+1)
  val x = s.functor
  println(addOne(s))
}

def flatMapTest(): Unit = {
  val s = Seq(1, 2, 3)


  val x = for {
    y <- s
    z <- "hello"
  } yield (y, z)

  println(s"X now ${x}")
}