import effects.All.*
import effects.instance.IO



@main
def main(): Unit = {

  addNumbers()
}

def sayGoodbye(): Unit = {
  val example: IO[Unit] = println("hello world")
  IO.runEffect(example)

  val in: IO[String] = getConsoleLine("Enter your name: ")
  val res: IO[Unit] = in
    .map("goodbye " ++ _) >>= (println(_))

  IO.runEffect(res)
}

def addNumbers(): Unit = {

  val addNumbers: Int => Int => Int = curry((_:Int) + (_:Int))

  val first:IO[Int] = println("Enter first number") >> readline() <\> (_.toInt)

  val second:IO[Int] = println("Enter second number") >> readline() <\> (_.toInt)

  val add:IO[Unit] = first.zipWith(second)(addNumbers) <\> ("Result is: " ++ (_:Int).toString) >>= println

  IO.runEffect(add)

//  IO.tryRun(add) match
//    case Left(x) => IO.runEffect(println("Are you sure you know what a number is?") >> println(x.getMessage))
//    case Right(x) =>
}


def getConsoleLine(s: String): IO[String] = {

  println(s) >> readline()

//  println(s).flatMap(_ => readline())
//  println(s) >>= (_ => readline())


  //  println(s)
//  readline()


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


def getNames(): Unit = {
  val firstname = getConsoleLine("Enter firstname: ").map(_.toSeq)
  val surname = getConsoleLine("Enter surname: ").map(_.toSeq)

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