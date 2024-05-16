package effects

import org.scalatest.funsuite.AnyFunSuite
import effects.All.*
import effects.instance.IO

class MonadTest extends AnyFunSuite {


  test("flat map of option") {

    val addOneSomeNumber:Int => Option[Int] = a => Some(a+1)

    val res = Some(123) >>= addOneSomeNumber
    val res2 = Some(123).flatMap(addOneSomeNumber)
    assert(res == res2)
    assert(res.contains(124))
  }

  test("Option As Monad ") {
    val x = Some(123).monad;
    println(x)
  }

  test("monad sequence") {

    val list: Seq[Option[String]] = Seq(Some("123"), Some("321"))
    val res: Option[Seq[String]] = Monad.sequence(list)

    IO.runEffect(println(res))
    assert(res.contains(List("123", "321")))
  }

  test("mapM options") {

    def asOption(a: Int): Option[Int] =
      a match
        case 5 => Some(50)
        case y => Some(y)

//      if (a == 5)
//        Some(50)
//      else
//        Some(a)


    val nums: Seq[Int] = 1 to 10

    val res = Monad.mapM(asOption)(nums)

    IO.runEffect(println(res))
    assert(res.contains(List(1, 2, 3, 4, 50, 6, 7, 8, 9, 10)))
  }

}
