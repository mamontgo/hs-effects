package effects.instance

import effects.utils.DateIO
import effects.All.*
import org.scalatest.funsuite.AnyFunSuite

import scala.util.Random

class RandomTest extends AnyFunSuite {

  test("random number list test") {
    // getting system time is a IO operation so generating a seeded random should also be IO

    DateIO.nanoTime.map(Random(_))

  }
}
