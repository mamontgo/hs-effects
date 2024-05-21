package effects

import effects.All.*
import org.scalatest.funsuite.AnyFunSuite

class MonoidTest extends AnyFunSuite {

  test("test mconcat") {
    val x: Seq[Seq[Int]] = (1 to 10) <\> (Seq(_))
    assert(Monoid.mconcat(x) == (1 to 10))
  }

  test("test option monoid mconcat") {
    val s: Seq[Option[Seq[Int]]] = Seq(Some(Seq(1, 2, 3)), Some(Seq(4, 5, 6)))
    val res = Monoid.mconcat(s)
    assert(res.contains(Seq(1, 2, 3, 4, 5, 6)))
  }


  test("implicit monoid converters") {
    def asMonoid[F[_], A](i: F[A])(implicit monoidConverter: MonoidConverter[F, A]): Monoid[F, A] = {
      monoidConverter.to(i)
    }

    val o: Option[Seq[Int]] = Some(Seq(1, 2, 3))
    val s: Monoid[Seq, Int] = asMonoid(Seq(1, 2, 3))
    val s2: Monoid[Option, Seq[Int]] = asMonoid(o)

    assert(s.combine(Seq(4)) == Seq(1, 2, 3, 4))
    assert(s2.combine(Some(Seq(4))).contains(Seq(1, 2, 3, 4)))

  }

}
