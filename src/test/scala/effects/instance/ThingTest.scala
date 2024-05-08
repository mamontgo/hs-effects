package effects.instance

import effects.instance.All.*
import effects.{Applicative, Functor, Monad, Pure}
import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

case class Thing[A](inner: A)

object Thing {

  implicit class ThingMonad[A](s: Thing[A]) extends ThingApplicative(s) with Monad[Thing, A] {
    override def flatMap[B](f: A => Thing[B]): Thing[B] = f(s.inner)
  }

  implicit def pureThing: Pure[Thing] = new Pure[Thing]:
    override def apply[A](a: A): Thing[A] = Thing(a)

  class ThingApplicative[A](s: Thing[A]) extends ThingFunctor(s) with Applicative[Thing, A] {
    override def ap[B](a: Thing[A => B]): Thing[B] = Thing(a.inner(s.inner))
  }

  trait ThingFunctor[A](s: Thing[A]) extends Functor[Thing, A] {
    override def map[B](f: A => B): Thing[B] = Thing(f(s.inner))
  }
}

class ThingTest extends AnyFunSuite {

  test("Monad for custom Thing") {
    val res = for {
      a <- Thing(123)
      b <- Thing(222)
    } yield a + b
    assert(res == Thing(345))
  }

  test("join nested Seq monad") {
    val nested = Seq(Seq(123))
    val y = Monad.join(nested.monad)
    assert(y == Seq(123))
  }

  test("join nested Thing monad") {
    val nested = Thing(Thing(123))
    val y = Monad.join(nested.monad)
    assert(y == Thing(123))
  }

  test("create pure thing") {
    val nested = Thing(123)
    val p = nested.pure("hello")
    assert(p == Thing("hello"))
  }
}
