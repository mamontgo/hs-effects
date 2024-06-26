package effects.instance

import effects.instance.All.*
import effects.{Applicative, Functor, Monad, MonadConverter, Pure, Return}
import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

case class Thing[A](inner: A)

object Thing {


  implicit def returnMonadConverter: MonadConverter[Thing] = new MonadConverter[Thing] {
    override def to[A](inst: Thing[A]): Monad[Thing, A] = inst.monad
  }

  implicit def returnThing: Return[Thing] = new Return[Thing]:
    override def apply[A](a: A): Thing[A] = Thing(a)

  implicit def pureThing: Pure[Thing] = new Pure[Thing]:
    override def apply[A](a: A): Thing[A] = Thing(a)

  implicit class ThingInstance[A](s:Thing[A]) extends ThingFunctor(s) with ThingMonad(s) with ThingApplicative(s)
  
  trait ThingApplicative[A](s: Thing[A]) extends Applicative[Thing, A] {
    override def ap[B](a: Thing[A => B]): Thing[B] = Thing(a.inner(s.inner))
  }

  trait ThingMonad[A](s: Thing[A]) extends Monad[Thing, A] {
    override def flatMap[B](f: A => Thing[B]): Thing[B] = f(s.inner)
  }

  trait ThingFunctor[A](s: Thing[A]) extends Functor[Thing, A] {
    override def map[B](f: A => B): Thing[B] = Thing(f(s.inner))

    override def inst: Thing[A] = s
  }
}

def returnTest[A, M[_]](a:A)(implicit m: Return[M]) = {
  m(a)
}

class ThingTest extends AnyFunSuite {


  test("implicit monad return") {
    val t: Thing[String] = returnTest[String, Thing]("Hello")
    assert(t == Thing("Hello"))
  }

  test("type casting Thing") {
    val t:Thing[String] = Thing("Hello")
    assert(t.isInstanceOf[Thing[String]])
    assert(!t.isInstanceOf[Functor[Thing, String]])

    val x = t.functor

    assert(!x.isInstanceOf[Thing[String]])
    assert(x.isInstanceOf[Functor[Thing, String]])

    val y = x.inst
    assert(y.isInstanceOf[Thing[String]])
    assert(!y.isInstanceOf[Functor[Thing, String]])


  }

  test("Monad for custom Thing") {
    val res = for {
      a <- Thing(123)
      b <- Thing(222)
    } yield a + b
    assert(res == Thing(345))
  }

  test("join nested Seq monad") {
    val nested = Seq(Seq(123))
    val y = Monad.join(nested)
    assert(y == Seq(123))
  }

  test("join nested Thing monad") {
    val nested = Thing(Thing(123))
    val y = Monad.join(nested)
    assert(y == Thing(123))
  }

  test("create pure thing") {
    val nested = Thing(123)
    val p = nested.pure("hello")
    assert(p == Thing("hello"))
  }
}
