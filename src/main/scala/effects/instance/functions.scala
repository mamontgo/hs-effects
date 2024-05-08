package effects.instance

import effects.syntax.FunctionSyntax.*
import effects.{Applicative, Functor, Monad, Pure}


private trait FunctionInstances {


  implicit class FunctionInstanceImpl[A, B](s: A => B) extends FunctionFunctor(s) with FunctionApplicative(s) with FunctionMonad(s)
  implicit class ProducerInstanceImpl[A](s: () => A) extends ProducerFunctor(s) with ProducerApplicative(s) with ProducerMonad(s)

  implicit def pureFunction: Pure[[F] =>> ? => F] = new Pure[[F] =>> ? => F]:
    override def apply[A](a: A): ? => A = const(a)

  implicit def purePartialFunction: Pure[[F] =>> () => F] = new Pure[[F] =>> () => F]:
    override def apply[A](a: A): () => A = () => a

  trait FunctionApplicative[A, B](s: A => B) extends Applicative[[F] =>> A => F, B] {
    override def ap[C](app: A => (B => C)): A => C = (a:A) => app(a)(s(a))
  }

  // M (? => A) => ((? => A) => (? => B)) => (? => B)
  trait FunctionMonad[A, B] (s: A => B) extends Monad[[F] =>> A => F, B] {

    override def flatMap[C](f: B => A => C): A => C = (a:A) => f(s(a))(a)

  }

   trait FunctionFunctor[A, B](s: A => B) extends Functor[[F] =>> A => F, B] {

    //    override def map[A](f: A => B):  A => C = (a:A) => s(f(a))
    override def toString: String = s.toString()

    override def map[C](f: B => C): A => C = (a: A) => f(s(a))
  }

  trait ProducerMonad[B](s: () => B) extends Monad[[F] =>> () => F, B] {

    override def flatMap[C](f: B => () => C): () => C = f(s())
  }

  trait ProducerApplicative[B](s: () => B) extends Applicative[[F] =>> () => F, B] {
    override def ap[C](app: () => (B => C)): () => C = () => app()(s())
  }

  trait ProducerFunctor[B](s: () => B) extends Functor[[F] =>> () => F, B] {
    override def map[C](f: B => C): () => C = () => f(s())
  }
}

object FunctionInstances extends FunctionInstances