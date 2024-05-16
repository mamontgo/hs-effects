package effects.instance

import effects.syntax.FunctionSyntax.*
import effects.{Applicative, ApplicativeConverter, Functor, FunctorConverter, Monad, MonadConverter, Pure, Return, Zip, ZipConverter}

private trait FunctionInstances {

  implicit def functionFunctorConverter[B]: FunctorConverter[[F] =>> B => F] = new FunctorConverter[[F] =>> B => F]:
    override def to[A](inst: B => A): Functor[[F] =>> B => F, A] = inst.functor

  implicit def functionMonadConverter[B]: MonadConverter[[F] =>> B => F] = new MonadConverter[[F] =>> B => F]:
    override def to[A](inst: B => A): Monad[[F] =>> B => F, A] = inst.monad

  implicit def functionApplicativeConverter[B]: ApplicativeConverter[[F] =>> B => F] = new ApplicativeConverter[[F] =>> B => F]:
    override def to[A](inst: B => A): Applicative[[F] =>> B => F, A] = inst.applicative

  implicit class FunctionInstanceImpl[A, B](s: A => B) extends FunctionFunctor(s) with FunctionApplicative(s) with FunctionMonad(s) with FunctionZip(s)
  implicit class ProducerInstanceImpl[A](s: () => A) extends ProducerFunctor(s) with ProducerApplicative(s) with ProducerMonad(s) with ProducerZip(s)

  implicit def functionZipConverter[B]: ZipConverter[[F] =>> B => F] = new ZipConverter[[F] =>> B => F]:
    override def to[A](a: B => A): Zip[[F] =>> B => F, A] = a.asZip

  implicit def returnFunction: Return[[F] =>> ? => F] = new Return[[F] =>> ? => F]:
    override def apply[A](a: A): ? => A = const(a)
    override def monad[A](a: A): Monad[[F] =>> ? => F, A] = this (a).monad
    override def toMonad[A](a: ? => A): Monad[[F] =>> Function[_, F], A] = a.monad

  implicit def pureFunction: Pure[[F] =>> ? => F] = new Pure[[F] =>> ? => F]:
    override def apply[A](a: A): ? => A = const(a)
    override def ap[A](a: A): Applicative[[F] =>> ? => F,A] = const(a)

  implicit def returnPartialFunction: Return[[F] =>> () => F] = new Return[[F] =>> () => F]:
    override def apply[A](a: A): () => A = () => a
    override def monad[A](a: A): Monad[[F] =>> () => F, A] = this (a).monad
    override def toMonad[A](a: () => A): Monad[[F] =>> () => F, A] = a.monad

  implicit def purePartialFunction: Pure[[F] =>> () => F] = new Pure[[F] =>> () => F]:
    override def apply[A](a: A): () => A = () => a
    override def ap[A](a: A): Applicative[[F] =>> () => F, A] = () => a

  trait FunctionApplicative[A, B](s: A => B) extends Applicative[[F] =>> A => F, B] {
    override def ap[C](app: A => (B => C)): A => C = (a:A) => app(a)(s(a))
  }

  // M (? => A) => ((? => A) => (? => B)) => (? => B)
  trait FunctionMonad[A, B] (s: A => B) extends Monad[[F] =>> A => F, B] {

    override def flatMap[C](f: B => A => C): A => C = (a:A) => f(s(a))(a)

  }

  trait FunctionZip[A, B](s: A => B) extends Zip[[F] =>> A => F, B] {
    override def zipWith[C, D](o: A => C)(zip: B => C => D): A => D = (a:A) => zip(s(a))(o(a))
  }

  trait FunctionFunctor[A, B](s: A => B) extends Functor[[F] =>> A => F, B] {

    override def map[C](f: B => C): A => C = (a: A) => f(s(a))

     override def inst: A => B = s
  }

  trait ProducerMonad[B](s: () => B) extends Monad[[F] =>> () => F, B] {

    override def flatMap[C](f: B => () => C): () => C = f(s())
  }

  trait ProducerApplicative[B](s: () => B) extends Applicative[[F] =>> () => F, B] {
    override def ap[C](app: () => (B => C)): () => C = () => app()(s())
  }

  trait ProducerZip[B](s: () => B) extends Zip[[F] =>> () => F, B] {
    override def zipWith[C, D](o:() => C)(zip: B => C => D): () => D = () => zip(s())(o())
  }

  trait ProducerFunctor[B](s: () => B) extends Functor[[F] =>> () => F, B] {
    override def map[C](f: B => C): () => C = () => f(s())

    override def inst: () => B = s
  }
}

object FunctionInstances extends FunctionInstances