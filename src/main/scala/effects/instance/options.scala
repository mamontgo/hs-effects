package effects.instance

import effects.syntax.FunctionSyntax
import effects.{Applicative, Foldable, Functor, Monad, Pure, Return}




private trait OptionInstances {

  implicit def returnOption: Return[Option] = new Return[Option]:
    override def apply[A](a: A): Option[A] = Some(a)
    override def monad[A](a: A): Monad[Option, A] = this (a).monad
    override def toMonad[A](a: Option[A]): Monad[Option, A] = a.monad


  implicit def pureOption: Pure[Option] = new Pure[Option]:
    override def apply[A](a: A): Option[A] = Some(a)
    override def ap[A](a: A): Applicative[Option, A] = Some(a)

  implicit class OptionMonad[A](s: Option[A]) extends OptionApplicative(s) with Monad[Option, A] {
    override def flatMap[B](f: A => Option[B]): Option[B] = s.flatMap(f)
  }

  class OptionApplicative[A](s: Option[A]) extends OptionFunctor(s) with Applicative[Option, A] {

    override def ap[B](a: Option[A => B]): Option[B] = a.flatMap(f => s.map(v => f(v)))
  }

  trait OptionFunctor[A](s: Option[A]) extends Functor[Option, A] {
    override def map[B](f: A => B): Option[B] = s.map(f)

    override def inst: Option[A] = s
  }

  trait OptionFoldable[A](s: Option[A]) extends Foldable[Option, A] {
    def foldLeft[B](b: B)(f: (B, A) => B): B = s.fold(b)(a => f(b, a))

    def foldRight[B](b: B)(f: (A, B) => B): B = foldLeft(b)(FunctionSyntax.flip(f))
  }
}


object OptionInstances extends OptionInstances