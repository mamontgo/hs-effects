package effects.instance

import effects.syntax.FunctionSyntax
import effects.*

private trait OptionInstances {

  implicit def returnOption: Return[Option] = new Return[Option]:
    override def apply[A](a: A): Option[A] = Some(a)

    override def monad[A](a: A): Monad[Option, A] = this (a).monad

    override def toMonad[A](a: Option[A]): Monad[Option, A] = a.monad


  implicit def pureOption: Pure[Option] = new Pure[Option]:
    override def apply[A](a: A): Option[A] = Some(a)

    override def ap[A](a: A): Applicative[Option, A] = Some(a)

  implicit class OptionInstanceImpl[A](s: Option[A]) extends OptionApplicative(s) with OptionMonad(s) with OptionFunctor(s) with OptionFoldable(s)
  implicit class OptionMonoidEffectTypeClass[B, F[_]](a: Option[Monoid[F, B]]) extends OptionMonoid(a)

  trait OptionMonad[A](s: Option[A]) extends Monad[Option, A] {
    override def flatMap[B](f: A => Option[B]): Option[B] = s.flatMap(f)
  }

  trait OptionApplicative[A](s: Option[A]) extends Applicative[Option, A] {

    override def ap[B](a: Option[A => B]): Option[B] = a.flatMap(f => s.map(v => f(v)))
  }

  trait OptionFunctor[A](s: Option[A]) extends Functor[Option, A] {
    override def map[B](f: A => B): Option[B] = s.map(f)

    override def inst: Option[A] = s
  }

  trait OptionMonoid[O[_], B](s: Option[Monoid[O, B]]) extends Monoid[Option, O[B]] {


    override def combine(y: Option[O[B]]): Option[O[B]] = {
      for {
        inS <- s
        inY <- y
      } yield inS.combine(inY)
    }

    override def empty: Option[O[B]] = None

    override def inst: Option[O[B]] = s.map(_.inst)
  }

  trait OptionFoldable[A](s: Option[A]) extends Foldable[Option, A] {
    def foldLeft[B](b: B)(f: (B, A) => B): B = s.fold(b)(a => f(b, a))

    def foldRight[B](b: B)(f: (A, B) => B): B = foldLeft(b)(FunctionSyntax.flip(f))
  }
}


object OptionInstances extends OptionInstances