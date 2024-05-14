package effects.instance

import effects.syntax.FunctionSyntax
import effects.{Applicative, Empty, Foldable, Functor, Monad, Monoid, Pure, Return}


trait EitherInstances {

  implicit def emptyEither[F[_], B](implicit empty: Empty[F, B]): Empty[[E] =>> Either[?, E], F[B]] = () => Right(empty())

  implicit def eitherReturn[B]: Return[[F] =>> Either[?, F]] = new Return[[F] =>> Either[?, F]]:
    override def apply[A](a: A): Either[?, A] = Right(a)
    override def monad[A](a: A): Monad[[F] =>> Either[?, F], A] = Right(a)
    override def toMonad[A](a: Either[?, A]): Monad[[F] =>> Either[?, F], A] = a.monad


  implicit def pureEither: Pure[[F] =>> Either[?, F]] = new Pure[[F] =>> Either[?, F]]:
    override def apply[A](a: A): Either[?, A] = Right(a)
    override def ap[A](a: A): Applicative[[F] =>> Either[?, F], A] = Right(a)

  implicit class EitherInstanceImpl[A, B](s: Either[A, B]) extends EitherApplicative(s) with EitherMonad(s) with EitherFunctor(s)
  implicit class EitherMonoidEffectTypeClass[A, E[_], B](s: Either[A, Monoid[E, B]]) extends EitherMonoid(s)

  trait EitherMonoid[A, E[_], B] (s: Either[A, Monoid[E, B]]) extends Monoid[[F] =>> Either[A, F], E[B]] {

    override def inst: Either[A, E[B]] = s.map(_.inst)

    override def combine(y: Either[A, E[B]]): Either[A, E[B]] = {
      for {
        f <- s
        n <- y
      } yield f.combine(n)
    }
  }

  trait EitherMonad[A, B](s: Either[A, B]) extends Monad[[F] =>> Either[A, F], B] {
    override def flatMap[C](f: B => Either[A, C]): Either[A, C] = s.flatMap(f)
  }

  trait EitherApplicative[A, B](s: Either[A, B]) extends Applicative[[F] =>> Either[A, F], B] {
    override def ap[C](a: Either[A, B => C]): Either[A, C] = s.flatMap(b => a.map(f => f(b)))
  }

  trait EitherFunctor[A, B](s: Either[A, B]) extends Functor[[F] =>> Either[A, F], B] {

    override def inst: Either[A, B] = s

    override def map[C](f: B => C): Either[A, C] = s.map(f)
  }


  trait EitherFoldable[A, B](s: Either[A, B]) extends Foldable[[F] =>> Either[A, F], B] {

    override def foldLeft[C](b: C)(f: (C, B) => C): C = s match
      case Left(_) => b
      case Right(r) => f(b, r)

    override def foldRight[C](b: C)(f: (B, C) => C): C = foldLeft(b)(FunctionSyntax.flip(f))
  }

}