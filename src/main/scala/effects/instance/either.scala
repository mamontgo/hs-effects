package effects.instance

import effects.syntax.FunctionSyntax
import effects.{Applicative, ApplicativeConverter, Empty, Foldable, Functor, FunctorConverter, Monad, MonadConverter, Monoid, Pure, Return, Zip, ZipConverter}


trait EitherInstances {

  implicit def eitherFunctorConverter[B]: FunctorConverter[[F] =>> Either[B, F]] = new FunctorConverter[[F] =>> Either[B, F]]:
    override def to[A](inst: Either[B, A]): Functor[[F] =>> Either[B, F], A] = inst.functor

  implicit def eitherMonadConverter[B]: MonadConverter[[F] =>> Either[B, F]] = new MonadConverter[[F] =>> Either[B, F]]:
    override def to[A](inst: Either[B, A]): Monad[[F] =>> Either[B, F], A] = inst.monad

  implicit def eitherApplicativeConverter[B]: ApplicativeConverter[[F] =>> Either[B, F]] = new ApplicativeConverter[[F] =>> Either[B, F]]:
    override def to[A](inst: Either[B, A]): Applicative[[F] =>> Either[B, F], A] = inst.applicative

  
  implicit def eitherZipConverter[B]: ZipConverter[[F] =>> Either[B, F]] = new ZipConverter[[F] =>> Either[B, F]]:
    override def to[A](a: Either[B, A]): Zip[[F] =>> Either[B, F], A] = a.inst

  implicit def emptyEither[F[_], B](implicit empty: Empty[F, B]): Empty[[E] =>> Either[?, E], F[B]] = () => Right(empty())

  implicit def eitherReturn[B]: Return[[F] =>> Either[B, F]] = new Return[[F] =>> Either[B, F]]:
    override def apply[A](a: A): Either[B, A] = Right(a)

  implicit def pureEither: Pure[[F] =>> Either[?, F]] = new Pure[[F] =>> Either[?, F]]:
    override def apply[A](a: A): Either[?, A] = Right(a)
    override def ap[A](a: A): Applicative[[F] =>> Either[?, F], A] = Right(a)

  implicit class EitherInstanceImpl[A, B](s: Either[A, B]) extends EitherApplicative(s) with EitherMonad(s) with EitherFunctor(s) with EitherZip(s)
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

  trait EitherZip[A, B](s: Either[A, B]) extends Zip[[F] =>> Either[A, F], B] {
    override def zipWith[D, E](o: Either[A, D])(zip: B => D => E): Either[A, E] = for {
      si <- s
      oi <- o
    } yield zip(si)(oi)
  }

  trait EitherFoldable[A, B](s: Either[A, B]) extends Foldable[[F] =>> Either[A, F], B] {

    override def foldLeft[C](b: C)(f: (C, B) => C): C = s match
      case Left(_) => b
      case Right(r) => f(b, r)

    override def foldRight[C](b: C)(f: (B, C) => C): C = foldLeft(b)(FunctionSyntax.flip(f))
  }

}