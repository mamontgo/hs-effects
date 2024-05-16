package effects.instance

import effects.{Applicative, Empty, Functor, Monad, Monoid, Pure, Return}

import scala.util.{Success, Try}

trait TryInstances {

  implicit def emptyTry[F[_], A](implicit empty: Empty[F, A]): Empty[Try, F[A]] = () => Success(empty())

  implicit def returnTry: Return[Try] = new Return[Try]:
    override def apply[A](a: A): Try[A] = Success(a)
    override def monad[A](a: A): Monad[Try, A] = this (a).monad
    override def toMonad[A](a: Try[A]): Monad[Try, A] = a.monad

  implicit def pureTry: Pure[Try] = new Pure[Try]:
    override def apply[A](a: A): Try[A] = Success(a)
    override def ap[A](a: A): Applicative[Try, A] = Success(a).applicative


  implicit class TryEffectTypeClass[A](a: Try[A]) extends TryMonad(a) with TryApplicative(a) with TryFunctor(a) with TryIO(a)

  implicit class TryMonoidEffectTypeClass[B, F[_]](a: Try[Monoid[F, B]]) extends TryMonoid(a)

  trait TryIO[A](s: Try[A]) {
    def asIO:IO[A] = IO.create(s.get)
  }

  trait TryApplicative[A](s: Try[A]) extends Applicative[Try, A] {
    override def ap[B](a: Try[A => B]): Try[B] = a.flatMap(s.map)
  }

  trait TryMonad[A](s: Try[A]) extends Monad[Try, A] {
    override def flatMap[B](f: A => Try[B]): Try[B] = s.flatMap(f)
  }

  trait TryFunctor[A](s: Try[A]) extends Functor[Try, A] {
    override def map[B](f: A => B): Try[B] = s.map(f)

    override def inst: Try[A] = s
  }

  trait TryMonoid[B, F[_]](s: Try[Monoid[F, B]]) extends Monoid[Try, F[B]] {

    override def combine(y: Try[F[B]]): Try[F[B]] = s match
      case Success(i) => y match
        case Success(x) => Success(i.combine(x))
        case z => z
      case n => n.map(_.inst)

    override def inst: Try[F[B]] = {
      s.map(_.inst)
    }

  }
}