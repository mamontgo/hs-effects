package effects.instance

import effects.{Applicative, ApplicativeConverter, Empty, Functor, FunctorConverter, Monad, MonadConverter, Monoid, MonoidConverter, Pure, Return}

import scala.util.{Success, Try}

trait TryInstances {

  implicit def tryMonoidConverter[F[_], A](implicit innerConverter: MonoidConverter[F, A]): MonoidConverter[Try, F[A]] = (inst: Try[F[A]]) => {
    inst.map(innerConverter.to).monoid
  }

  implicit def tryFunctorConverter: FunctorConverter[Try] = new FunctorConverter[Try]:
    override def to[A](inst: Try[A]): Functor[Try, A] = inst.functor

  implicit def tryMonadConverter: MonadConverter[Try] = new MonadConverter[Try]:
    override def to[A](inst: Try[A]): Monad[Try, A] = inst.monad

  implicit def tryApplicativeConverter: ApplicativeConverter[Try] = new ApplicativeConverter[Try]:
    override def to[A](inst: Try[A]): Applicative[Try, A] = inst.applicative
  
  implicit def emptyTry[F[_], A](implicit empty: Empty[F, A]): Empty[Try, F[A]] = () => Success(empty())

  implicit def returnTry: Return[Try] = new Return[Try]:
    override def apply[A](a: A): Try[A] = Success(a)


  implicit def pureTry: Pure[Try] = new Pure[Try]:
    override def apply[A](a: A): Try[A] = Success(a)

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