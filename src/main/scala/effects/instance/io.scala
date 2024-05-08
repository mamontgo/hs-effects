package effects.instance

import effects.{Applicative, Functor, Monad, Monoid, Pure}
import effects.instance.FunctionInstances.*
import effects.instance.IO.{IOApplicative, IOFunctor, IOMonad, IOMonoid}

class IO[T] private(payLoad: () => T) {
  private def getPayload: () => T = payLoad
}


object IO {

  implicit def pureIO: Pure[IO] = new Pure[IO]:
    override def apply[A](a: A): IO[A] = IO.create(a)

  def runEffect[T](in: IO[T]): T = in.getPayload()

  def create[T](data: => T): IO[T] = IO(() => data)

  def combine[B, F[_]](a: IO[Monoid[F, B]], b: IO[F[B]]): IO[F[B]] = {
    IO.create(a.getPayload().combine(b.getPayload()))
  }

  trait IOMonoid[B, F[_]](s: IO[Monoid[F, B]]) extends Monoid[IO, F[B]] {

    override def combine(y: IO[F[B]]): IO[F[B]] = IO.create(s.getPayload().combine(y.getPayload()))

    override def empty: IO[F[B]] = IO.create(s.getPayload().empty)
  }


  trait IOMonad[A](s: IO[A]) extends Monad[IO, A] {
    //    override def flatMap[B](f: A => IO[B]): IO[B] = f(s.getPayload())
    override def flatMap[B](f: A => IO[B]): IO[B] = {
      IO.create(f(s.getPayload()).getPayload())
    }
  }


  trait IOApplicative[A](s: IO[A]) extends Applicative[IO, A] {
    override def ap[B](a: IO[A => B]): IO[B] = IO(s.getPayload.ap(a.getPayload))
  }

  trait IOFunctor[A](s: IO[A]) extends Functor[IO, A] {
    override def map[B](f: A => B): IO[B] = IO(s.getPayload.map(f))

  }

}

trait IOInstances {
  implicit class IOEffectTypeClass[A](a:IO[A]) extends IOMonad(a) with IOApplicative(a) with IOFunctor(a)
  implicit class IOMonoidEffectTypeClass[B, F[_]](a:IO[Monoid[F, B]]) extends IOMonoid(a)
}

object IOInstances extends IOInstances


