package effects.instance

import effects.instance.FunctionInstances.*
import effects.instance.IO.{IOApplicative, IOFunctor, IOMonad, IOMonoid, IOZip, asFuture}
import effects.*

import scala.concurrent.{ExecutionContext, Future}

class IO[T] private(payLoad: () => T) {
  private def getPayload: () => T = payLoad
  def future(implicit executor: ExecutionContext): Future[T] = asFuture(this)
}


object IO {

  implicit def emptyIO[F[_], A](implicit empty: Empty[F, A]): Empty[IO, F[A]] = () => IO.create(empty())

  def asFuture[T](in: IO[T])(implicit executor: ExecutionContext):Future[T] = Future {    runEffect(in)   }

  def runEffect[T](in: IO[T]): T = in.getPayload()

  def create[T](data: => T): IO[T] = IO(() => data)

  def combine[B, F[_]](a: IO[Monoid[F, B]], b: IO[F[B]]): IO[F[B]] = {
    IO.create(a.getPayload().combine(b.getPayload()))
  }

  trait IOMonoid[B, F[_]](s: IO[Monoid[F, B]]) extends Monoid[IO, F[B]] {

    override def combine(y: IO[F[B]]): IO[F[B]] = IO.create(s.getPayload().combine(y.getPayload()))

    override def inst: IO[F[B]] = {
      import IOInstances._
      s.map(_.inst)
    }

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

    override def inst: IO[A] = s
  }

  trait IOZip[A](s: IO[A]) extends Zip[IO, A] {
    override def zipWith[B, C](o: IO[B])(zip: A => B => C): IO[C] = IO.create(zip(s.getPayload())(o.getPayload()))
      
  }

}

trait IOInstances {
  implicit class IOEffectTypeClass[A](a: IO[A]) extends IOMonad(a) with IOApplicative(a) with IOFunctor(a) with IOZip(a)

  implicit class IOMonoidEffectTypeClass[B, F[_]](a: IO[Monoid[F, B]]) extends IOMonoid(a)

  implicit def ioZipConverter: ZipConverter[IO] = new ZipConverter[IO]:
    override def to[A](a: IO[A]): Zip[IO, A] = a.asZip


  implicit def returnIO: Return[IO] = new Return[IO]:
    override def apply[A](a: A): IO[A] = IO.create(a)
    override def monad[A](a: A): Monad[IO, A] = this (a).monad
    override def toMonad[A](a: IO[A]): Monad[IO, A] = a.monad


  implicit def pureIO: Pure[IO] = new Pure[IO]:
    override def apply[A](a: A): IO[A] = IO.create(a)
    override def ap[A](a: A): Applicative[IO, A] = IO.create(a).applicative
}

object IOInstances extends IOInstances


