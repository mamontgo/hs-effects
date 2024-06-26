package effects.instance

import effects.instance.FunctionInstances.*
import effects.instance.IO.{IOAlternative, IOApplicative, IOFunctor, IOMonad, IOMonoid, IOZip, asFuture}
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

  def create[T](data: => T): IO[T] = new IO(() => data)

  def tryRun[T](in: IO[T]): Either[Throwable, T] = {
    try Right(runEffect(in))
    catch case e => Left(e)
  }

  def combine[B, F[_]](a: IO[F[B]], b: IO[F[B]])(implicit c:MonoidConverter[F, B]): IO[F[B]] = {
    IO.create(c.to(a.getPayload()).combine(b.getPayload()))
  }

  private def failIO[A](desc:String): IO[A] = IO.create[A](throw Exception(desc))

  def mplusIO[A](a: IO[A])(b: IO[A]): IO[A] = {
    IO.create(
      tryRun(a) match
        case Left(_) => runEffect(b)
        case Right(x) => x
    )

  }

  trait IOMonoid[B, F[_]](s: IO[F[B]])(implicit c:MonoidConverter[F, B]) extends Monoid[IO, F[B]] {

    override def combine(y: IO[F[B]]): IO[F[B]] = IO.create(c.to(s.getPayload()).combine(y.getPayload()))

    override def inst: IO[F[B]] = s

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

  trait IOAlternative[A](s: IO[A]) extends Alternative[IO, A] {

    override def empty: IO[A] = failIO("mzero")

    override def alt: IO[A] => IO[A] = mplusIO(s)
  }
}

trait IOInstances {

  implicit def ioMonoidConverter[F[_], A](implicit innerConverter: MonoidConverter[F, A]): MonoidConverter[IO, F[A]] = (inst: IO[F[A]]) => {
    IOMonoidEffectTypeClass(inst)
  }

  implicit def ioFunctorConverter: FunctorConverter[IO] = new FunctorConverter[IO]:
    override def to[A](inst: IO[A]): Functor[IO, A] = inst.functor

  implicit def ioMonadConverter: MonadConverter[IO] = new MonadConverter[IO]:
    override def to[A](inst: IO[A]): Monad[IO, A] = inst.monad

  implicit def ioApplicativeConverter: ApplicativeConverter[IO] = new ApplicativeConverter[IO]:
    override def to[A](inst: IO[A]): Applicative[IO, A] = inst.applicative

  implicit def ioAlternativeConverter: AlternativeConverter[IO] = new AlternativeConverter[IO]:
    override def to[A](inst: IO[A]): Alternative[IO, A] = inst.alternative

  implicit class IOEffectTypeClass[A](a: IO[A]) extends IOMonad(a) with IOApplicative(a) with IOFunctor(a) with IOZip(a)
    with IOAlternative(a)

  implicit class IOMonoidEffectTypeClass[B, F[_]](a: IO[F[B]])(implicit c:MonoidConverter[F, B]) extends IOMonoid(a)(c)

  implicit def ioZipConverter: ZipConverter[IO] = new ZipConverter[IO]:
    override def to[A](a: IO[A]): Zip[IO, A] = a.asZip


  implicit def returnIO: Return[IO] = new Return[IO]:
    override def apply[A](a: A): IO[A] = IO.create(a)


  implicit def pureIO: Pure[IO] = new Pure[IO]:
    override def apply[A](a: A): IO[A] = IO.create(a)
}

object IOInstances extends IOInstances


