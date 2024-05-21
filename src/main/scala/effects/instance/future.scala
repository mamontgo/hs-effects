package effects.instance

import scala.concurrent.ExecutionContext.Implicits.global

import effects.{All, Applicative, ApplicativeConverter, Functor, FunctorConverter, Monad, MonadConverter, Monoid, MonoidConverter, Pure, Return, Zip, ZipConverter}

import scala.concurrent.{ExecutionContext, Future}

trait FutureInstances(implicit executor: ExecutionContext) {

  implicit def futureMonoidConverter[F[_], A](implicit innerConverter: MonoidConverter[F, A]): MonoidConverter[Future, F[A]] = (inst: Future[F[A]]) => {
    inst.map(innerConverter.to).monoid
  }

  implicit def futureFunctorConverter: FunctorConverter[Future] = new FunctorConverter[Future]:
    override def to[A](inst: Future[A]): Functor[Future, A] = inst.functor

  implicit def futureMonadConverter: MonadConverter[Future] = new MonadConverter[Future]:
    override def to[A](inst: Future[A]): Monad[Future, A] = inst.monad

  implicit def futureApplicativeConverter: ApplicativeConverter[Future] = new ApplicativeConverter[Future]:
    override def to[A](inst: Future[A]): Applicative[Future, A] = inst.applicative
  
  implicit def futureZipConverter: ZipConverter[Future] = new ZipConverter[Future]:
    override def to[A](a: Future[A]): Zip[Future, A] = a.asZip


  implicit def returnFuture: Return[Future] = new Return[Future]:
    override def apply[A](a: A): Future[A] = Future(a)

  implicit def pureFuture: Pure[Future] = new Pure[Future]:
    override def apply[A](a: A): Future[A] = Future(a)

  implicit class FutureInstanceImpl[A](s: Future[A]) extends FutureFunctor(s) with FutureMonad(s) with FutureApplicative(s) with FutureZip(s)
  implicit class FutureMonoidEffectTypeClass[B, F[_]](a: Future[Monoid[F, B]]) extends FutureMonoid(a)

  trait FutureZip[A](s: Future[A]) extends Zip[Future, A] {
    override def zipWith[B, C](o: Future[B])(zip: A => B => C): Future[C] = s.zipWith(o)(All.uncurry(zip))
  }

  trait FutureApplicative[A](s: Future[A]) extends Applicative[Future, A] {
    override def ap[B](a: Future[A => B]): Future[B] = a.flatMap(f => s.map(v => f(v)))
  }

  trait FutureMonad[A](s: Future[A]) extends Monad[Future, A] {
    override def flatMap[B](f: A => Future[B]): Future[B] = s.flatMap(f)
  }

  trait FutureFunctor[A](s: Future[A]) extends Functor[Future, A] {
    override def map[B](f: A => B): Future[B] = s.map(f)

    override def inst: Future[A] = s
  }


  trait FutureMonoid[O[_], B](s: Future[Monoid[O, B]]) extends Monoid[Future, O[B]] {


    override def combine(y: Future[O[B]]): Future[O[B]] = {
      for {
        inS <- s
        inY <- y
      } yield inS.combine(inY)
    }

    override def inst: Future[O[B]] = s.map(_.inst)
  }
}

object FutureInstances extends FutureInstances
