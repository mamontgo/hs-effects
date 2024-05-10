package effects.instance

import effects.{Applicative, Functor, Monad, Pure, Return}

import scala.concurrent.{ExecutionContext, Future}

private trait FutureInstances(implicit executor: ExecutionContext) {


  implicit def returnFuture: Return[Future] = new Return[Future]:
    override def apply[A](a: A): Future[A] = Future(a)
    override def monad[A](a: A): Monad[Future, A] = this (a).monad
    override def toMonad[A](a: Future[A]): Monad[Future, A] = a.monad

  implicit def pureFuture: Pure[Future] = new Pure[Future]:
    override def apply[A](a: A): Future[A] = Future(a)
    override def ap[A](a: A): Applicative[Future, A] = Future(a)

  implicit class FutureInstanceImpl[A](s: Future[A]) extends FutureFunctor(s) with FutureMonad(s) with FutureApplicative(s)

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

}
