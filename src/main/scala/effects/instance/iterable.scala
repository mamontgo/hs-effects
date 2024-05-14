package effects.instance

import effects.{Applicative, Empty, Foldable, Functor, Monad, Monoid, Pure, Return}

import scala.collection.immutable.Iterable

trait IterableInstances {

  implicit def emptyIterable[A]: Empty[Iterable, A] = () => Seq()

  implicit class IterableTypeClasses[T](a: Iterable[T]) extends IterableMonad(a)
    with IterableMonoid(a) with IterableApplicative(a) with IterableFunctor(a) with IterableFoldable(a)

  implicit def returnIterable: Return[Iterable] = new Return[Iterable]:
    override def apply[A](a: A): Iterable[A] = Seq(a)
    override def monad[A](a: A): Monad[Iterable, A] = this (a).monad
    override def toMonad[A](a: Iterable[A]): Monad[Iterable, A] = a.monad

  implicit def pure: Pure[Iterable] = new Pure[Iterable]:
    override def apply[A](a: A): Iterable[A] = Seq(a)
    override def ap[A](a: A): Applicative[Iterable, A] = Seq(a)

  trait IterableMonad[A](s: Iterable[A]) extends Monad[Iterable, A] {
    override def flatMap[B](f: A => Iterable[B]): Iterable[B] = s.flatMap(f)
  }

  trait IterableApplicative[A](s: Iterable[A]) extends Applicative[Iterable , A]{

    override def ap[B](a: Iterable[A => B]): Iterable[B] = a.flatMap(f => s.map(v => f(v)))
  }

  trait IterableFoldable[A](s: Iterable[A]) extends Foldable[Iterable, A] {
    override def foldLeft[B](b: B)(f: (B, A) => B): B = s.foldLeft(b)(f)

    override def foldRight[B](b: B)(f: (A, B) => B): B = s.foldRight(b)(f)
  }

  trait IterableFunctor[A](s: Iterable[A]) extends Functor[Iterable, A] {
    override def map[B](f: A => B): Iterable[B] = s.map(f)

    override def inst: Iterable[A] = s
  }

  trait IterableMonoid[A](s: Iterable[A]) extends Monoid[Iterable, A] {

    override def combine(y: Iterable[A]): Iterable[A] = s ++ y
  }
}

object IterableInstances extends IterableInstances