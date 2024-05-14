package effects.instance

import effects.syntax.FunctionSyntax
import effects.{Applicative, Empty, Foldable, Functor, Monad, Monoid, Pure, Return}


trait TupleInstances {

  implicit def emptyTuple[F[_], B](implicit empty: Empty[F, B]): Empty[[E] =>> (?, E), F[B]] = () => (empty(), empty())

  implicit def tupleReturn[B[_] <: Monoid[B, C], C](implicit empty: Empty[B, C]): Return[[F] =>> (?, F)] = new Return[[F] =>> (?, F)]:
    override def apply[A](a: A): (B[C], A) = (empty(), a)

    override def monad[A](a: A): Monad[[F] =>> (?, F), A] = (empty(), a)

    override def toMonad[A](a: (?, A)): Monad[[F] =>> (?, F), A] = a.monad

  implicit def pureTuple[B[_] <: Monoid[B, C], C](implicit empty: Empty[B, C]): Pure[[F] =>> (?, F)] = new Pure[[F] =>> (?, F)]:
    override def apply[A](a: A): (?, A) = (empty(), a)

    override def ap[A](a: A): Applicative[[F] =>> (?, F), A] = (empty(), a)

  implicit class TupleInstanceImpl[A, B](s: (A, B)) extends TupleApplicative(s) with TupleMonad(s) with TupleFunctor(s) with TupleFoldable(s)

  trait TupleApplicative[A, B](s: (A, B))  extends Applicative[[F] =>> (A, F), B] {

    override def ap[C](a: (A, B => C)): (A, C) = a match
      case (x, f) => (x, f(s._2))
  }

  trait TupleMonad[A, B](s: (A, B)) extends Monad[[F] =>> (A, F), B] {

    override def flatMap[C](f: B => (A, C)): (A, C) = s match
      case (_, y) => f(y)
  }

  trait TupleFunctor[A, B](s: (A, B)) extends Functor[[F] =>> (A, F), B] {

    override def inst: (A, B) = s

    override def map[C](f: B => C): (A, C) = s match
      case (x, y) => (x, f(y))
  }

  trait TupleFoldable[A, B](s: (A, B)) extends Foldable[[F] =>> (A, F), B] {

    override def foldLeft[C](b: C)(f: (C, B) => C): C = s match
      case (x, y)=> f(b, y)

    override def foldRight[C](b: C)(f: (B, C) => C): C = foldLeft(b)(FunctionSyntax.flip(f))
  }
}
