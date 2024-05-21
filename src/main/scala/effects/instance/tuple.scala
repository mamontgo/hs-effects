package effects.instance

import effects.syntax.FunctionSyntax
import effects.{Applicative, ApplicativeConverter, Empty, Foldable, Functor, FunctorConverter, Monad, MonadConverter, Monoid, Pure, Return}


trait TupleInstances {


  implicit def tupleFunctorConverter[B]: FunctorConverter[[E] =>> (B, E)] = new FunctorConverter[[E] =>> (B, E)]:
    override def to[A](inst: (B, A)): Functor[[E] =>> (B, E), A] = inst.functor

  implicit def tupleMonadConverter[B]: MonadConverter[[E] =>> (B, E)] = new MonadConverter[[E] =>> (B, E)]:
    override def to[A](inst: (B, A)): Monad[[E] =>> (B, E), A] = inst.monad

  implicit def tupleApplicativeConverter[B]: ApplicativeConverter[[E] =>> (B, E)] = new ApplicativeConverter[[E] =>> (B, E)]:
    override def to[A](inst: (B, A)): Applicative[[E] =>> (B, E), A] = inst.applicative

  implicit def emptyTuple[F[_], B](implicit empty: Empty[F, B]): Empty[[E] =>> (?, E), F[B]] = () => (empty(), empty())

  implicit def tupleReturn[B[_] <: Monoid[B, C], C](implicit empty: Empty[B, C]): Return[[F] =>> (?, F)] = new Return[[F] =>> (?, F)]:
    override def apply[A](a: A): (B[C], A) = (empty(), a)


  implicit def pureTuple[B[_] <: Monoid[B, C], C](implicit empty: Empty[B, C]): Pure[[F] =>> (?, F)] = new Pure[[F] =>> (?, F)]:
    override def apply[A](a: A): (?, A) = (empty(), a)

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


  trait TupleMonoid[A, E[_], B](s: (A, Monoid[E, B])) extends Monoid[[F] =>> (A, F), E[B]] {

    override def inst: (A, E[B]) = s.map(_.inst)

    override def combine(y: (A, E[B])): (A, E[B]) = {
      for {
        f <- s
        n <- y
      } yield f.combine(n)
    }
  }
}
