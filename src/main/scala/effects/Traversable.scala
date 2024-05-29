package effects

trait TraversableConverter[F[_]] extends EffectConverter[F, Traversable]:
  override def from[A](e: Traversable[F, A]): F[A] = e.inst

trait Traversable[F[_], A] extends Functor[F, A] with Foldable[F, A] {
  def traverse[M[_], B](f: A => M[B])(implicit c: MonadConverter[M], r: Return[M]): M[F[B]]
  def traversable: Traversable[F, A] = this
}

trait TraversableFunctions {
  def sequenceA[T[_], A[_], V](a: T[A[V]])(implicit tc: TraversableConverter[T], c: MonadConverter[A], r: Return[A]): A[T[V]] = {
    tc.to(a).traverse(identity)
  }

  def mapM[T[_], M[_], A, B](f: A => M[B])(l: T[A])(implicit tc: TraversableConverter[T], pure: Pure[T], e: Empty[T, B], tm: MonoidConverter[T, B], c: MonadConverter[M], returnM: Return[M]): M[T[B]] = {
    val foldFunction: (A, Monad[M, T[B]]) => Monad[M, T[B]] = (a: A, r: Monad[M, T[B]]) => {
      val res = for {
        x <- c.to(f(a))
        xs <- r
      } yield tm.to(pure(x)).combine(xs)
      c.to(res)
    }

    val x = c.to(returnM(e()))
    tc.to(l).foldRight(x)(foldFunction).inst
  }
}


object Traversable extends TraversableFunctions