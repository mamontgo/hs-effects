package effects

import scala.annotation.targetName

trait MonadConverter[F[_]] extends EffectConverter[F, Monad]:
  override def from[A](e: Monad[F, A]): F[A] = e.inst

trait Return[F[_]] {
  def apply[A](a: A): F[A]

  def monad[A](a: A): Monad[F, A]

  def toMonad[A](a: F[A]): Monad[F, A]
}


// M[A] => (A => M[B]) => M[B]
trait Monad[F[_], A] extends Functor[F, A] {
  def flatMap[B](f: A => F[B]): F[B]

  @targetName("extFlatMap")
  def >>=[B](f: A => F[B]): F[B] = flatMap(f)

  @targetName("extFlatMapSingle")
  def >>[B](f:F[B]): F[B] = flatMap(_ => f)

  def monad: Monad[F, A] = this

}


object Monad {

  // M[M[A]] => M[A]
  def join[F[_], B](a: Monad[F, F[B]]): F[B] = a.flatMap(identity)

  
  // F[M[A]] => M[F[A]]
  def sequence[M[_], F <: Monad[M,A], A](s: Seq[F])(implicit p: Return[M]):M[Seq[A]] = {
    mapM(identity[F])(s)(p)
  }

  def mapM[M[_], A, B](f:A => Monad[M, B])(l:Seq[A])(implicit p: Return[M]): M[Seq[B]] = {
    val foldFunction: (A, Monad[M, Seq[B]]) => Monad[M, Seq[B]] = (a:A, r: Monad[M, Seq[B]]) => {
       val y = for {
        x <- f(a)
        xs <- r
      } yield Seq(x) ++ xs
        p.toMonad(y)
    }
    l.foldRight(p.monad(Seq()))(foldFunction).inst
  }

}
