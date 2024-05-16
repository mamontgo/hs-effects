package effects

import scala.annotation.targetName

trait MonadConverter[F[_]] extends EffectConverter[F, Monad]:
  override def from[A](e: Monad[F, A]): F[A] = e.inst

trait Return[F[_]] {
  def apply[A](a: A): F[A]
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
  def join[F[_], B](a:F[F[B]])(implicit c: MonadConverter[F]): F[B] = {
    c.to(a).flatMap(identity)
  }

  
  // Seq[F[A]] => F[Seq[A]]
  def sequence[M[_], A](s: Seq[M[A]])(implicit c: MonadConverter[M], r: Return[M]):M[Seq[A]] = {
    mapM(identity[M[A]])(s)(c, r)
  }

  def mapM[M[_], A, B](f:A => M[B])(l:Seq[A])(implicit c: MonadConverter[M], r: Return[M]): M[Seq[B]] = {
    val foldFunction: (A, Monad[M, Seq[B]]) => Monad[M, Seq[B]] = (a:A, r:  Monad[M, Seq[B]]) => {
       val res = for {
        x <- c.to(f(a))
        xs <- r
      } yield Seq(x) ++ xs
        c.to(res)
    }

    l.foldRight(c.to(r(Seq[B]())))(foldFunction).inst
  }

}
