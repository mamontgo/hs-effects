package effects

import effects.instance.All
import scala.annotation.targetName

// M[A] => (A => M[B]) => M[B]
trait Monad[F[_], A] extends Applicative[F, A] {
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
  
  def sequence[M[_] <: Monad[M, _], A](s: Seq[M[A]])(implicit p: Pure[M]):M[Seq[A]] = {
    mapM(identity[M[A]])(s)(p)
  }
  
  def mapM[M[_] <: Monad[M, _], A, B](f:A => M[B])(l:Seq[A])(implicit p: Pure[M]): M[Seq[B]] = {
    val foldFunction: (M[Seq[B]], A) => M[Seq[B]] = (r: M[Seq[B]], a: A) => {
       for {
        x <- f(a)
        xs <- r
      } yield Seq(x.asInstanceOf[B]) ++ xs.asInstanceOf[Seq[B]]
    }
    val x: M[Seq[B]] = p(Seq())
    l.foldLeft(x)(foldFunction)
  }

}
