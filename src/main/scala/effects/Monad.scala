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

//  def mapM[M[_], A, B](f:A => Monad[M, B])(l:Seq[A]): Monad[M, Seq[B]] = {
//
//    val foldFunction: (A, Monad[M, Seq[B]]) => M[Seq[B]] = (a: A, r: Monad[M, Seq[B]]) => {
//       for {
//        x <- f(a)
//        xs <- r
//      } yield Seq(x) ++ xs
//    }
//    val x = pure[M, Seq[B]](Seq())
//  }

}
