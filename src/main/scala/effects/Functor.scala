package effects

import scala.annotation.targetName


// F[A] => (A => B) => F[B]
trait Functor[F[_], A] extends Effect[F, A] {
  def map[B](f: A => B): F[B]

  @targetName("extMap")
  def <\>[B](f: A => B): F[B] = map(f)

  def functor: Functor[F, A] = this
  
}

object Functor {

  // (A => B) => F[A] => F[B]
  def liftF[A, B](f: A => B): FunctorMap[A, B] = FunctorMap(f)
}

case class FunctorMap[A,B](f:A => B) {
  def apply[F[_]](x: Functor[F, A]): F[B] = {
    x.map(f)
  }
}