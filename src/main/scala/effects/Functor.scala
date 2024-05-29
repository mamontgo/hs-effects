package effects

import scala.annotation.targetName

trait FunctorConverter[F[_]] extends EffectConverter[F, Functor]:
  override def from[A](e: Functor[F, A]): F[A] = e.inst



// F[A] => (A => B) => F[B]
trait Functor[F[_], A] extends Effect[F, A] {
  def map[B](f: A => B): F[B]

  @targetName("extMap")
  def <\>[B](f: A => B): F[B] = map(f)

  def functor: Functor[F, A] = this
  
}

trait FunctorFunctions {
  // (A => B) => F[A] => F[B]
  def liftF[A, B](f: A => B): FunctorMap[A, B] = FunctorMap(f)
}

object Functor extends FunctorFunctions

case class FunctorMap[A,B](f:A => B) {
  def apply[F[_]](x: Functor[F, A]): F[B] = {
    x.map(f)
  }
}