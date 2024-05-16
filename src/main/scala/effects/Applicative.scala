package effects

import scala.annotation.targetName


trait Pure[F[_]] {
  def apply[A](a:A):F[A]
}

trait ApplicativeConverter[F[_]] extends EffectConverter[F, Applicative]:
  override def from[A](e: Applicative[F, A]): F[A] = e.inst


trait Applicative[F[_], A] extends Monad[F, A] {


  def pure[B](a:B)(implicit p:Pure[F]): F[B] = p(a)

  def ap[B](a: F[A => B]): F[B]

  @targetName("extAp")
  def <*>[B](a: F[A => B]): F[B] = ap(a)

  def applicative: Applicative[F, A] = this

}

object Applicative {

  // (A => B) => F[A] => F[B]
  def liftA[A, B](f: A => B): ApplicativeMap[A, B] = ApplicativeMap(f)
}

case class ApplicativeMap[A,B](f:A => B) {
  def apply[F[_]](x: Applicative[F, A]): F[B] = {
    x.map(f)
  }
}