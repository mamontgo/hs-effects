package effects

import scala.annotation.targetName

trait AlternativeConverter[F[_]] extends EffectConverter[F, Alternative]:
  override def from[A](e: Alternative[F, A]): F[A] = e.inst

trait Alternative[F[_], A] extends Applicative[F, A] {
  def empty: F[A]
  def alt: F[A] => F[A]

  @targetName("extAlt")
  def <|>(x:F[A]):F[A] = alt(x)

  def alternative: Alternative[F, A] = this
}
