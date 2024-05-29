package effects

import scala.concurrent.ExecutionContext.Implicits.global

object All extends effects.instance.All with effects.syntax.All with effects.utils.All
  with FunctorFunctions with TraversableFunctions with MonadFunctions

trait Effect[F[_], A] {
  def inst: F[A]
}


trait EffectConverter[F[_], E[H[_], _]] {
  def to[A](inst: F[A]): E[F, A]

  def from[A](e: E[F, A]): F[A]
}
