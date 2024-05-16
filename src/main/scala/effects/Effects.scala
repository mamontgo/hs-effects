package effects

object All extends effects.instance.All with effects.syntax.All


trait Effect[+F[_], A] {
  def inst: F[A]
}


trait EffectConverter[F[_], E[H[_], _]] {
  def to[A](inst: F[A]): E[F, A]

  def from[A](e: E[F, A]): F[A]
}
