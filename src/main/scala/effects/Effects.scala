package effects

object All extends effects.instance.All with effects.syntax.All


trait Effect[F[_], A] {
  def inst: F[A]
}