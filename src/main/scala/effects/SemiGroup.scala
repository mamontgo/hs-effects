package effects

trait SemiGroup[F[_], A] {
  def combine(y: F[A]): F[A]
  def semiGroup: SemiGroup[F, A] = this
}
