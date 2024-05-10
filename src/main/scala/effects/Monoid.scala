package effects

trait Empty[F[_]] {
  def apply[A](): F[A]
}


trait Monoid[F[_], A] extends SemiGroup[F, A] with Effect[F, A]{
  def empty:F[A]
  def monoid: Monoid[F, A] = this
}


