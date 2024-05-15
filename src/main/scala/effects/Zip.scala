package effects

trait Zip[F[_], A] extends Functor[F, A] {
  def zipWith[B,C](o:F[B])(zip: A => B => C): F[C]
  def zip: Zip[F, A] = this
}
