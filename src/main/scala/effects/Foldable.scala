package effects

trait Foldable[F[_], A]  {
  def foldLeft[B](b: B)(f: (B, A) => B): B
  def foldRight[B](b: B)(f: (A, B) => B): B
}
