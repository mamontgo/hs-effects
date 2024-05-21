package effects

  
trait Foldable[F[_], A]  {
  def foldLeft[B](b: B)(f: (B, A) => B): B
  def foldRight[B](b: B)(f: (A, B) => B): B
  def foldMap[M[_], B](f: A => M[B])(implicit empty: Empty[M, B], c: MonoidConverter[M, B]): M[B] = {
    foldRight(empty()) { (v, i) => c.to(f(v)).combine(i) }
  }
}
