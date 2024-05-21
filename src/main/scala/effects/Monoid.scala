package effects

trait Empty[F[_], A] {
  def apply(): F[A]
}

trait MonoidConverter[F[_], A]:
  def from(e: Monoid[F, A]): F[A] = e.inst
  def to(i: F[A]): Monoid[F, A]


trait Monoid[F[_], A] extends SemiGroup[F, A] with Effect[F, A] {
  def empty(implicit e: Empty[F, A]): F[A] = e()

  def monoid: Monoid[F, A] = this
}

object Monoid {
  def mconcat[F[_], A](l: Seq[F[A]])(implicit empty: Empty[F, A], c: MonoidConverter[F, A]): F[A] = {
    l.foldRight(empty()) { (a, b) => c.to(a).combine(b)
    }
  }
}

