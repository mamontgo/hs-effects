package effects

import scala.annotation.tailrec

trait ZipConverter[F[_]] extends EffectConverter[F, Zip] {
  def from[A](a:Zip[F, A]): F[A] = a.inst
  def to[A](a:F[A]): Zip[F,A]
}

trait Zip[F[_], A] extends Functor[F, A] {
  def zipWith[B, C](o:F[B])(zip: A => B => C): F[C]
  def asZip: Zip[F, A] = this
}

object Zip {

  def zipSeq[F[_], A](f: A => A => A, l: Seq[F[A]])(implicit c: ZipConverter[F]): Option[F[A]] = {
    l match
      case x::xs => Some(zipAll(f, x, xs))
      case Nil => None
  }


  def zipAll[F[_], A, B](f: A => B => B, init:F[B], l:Seq[F[A]] )(implicit c: ZipConverter[F]) : F[B] = {
    l.foldLeft(init) { (i, v) => c.to(v).zipWith(i)(f)  }
  }

}