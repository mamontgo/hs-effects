package effects.instance

import effects.{Empty, Functor, Monad, MonoidConverter, Return}


case class Writer[W, A](runWriter:(A, W))

object Writer {
  def tell[M[_], B, W <: M[B]](x:M[B])(implicit c: MonoidConverter[M, B]): Writer[M[B], Unit] = Writer((), x)
}
trait WriterInstances {


  implicit class WriterInstanceImpl[M[_], B, W <: M[B], A](s: Writer[W, A])(implicit c: MonoidConverter[M, B]) extends WriterMonad(s) with WriterFunctor(s)

  implicit def writerReturn[M[_], B, W <: M[B], A](implicit empty: Empty[M, B]): Return[[F] =>> Writer[W, F]] = new Return[[F] =>> Writer[W, F]]:
    override def apply[C](a: C): Writer[W, C] = Writer((a, empty().asInstanceOf[W]))

  trait WriterMonad[M[_], B, W <: M[B], A](s: Writer[W, A])(implicit c: MonoidConverter[M, B]) extends Monad[[F] =>> Writer[W, F], A] {

    override def flatMap[C](f: A => Writer[W, C]): Writer[W, C] = s.runWriter match
      case (x, y) =>
        f(x).runWriter match
          case (x1, y1) =>
            Writer((x1, c.to(y).combine(y1).asInstanceOf[W]))


  }

  trait WriterFunctor[W, A](s: Writer[W, A]) extends Functor[[F] =>> Writer[W, F], A] {

    override def inst: Writer[W, A] = s

    override def map[C](f: A => C): Writer[W, C] = s.runWriter match
      case (x, y) => Writer((f(x), y))
  }
}

object WriterInstances extends WriterInstances