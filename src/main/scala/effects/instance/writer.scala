package effects.instance

import effects.syntax.FunctionSyntax
import effects.{Applicative, Empty, Foldable, Functor, Monad, MonadConverter, MonoidConverter, Return, Traversable}


case class Writer[W, A](runWriter:(A, W))

object Writer {
  def tell[M[_], B, W <: M[B]](x:M[B])(implicit c: MonoidConverter[M, B]): Writer[M[B], Unit] = Writer((), x)
}
trait WriterInstances {


  implicit class WriterInstanceImpl[M[_], B, W <: M[B], A](s: Writer[W, A])(implicit c: MonoidConverter[M, B]) extends
    WriterMonad(s) with WriterFunctor(s) with WriterApplicative(s)

  implicit def writerReturn[M[_], B, W <: M[B], A](implicit empty: Empty[M, B]): Return[[F] =>> Writer[W, F]] = new Return[[F] =>> Writer[W, F]]:
    override def apply[C](a: C): Writer[W, C] = Writer((a, empty().asInstanceOf[W]))

  trait WriterTraversable[W, B](s: Writer[W, B]) extends Traversable[[F] =>> Writer[W, F], B] {
    override def traverse[M[_], C](f: B => M[C])(implicit c: MonadConverter[M], r: Return[M]): M[Writer[W, C]] = {
      s.runWriter match
        case (x, y) => c.to(f(x)).map(c => Writer((c, y)))
    }
  }

  trait WriterFoldable[W, B](s: Writer[W, B]) extends Foldable[[F] =>> Writer[W, F], B] {
    override def foldLeft[C](b: C)(f: (C, B) => C): C = s.runWriter match
      case (x, y) => f(b, x)

    override def foldRight[C](b: C)(f: (B, C) => C): C = foldLeft(b)(FunctionSyntax.flip(f))

  }

  trait WriterApplicative[M[_], C, W <: M[C], A](s: Writer[W, A])(implicit c: MonoidConverter[M, C])  extends Applicative[[F] =>> Writer[W, F], A] {
    override def ap[B](a: Writer[W, A => B]): Writer[W, B] = {
      s.runWriter match
        case (x,y) => a.runWriter match
          case (x1, y1) => Writer( (x1(x), c.to(y).combine(y1).asInstanceOf[W]) )
    }
  }

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