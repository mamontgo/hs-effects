package effects.instance

import effects.{Applicative, Empty, Foldable, Functor, Monad, Monoid, Pure}

import scala.language.implicitConversions


private trait ListInstances {

  implicit def pureSeq: Pure[Seq] = new Pure[Seq]:
    override def apply[A](a: A): Seq[A] = Seq(a)

  implicit def asSeqMonad[A](s: Seq[A]):SeqMonad[A] = SeqMonad(s)

  class SeqMonad[A](s: Seq[A]) extends SeqApplicative(s) with SeqMonoid(s) with Monad[Seq, A]  {
    override def flatMap[B](f: A => Seq[B]): Seq[B] = s.flatMap(f)
  }

  class SeqApplicative[A](s: Seq[A]) extends SeqFunctor(s) with SeqFoldable(s) with Applicative[Seq, A] {

    override def ap[B](a: Seq[A => B]): Seq[B] = a.flatMap(f => s.map(v => f(v)))
  }

  trait SeqFoldable[A](s: Seq[A]) extends Foldable[Seq, A] {

    override def foldLeft[B](b: B)(f: (B, A) => B): B = s.foldLeft(b)(f)

    override def foldRight[B](b: B)(f: (A, B) => B): B = s.foldRight(b)(f)
  }


  trait SeqFunctor[A](s: Seq[A]) extends Functor[Seq, A] {
    override def map[B](f: A => B): Seq[B] = s.map(f)
  }

  trait SeqMonoid[A](s: Seq[A]) extends Monoid[Seq, A] {
    override def combine(y: Seq[A]): Seq[A] = s ++ y

    override def empty: Seq[A] = Seq()
  }
}

object ListInstances extends ListInstances
