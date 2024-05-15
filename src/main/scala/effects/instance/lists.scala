package effects.instance

import effects.{Applicative, Empty, Foldable, Functor, Monad, Monoid, Pure, Return, Zip}

import scala.language.implicitConversions


private trait ListInstances {

  implicit def emptySeq[A]: Empty[Seq, A] = () => Seq()

  implicit def returnSeq: Return[Seq] = new Return[Seq]:
    override def apply[A](a: A): Seq[A] = Seq(a)
    override def monad[A](a: A): Monad[Seq, A] = this(a).monad
    override def toMonad[A](a: Seq[A]): Monad[Seq, A] = a.monad

  implicit def pureSeq: Pure[Seq] = new Pure[Seq]:
    override def apply[A](a: A): Seq[A] = Seq(a)
    override def ap[A](a: A): Applicative[Seq, A] = Seq(a)

  implicit class SeqInstanceImpl[A](s: Seq[A]) extends SeqApplicative(s) with SeqMonad(s) with SeqFunctor(s) with SeqMonoid(s) with SeqFoldable(s) with SeqZip(s)

  trait SeqMonad[A](s: Seq[A]) extends  Monad[Seq, A]  {
    override def flatMap[B](f: A => Seq[B]): Seq[B] = s.flatMap(f)
  }

  trait SeqApplicative[A](s: Seq[A]) extends Applicative[Seq, A] {

    override def ap[B](a: Seq[A => B]): Seq[B] = a.flatMap(f => s.map(v => f(v)))
  }

  trait SeqFoldable[A](s: Seq[A]) extends Foldable[Seq, A] {

    override def foldLeft[B](b: B)(f: (B, A) => B): B = s.foldLeft(b)(f)

    override def foldRight[B](b: B)(f: (A, B) => B): B = s.foldRight(b)(f)
  }


  trait SeqFunctor[A](s: Seq[A]) extends Functor[Seq, A] {
    override def map[B](f: A => B): Seq[B] = s.map(f)

    override def inst: Seq[A] = s
  }

  trait SeqZip[A](s: Seq[A]) extends Zip[Seq, A] {
    override def zipWith[B, C](o: Seq[B])(zip: A => B => C): Seq[C] = s.zip(o).map(t => zip(t._1)(t._2))
  }


  trait SeqMonoid[A](s: Seq[A]) extends Monoid[Seq, A] {
    override def combine(y: Seq[A]): Seq[A] = s ++ y
  }
}

object ListInstances extends ListInstances
