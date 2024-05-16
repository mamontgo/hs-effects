package effects.instance

import effects.syntax.FunctionSyntax
import effects.*

private trait OptionInstances {

  implicit def optionZipConverter: ZipConverter[Option] = new ZipConverter[Option]:
    override def to[A](a: Option[A]): Zip[Option, A] = a.asZip
    
  implicit def optionFunctorConverter: FunctorConverter[Option] = new FunctorConverter[Option]:
    override def to[A](inst: Option[A]): Functor[Option, A] = inst.functor

  implicit def optionMonadConverter: MonadConverter[Option] = new MonadConverter[Option]:
    override def to[A](inst: Option[A]): Monad[Option, A] = inst.monad

  implicit def optionApplicativeConverter: ApplicativeConverter[Option] = new ApplicativeConverter[Option]:
    override def to[A](inst: Option[A]): Applicative[Option, A] = inst.applicative
  
  implicit def emptyOption[A]: Empty[Option, A] = () => None

  implicit def returnOption: Return[Option] = new Return[Option]:
    override def apply[A](a: A): Option[A] = Some(a)


  implicit def pureOption: Pure[Option] = new Pure[Option]:
    override def apply[A](a: A): Option[A] = Some(a)

    override def ap[A](a: A): Applicative[Option, A] = Some(a)

  implicit class OptionInstanceImpl[A](s: Option[A]) extends OptionApplicative(s) with OptionMonad(s) with OptionFunctor(s) with OptionFoldable(s) with OptionZip(s)
  implicit class OptionMonoidEffectTypeClass[B, F[_]](a: Option[Monoid[F, B]]) extends OptionMonoid(a)

  trait OptionMonad[A](s: Option[A]) extends Monad[Option, A] {
    override def flatMap[B](f: A => Option[B]): Option[B] = s.flatMap(f)
  }

  trait OptionApplicative[A](s: Option[A]) extends Applicative[Option, A] {

    override def ap[B](a: Option[A => B]): Option[B] = a.flatMap(s.map)
  }

  trait OptionFunctor[A](s: Option[A]) extends Functor[Option, A] {
    override def map[B](f: A => B): Option[B] = s.map(f)

    override def inst: Option[A] = s
  }

  trait OptionMonoid[O[_], B](s: Option[Monoid[O, B]]) extends Monoid[Option, O[B]] {


    override def combine(y: Option[O[B]]): Option[O[B]] = {
      for {
        inS <- s
        inY <- y
      } yield inS.combine(inY)
    }

    override def inst: Option[O[B]] = s.map(_.inst)
  }

  trait OptionZip[A](s: Option[A]) extends Zip[Option, A] {
    override def zipWith[B, C](o: Option[B])(zip: A => B => C): Option[C] = for {
      si <- s
      oi <- o
    } yield zip(si)(oi)
  }
  
  trait OptionFoldable[A](s: Option[A]) extends Foldable[Option, A] {
    def foldLeft[B](b: B)(f: (B, A) => B): B = s.fold(b)(a => f(b, a))

    def foldRight[B](b: B)(f: (A, B) => B): B = foldLeft(b)(FunctionSyntax.flip(f))
  }
}


object OptionInstances extends OptionInstances