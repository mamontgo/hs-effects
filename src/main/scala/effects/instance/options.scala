package effects.instance

import effects.syntax.FunctionSyntax
import effects.*

private trait OptionInstances {

  implicit def optionMonoidConverter[F[_], A](implicit innerConverter: MonoidConverter[F, A]): MonoidConverter[Option, F[A]] = (inst: Option[F[A]]) => {
    OptionMonoidEffectTypeClass(inst).monoid
  }

  implicit def optionAlternativeConverter: AlternativeConverter[Option] = new AlternativeConverter[Option]:
    override def to[A](inst: Option[A]): Alternative[Option, A] = inst.alternative

  implicit def optionTraversableConverter: TraversableConverter[Option] = new TraversableConverter[Option]:
    override def to[A](inst: Option[A]): Traversable[Option, A] = inst.traversable

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

  implicit class OptionInstanceImpl[A](s: Option[A]) extends OptionApplicative(s) with OptionMonad(s) with OptionFunctor(s)
    with OptionTraversable(s) with OptionFoldable(s) with OptionZip(s) with OptionAlternative(s)

  implicit class OptionMonoidEffectTypeClass[B, F[_]](a: Option[F[B]])(implicit c:MonoidConverter[F, B]) extends OptionMonoid(a)(c)

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

  trait OptionMonoid[O[_], B](s: Option[O[B]])(implicit c:MonoidConverter[O, B]) extends Monoid[Option, O[B]] {


    override def combine(y: Option[O[B]]): Option[O[B]] = {

      (s, y) match
        case (None, o) => o
        case (x, None) => x.inst
        case _ =>
          for {
            inS <- s
            inY <- y
          } yield c.to(inS).combine(inY)



    }

    override def inst: Option[O[B]] = s
  }

  trait OptionZip[A](s: Option[A]) extends Zip[Option, A] {
    override def zipWith[B, C](o: Option[B])(zip: A => B => C): Option[C] = for {
      si <- s
      oi <- o
    } yield zip(si)(oi)
  }

  trait OptionTraversable[A](s: Option[A]) extends Traversable[Option, A] {
    override def traverse[M[_], B](f: A => M[B])(implicit c: MonadConverter[M], r: Return[M]): M[Option[B]] = {
      s match
        case None => r(None)
        case Some(x) => c.to(f(x)).map(Some(_))

    }
  }


  trait OptionFoldable[A](s: Option[A]) extends Foldable[Option, A] {
    def foldLeft[B](b: B)(f: (B, A) => B): B = s.fold(b)(a => f(b, a))

    def foldRight[B](b: B)(f: (A, B) => B): B = foldLeft(b)(FunctionSyntax.flip(f))
  }

  trait OptionAlternative[A](s: Option[A]) extends Alternative[Option, A] {

    override def empty: Option[A] = None

    override def alt: Option[A] => Option[A] = a =>  {
      (s, a) match
        case (x@Some(_), _) => x
        case (_, x) => x
    }
  }
}


object OptionInstances extends OptionInstances