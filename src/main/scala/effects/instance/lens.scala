package effects.instance

import effects.{All, Functor, FunctorConverter}


//type LensGetter2[S, A] = S => Const[A, S]
//type LensGetter s a = s -> Const a s

//view :: LensGetter s a -> s -> a
//view lens s = getConst(lens s)


//type LensGetter[S, A] = (A => Const[A, A]) => (S => Const[A, S])
//
//type LensModify[S, A] = (A => Identity[A]) => (S => Identity[S])

//type LensGetters a = (a -> Const a a) -> (s -> Const a s)
//view :: LensGetter s a -> s -> a
//view lens s = getConst(lens Const s)


trait Lens[S, A]{
  def apply[F[_]](a: A => Functor[F, A]): S => F[S]

//  def compose[F[_], O](o: Lens[A, O])(implicit functorConverter: FunctorConverter[F]) = {
//    val f2 = this
//    new Lens[S, O] {
//      override def apply(f: O => Functor[F, O]): S => F[S] = {
//        val res: A => F[A] = o(f)
//        val resX = (a:A) => functorConverter.to(res(a))
//        f2(resX)
//      }
//    }
//  }

}

// Lens[Person, Address]  //Lens[Address, String]

object Lens {

  private val identityConverter = All.identityFunctorConverter
  private def constConverter[A] = All.constFunctorConverter[A]

  def lens[S, A](getter: S => A)(setter: S => A => S): Lens[S, A] = new Lens[S, A] {
    override def apply[F[_]](f: A => Functor[F, A]): S => F[S] = {
      (s: S) => f(getter(s)).map(a => setter(s)(a))
    }
  }

  def over[S, A](lens: Lens[S, A], f:A => A, s:S):S = {
    lens((a:A) => identityConverter.to(Identity(f(a))))(s).runIdentity
  }

  def view[S, A](lens: Lens[S, A], s: S): A = {
    lens((a:A) => constConverter[A].to(Const[A, A](a)))(s).getConst
  }

//  def view3[S, A](lens: LensGetter[S, A], s: S): A = {
//    val temp = a => Const[A, A](a)
//    val check = lens(temp)(s)
//    lens(temp)(s).getConst
//  }
//
//  def over2[S, A](lens: LensModify[S, A], f: A => A, s:S): S = {
//    val i = (a:A) => Identity(f(a))
//    val l = lens(i)(s)
//    l.runIdentity
//  }
//
//
//
//  def view2[S, A](lens: LensGetter2[S, A], s: S): A = {
//    lens(s).getConst
//  }

//  def over[F[_] <: Functor[F, _], A, S](lens: Lens[F, A, S], f: A => A, s:S):S = {
//    val x = identity[A].compose(f)
//    Lens(identity.compose(f)(s))
//  }

}