package effects.instance

import effects.{Functor, FunctorConverter}


case class Identity[T](runIdentity:T)

trait IdentityInstances {

  implicit class IdentityInstanceImpl[A](s: Identity[A]) extends IdentityFunctor(s)

  implicit def identityFunctorConverter: FunctorConverter[Identity] = new FunctorConverter[Identity]:
    override def to[A](inst: Identity[A]): Functor[Identity, A] = inst.functor

  trait IdentityFunctor[A](s: Identity[A]) extends Functor[Identity, A] {
    override def map[B](f: A => B): Identity[B] = Identity(f(s.runIdentity))

    override def inst: Identity[A] = s
  }
}

object IdentityInstances extends IdentityInstances