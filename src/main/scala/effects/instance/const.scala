package effects.instance

import effects.{Functor, FunctorConverter}


case class Const[A, B](getConst:A)

trait ConstInstances {

  implicit class ConstInstanceImpl[A, B](s: Const[A, B]) extends ConstFunctor(s)

  implicit def constFunctorConverter[A]: FunctorConverter[[F] =>> Const[A, F]] = new FunctorConverter[[F] =>> Const[A, F]]:
    override def to[B](inst: Const[A, B]): Functor[[F] =>> Const[A, F], B] = inst.functor

  trait ConstFunctor[A, B](s: Const[A, B]) extends Functor[[F] =>> Const[A, F], B] {
    override def map[C](f: B => C): Const[A, C] = Const(s.getConst)

    override def inst: Const[A, B] = s
  }
}

object ConstInstances extends ConstInstances