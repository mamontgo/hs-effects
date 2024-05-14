package effects.syntax

import scala.annotation.targetName


trait FunctionSyntax {
  implicit class FunctionApplication[A, B](f: A => B) {
    @targetName("exec") def <->(a: A): B = withParam(a)

    def withParam(a: A): B = f(a)
  }

  implicit class BiFunctionApplication[A, B, C](f: (A, B) => C) {
    def flip: (B, A) => C = FunctionSyntax.flip(f)
  }

  def flip[A, B, C](f: (A, B) => C): (B, A) => C = (b: B, a: A) => f(a, b)

  def const[A, B](a: A)(b: B): A = a

  def curry[A, B, C](f: (A, B) => C): A => B => C = (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  def id[A](a: A): A = a

  def fst[A, B]: (A, B) => A = (a: A, b: B) => a

  def snd[A, B]: (A, B) => B = (a: A, b: B) => b
}

object FunctionSyntax extends FunctionSyntax