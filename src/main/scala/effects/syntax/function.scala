package effects.syntax

import scala.annotation.targetName


trait FunctionSyntax {
  implicit class FunctionApplication[A, B](f: A => B) {
    @targetName("exec") def <>(a: A): B = withParam(a)

    def withParam(a: A): B = f(a)
  }

  implicit class BiFunctionApplication[A, B, C](f: (A, B) => C) {
    def flip: (B, A) => C = FunctionSyntax.flip(f)
  }
}

object FunctionSyntax extends FunctionSyntax {
  def flip[A, B, C](f: (A, B) => C): (B, A) => C = (b: B, a: A) => f(a, b)

  def const[A, B](a: A)(b: B): A = a
}