package effects.instance

import effects.{Functor, Monoid}


object StringSetup {

  def asCharSeq(s:Seq[Char]):String = s.toString()

//  implicit class StringUpdate(s:java.lang.String) extends Functor[Seq, Char] with Monoid[Seq, Char] {
//    override def map[B](f: Char => B): Seq[B] = s.toSeq.map(f)
//
//    override def empty: Seq[Char] = Seq()
//
//    override def combine(y: Seq[Char]): Seq[Char] = s++y
//  }
  
}