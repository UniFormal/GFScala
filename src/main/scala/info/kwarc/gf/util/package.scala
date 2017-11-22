package info.kwarc.gf

import java.util.function.{BiConsumer, Consumer}

import org.grammaticalframework.pgf.{Concr, Expr, PGF}

import scala.collection.immutable.HashMap

package object util {
  implicit def asScala[A, B](map: java.util.Map[A, B]): HashMap[A, B] = {
    var ls: List[(A, B)] = Nil
    map.forEach(new BiConsumer[A,B]{ override def accept(p : A, q : B) = ls ::= (p, q) })
    HashMap(ls.reverse: _*)
  }

  implicit def asScala[A](it: java.lang.Iterable[A]): List[A] = {
    var ls: List[A] = Nil
    it.forEach(new Consumer[A]{override def accept(a:A) = ls ::= a})
    ls.reverse
  }

  // java.lang.Arr

  private[gf] implicit def PGFasScala(pgf: PGF) = new Grammar(pgf)

  private[gf] implicit def ConcasScala(conc: Concr, pgf: PGF) = new Language(conc, pgf)

  private[gf] implicit def ExpasScala(exp: Expr) = GFExpr.fromGF(exp)
}