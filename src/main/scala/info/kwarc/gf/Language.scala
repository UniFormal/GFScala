package info.kwarc.gf

import info.kwarc.gf.util._
import org.grammaticalframework.pgf.{BIND, Bracket, Concr, PGF}

class Language(private[gf] val conc : Concr,private[gf] val pgf : PGF) {
  val grammar = new Grammar(pgf)

  def parse(s : String) : List[(GFExpr,Double)] = {
    val epls = conc.parse(pgf.getStartCat,s)
    epls.map(ep => (ExpasScala(ep.getExpr),ep.getProb))
  }

  def lexicon : List[String] = conc.fullFormLexicon.map(_.getForm)

  def canLinearize(s : String) : Boolean = conc.hasLinearization(s)

  def linearize(expr : GFExpr) : List[Linearization] = conc.bracketedLinearize(expr.gf).toList.map(bracketToLinearization)

  private def bracketToLinearization(a : Any) : Linearization = a match {
    case s : java.lang.String => StringLinearization(s)
    case b : Bracket => LinearizationBracket(b.cat,b.fid,b.fun,b.lindex,b.children.toList.map(bracketToLinearization))
    case b : BIND =>
      StringLinearization("BIND_" + b)
  }

  // TODO
  private def parseWithHeuristics(s : String, factor : Double) : List[(GFExpr,Double)] = {
    val pls = conc.parseWithHeuristics(pgf.getStartCat,s,factor,???)
    ???
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case that : Language => this.conc == that.conc && this.pgf == that.pgf
    case _ => false
  }
}

object Language {

}

sealed abstract class Linearization

case class StringLinearization(string : String) extends Linearization {
  override def toString = string
}

case class LinearizationBracket(cat : String, id : Int, fun : String, lindex : Int, children : List[Linearization]) extends Linearization {
  override def toString = "[ " + children.map(_.toString).mkString(" | ") + " ]"
}

