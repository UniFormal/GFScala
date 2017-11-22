package info.kwarc.gf

import org.grammaticalframework.pgf.Expr
import util._


sealed abstract class GFExpr {
  private[gf] def gf : Expr
  def gfstring : String
}

case class GFStr(s : String) extends GFExpr {
  private[gf] lazy val gf : Expr = Expr.readExpr(s)

  override def toString: String = "\"" + s + "\""
  def gfstring = s
}
case class GFA(fun : String, args : List[GFExpr]) extends GFExpr {
  private[gf] lazy val gf : Expr = Expr.readExpr(gfstring)

  override def toString: String = fun.toString + {if (args.isEmpty) "" else args.mkString("(",",",")")}
  def gfstring = fun + " " + args.map(_.gfstring).mkString(" ")
}

object GFExpr {
  def parse(s : String) : GFExpr = fromGF(Expr.readExpr(s))

  private[gf] def fromGF(expr : Expr) : GFExpr = expr match {
    case e if e.unApp() != null =>
      val ap = e.unApp()
      GFA(ap.getFunction,ap.getArguments.toList.map(fromGF))
    case e if e.unStr() != null =>
      GFStr(e.unStr())
    case _ =>
      println(expr.toString)
      ???
      // TODO unAbs, unFloat, unMeta (missing??)
  }
}
