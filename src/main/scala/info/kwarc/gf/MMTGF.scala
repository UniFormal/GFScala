package info.kwarc.gf

import info.kwarc.mmt.api.{DPath, LocalName}
import info.kwarc.mmt.api.frontend.Extension
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.objects.{OMSemiFormal, Term}
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lf.ApplySpine

class MMTGF extends Extension {
  override def logPrefix: String = "gf"

  val dpath = DPath(URI.http colon "mathhub.info") / "Teaching" / "LBS"
  val key = dpath ? "LogicSyntax" ? "correspondsTo"

  def getGrammar(th : DeclaredTheory) = {
    val strings = th.metadata.get(key).map(_.value.asInstanceOf[OMSemiFormal].toStr(true).filter(_!='"'))
    println(strings)
    Grammar(strings:_*)
  }

  private def get (s : String,th : DeclaredTheory) = th.get(LocalName(s)) match {
    case c : Constant if c.df.isDefined => c.df.get
    case c : Constant => c.toTerm
  }

  private def toOMDocRec(expr : GFExpr, th : DeclaredTheory) : Term = expr match {
    case GFStr(s) => get(s,th)
    case GFA(fun,args) => ApplySpine(get(fun,th),args.map(toOMDocRec(_,th)):_*)
  }

  def toOMDoc(expr : GFExpr, th : DeclaredTheory) = controller.simplifier.apply(toOMDocRec(expr,th),th.getInnerContext)
}
