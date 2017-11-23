package info.kwarc.gf

import info.kwarc.mmt.api.{DPath, LocalName, MPath}
import info.kwarc.mmt.api.frontend.Extension
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.objects.{OMSemiFormal, Term}
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lf.ApplySpine
import org.grammaticalframework.pgf.PGF

import scala.collection.immutable.HashMap

class InstantiatedGrammar(pgf : PGF, val theory : DeclaredTheory, val asHashMap : HashMap[String,Constant],mmtgf : MMTGF) extends Grammar(pgf) {
  def toOMDoc(expr : GFExpr) = mmtgf.toOMDoc(this,expr)
}

class MMTGF extends Extension {
  override def logPrefix: String = "gf"

  val dpath = DPath(URI.http colon "mathhub.info") / "Teaching" / "LBS"
  val key = dpath ? "LogicSyntax" ? "correspondsTo"

  def getGrammar(th : DeclaredTheory) : InstantiatedGrammar = {
    val strings = th.metadata.get(key).map(_.value.asInstanceOf[OMSemiFormal].toStr(true).filter(_!='"'))
    log(th.path + " -> " + strings.mkString(", "))
    new InstantiatedGrammar(Grammar(strings:_*).pgf, th,theoryToMap(th),this)
  }

  def getGrammar(mp : MPath) : InstantiatedGrammar = {
    controller.getO(mp) match {
      case Some(th : DeclaredTheory) => getGrammar(th)
      case _ => ???
    }
  }

  private def theoryToMap(th : DeclaredTheory) = {
    controller.simplifier(th)
    val consts = th.getConstants ::: th.getIncludesWithoutMeta.map(controller.get).collect {
      case t : DeclaredTheory =>
        controller.simplifier(t)
        t.getConstants
    }.flatten
    log(th.path + "\n" + consts.map(c => (c.name.toString,c)).mkString("\n"))
    HashMap(consts.map(c => (c.name.toString,c)):_*)
  }

  private def getTerm (s : String,th : HashMap[String,Constant]) = th.get(s) match {
    case Some(c : Constant) if c.df.isDefined => c.df.get
    case Some(c : Constant) => c.toTerm
    case _ => ???
  }

  private def toOMDocRec(expr : GFExpr, th : HashMap[String,Constant]) : Term = expr match {
    case GFStr(s) => getTerm(s,th)
    case GFA(fun,args) => ApplySpine(getTerm(fun,th),args.map(toOMDocRec(_,th)):_*)
  }

  def toOMDoc(gr : InstantiatedGrammar,expr : GFExpr) = controller.simplifier.apply(
    toOMDocRec(expr,gr.asHashMap),
    gr.theory.getInnerContext
  )
}
