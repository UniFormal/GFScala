package info.kwarc.gf

import info.kwarc.gf.util.{ConcasScala, asScala}
import info.kwarc.mmt.api.{DPath, LocalName, MPath}
import info.kwarc.mmt.api.frontend.Extension
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.objects._
import Conversions._
import info.kwarc.mmt.api.symbols.{Constant, FinalConstant}
import info.kwarc.mmt.api.utils.{File, URI}
import info.kwarc.mmt.lf.{ApplySpine, Lambda}
import org.grammaticalframework.pgf.{Concr, PGF}

import scala.collection.immutable.HashMap

class InstantiatedGrammar(pgf : PGF, val theory : DeclaredTheory, val asHashMap : HashMap[String,Constant],mmtgf : MMTGF) extends Grammar(pgf) {
  def toOMDoc(expr : GFExpr) : Term = mmtgf.toOMDoc(this,expr)

  override def languages: HashMap[String, InstantiatedLanguage] =
    HashMap(asScala(pgf.getLanguages).toList.map(p => (p._1,new InstantiatedLanguage(p._2,pgf,this,mmtgf))):_*)
}

class InstantiatedLanguage(conc : Concr, pgf : PGF, override val grammar : InstantiatedGrammar,mmtgf : MMTGF) extends Language(conc,pgf) {
  def parseMMT(s : String) = mmtgf.parse(this,s)
}

class MMTGF extends Extension {
  override def logPrefix: String = "gf"
  private def present(tm : Term) = controller.presenter.asString(tm)

  def getGrammar(th : DeclaredTheory) : InstantiatedGrammar = {
    val fstrings = th.metadata.get(MMTGF.key).map(_.value.asInstanceOf[OMSemiFormal].toStr(true).filter(_!='"'))
    val strings = controller.backend.resolveLogical(th.parent.doc.uri) match {
      case Some((archive,_)) => fstrings.map(s => archive.root.resolve(s).toString)
      case _ => fstrings
    }
    log(th.path + " -> " + strings.mkString(", "))
    val gr = Grammar(strings:_*)
    log("Languages: " + gr.languages.keys.mkString(", "))
    new InstantiatedGrammar(gr.pgf, th,theoryToMap(th),this)
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
    HashMap(consts.map(c => (c.name.toString,c)):_*)
  }

  private val trav = new StatelessTraverser {
    override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
      case OMS(gn) => controller.getO(gn) match {
        case Some(fc : FinalConstant) if fc.df.isDefined => Traverser(this,fc.df.get)
        case _ => t
      }
      case _ => Traverser(this,t)
    }
  }
  import Convenience._
  private def postProc(t : Term) : Term = t match {
    case forall(v,bd) or r =>
      val (w,sub) = Context.pickFresh(Context((bd.freeVars.filterNot(_ == v) ::: r.freeVars).map(VarDecl(_,None,None,None,None)):_*),v)
      forall(w,postProc(bd^?sub or r))
    case not(forall(v,bd)) or r =>
      val (w,sub) = Context.pickFresh(Context((bd.freeVars.filterNot(_ == v) ::: r.freeVars).map(VarDecl(_,None,None,None,None)):_*),v)
      not(forall(w,postProc(not(postProc(not(bd^?sub)) or r))))
    case r or forall(v,bd) =>
      postProc(forall(v,bd) or r)
    case r or not(forall(v,bd)) =>
      postProc(not(forall(v,bd)) or r)
    case not(not(a)) =>
      postProc(a)
    case ApplySpine(OMS(Eq.predpath),List(Lambda(p,_,ApplySpine(OMV(p2),a :: Nil)),Lambda(q,_,ApplySpine(OMV(q2),b :: Nil))))
      if p == p2 && q == q2 => Eq(a,b)
    case _ => t
  }

  private val postProcTrav = new StatelessTraverser {
    override def traverse(t: Term)(implicit con: Context, state: State): Term = postProc(Traverser(this,t))
  }

  private def getTerm (s : String,th : HashMap[String,Constant]) = th.get(s) match {
    case Some(c : Constant) => trav(c.toTerm,Context.empty)
    case _ =>
      throw new Exception(s + " not known")
  }

  private def toOMDocRec(expr : GFExpr, th : HashMap[String,Constant]) : Term = expr match {
    case GFStr(s) => getTerm(s,th)
    case GFA(fun,args) => ApplySpine(getTerm(fun,th),args.map(toOMDocRec(_,th)):_*)
  }

  def toOMDoc(gr : InstantiatedGrammar,expr : GFExpr) = postProcTrav(controller.simplifier.apply(
    toOMDocRec(expr,gr.asHashMap),
    gr.theory.getInnerContext
  ),Context.empty)

  def parse(lang : InstantiatedLanguage, s : String) = {
    val results = lang.parse(s)
    log("Parsing \"" + s + "\": " + results.length + " Results:")
    val ret = results.map(p => (toOMDoc(lang.grammar,p._1),p._2))
    ret.foreach(p => log(" - " + present(p._1) + " (" + p._2 + ")"))
    ret
  }
}

object MMTGF {
  val dpath = DPath(URI.http colon "mathhub.info") / "Teaching" / "LBS"
  val key = dpath ? "LogicSyntax" ? "correspondsTo"
}