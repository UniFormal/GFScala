package info.kwarc.gf

import info.kwarc.mmt.api.{GlobalName, LocalName}
import info.kwarc.mmt.api.objects.{OMS, Term}
import info.kwarc.mmt.lf.{Apply, ApplySpine, Lambda}

import scala.collection.mutable

case class Model(individuals : List[Term], relations : List[Term]) {
  override def toString = "Individuals: " + individuals.mkString(", ") + "\n   " + "   Relations: " + relations.mkString(", ")
  def present(pr : Term => String) =
    "Individuals: " + individuals.map(pr).mkString(", ") + "\n   " + "   Relations: " + relations.map(pr).mkString(", ")
}

class ModelGenerator {
  import Convenience._

  private case class Column(formulas : List[(Term,Boolean)]) {
    def consistent = {
      val newlist = (formulas.flatMap{
        case (Pred1(p,a),sgn) => inds(a).map(i => (Pred1(p,i),sgn)).toList
        case (Pred2(p,a,b),sgn) => inds(a).flatMap(ia => inds(b).map(ib => (Pred2(p,ia,ib),sgn))).toList
        case (a Eq b,sgn) => inds(a).flatMap(ia => inds(b).map(ib => (ia eq ib,sgn))).toList
        case _ => Nil
      } ::: individuals.map(i => (i eq i,true))).distinct
      !newlist.exists(p => newlist.exists(q => p._1 == q._1 && p._2 != q._2))
    }
    // TODO check for all possible substitutions
    def toModel : Model = Model(individuals,formulas.collect{
      case (Pred1(p,a),true) => Pred1(p,a)
      case (Pred1(p,a),false) => not(Pred1(p,a))
      case (Pred2(p,a,b),true) => Pred2(p,a,b)
      case (Pred2(p,a,b),false) => not(Pred2(p,a,b))
      case (a Eq b,true) =>  Eq(a,b)
    })
    def replace(f : Term, s : Boolean, by : List[(Term,Boolean)]) = Column(formulas.filterNot(_ == (f,s)) ::: by)

    lazy val individuals = formulas.flatMap {
      case (Pred1(_,a),_) => List(a)
      case (Pred2(_,a,b),_) => List(a,b)
      case (a Eq b,_) => List(a,b)
      case _ => Nil
    }
    private lazy val inds = {
      val map = mutable.HashMap[Term,Set[Term]]()
      individuals foreach (i => map(i) = Set(i))
      formulas foreach {
        case (a Eq b, true) =>
          val is = map(a) ++ map(b)
          is foreach (i => map(i) = is)
        case _ =>
      }
      map
    }
    private def isAtomic(tm : (Term,Boolean)) = tm match {
      case (Pred1(_,a),_) => true
      case (Pred2(_,a,b),_) => true
      case (a Eq b,_) => true
      case _ => false
    }
  }

  private def tableaux(stack : Column) : List[Model] = {
    if (!stack.consistent) return Nil
    val next = stack.formulas.find {
      case ((_ or _),_) => true
      case ((not(_)),_) => true
      case _ => false
    }
    next match {
      case Some((not(a),sgn)) =>
        tableaux(stack.replace(not(a),sgn,List((a,!sgn))))
      case Some((a or b,false)) =>
        tableaux(stack.replace(a or b,false,List((a,false),(b,false))))
      case Some((a or b,true)) =>
        tableaux(stack.replace(a or b,true,List((a,true)))) :::
          tableaux(stack.replace(a or b,true,List((b,true))))
      case None => List(stack.toModel)
    }
  }

  def models(tm : Term*) : List[Model] = tableaux(Column(tm.map((_,true)).toList))
  def prove(query : Term, world : Term*) : List[Model] = models(not(query) :: world.toList:_*)

}

object Convenience {
  implicit class orApply(tm : Term) {
    def or(tm2 : Term) = Convenience.or(tm,tm2)
  }
  object forall {
    val path = MMTGF.dpath ? "LogicSyntax" ? "forall"
    val indpath = MMTGF.dpath ? "LogicSyntax" ? "ind"
    def unapply(tm : Term) = tm match {
      case ApplySpine(OMS(`path`),Lambda(x,_,bd) :: Nil) =>
        Some((x,bd))
      case _ => None
    }
    def apply(v : LocalName, bd : Term) = Apply(OMS(path),Lambda(v,OMS(indpath),bd))
  }
  object or {
    val path = MMTGF.dpath ? "LogicSyntax" ? "or"
    def unapply(tm : Term) = tm match {
      case ApplySpine(OMS(`path`),List(tm1,tm2)) => Some((tm1,tm2))
      case _ => None
    }
    def apply(tm1 : Term, tm2 : Term) = ApplySpine(OMS(path),tm1,tm2)
  }
  object not {
    val path = MMTGF.dpath ? "LogicSyntax" ? "negation"
    def unapply(tm : Term) = tm match {
      case ApplySpine(OMS(`path`),List(tm1)) => Some(tm1)
      case _ => None
    }
    def apply(tm1 : Term) = ApplySpine(OMS(path),tm1)
  }
  implicit class Eq(tm : Term) {
    def ieq(tm2 : Term) = Convenience.Eq.apply(tm,tm2)
  }
  object Eq {
    val predpath = MMTGF.dpath ? "LogicSyntax" ? "npeq"
    val path = MMTGF.dpath ? "LogicSyntax" ? "ieq"
    def unapply(tm : Term) = tm match {
      case ApplySpine(OMS(`path`),List(tm1,tm2)) => Some((tm1,tm2))
      case _ => None
    }
    def apply(tm1 : Term, tm2 : Term) = ApplySpine(OMS(path),tm1,tm2)
  }
  object the {
    val path = MMTGF.dpath ? "LogicSyntax" ? "that"
    def unapply(tm : Term) = tm match {
      case ApplySpine(OMS(`path`),List(tm1)) => Some(tm1)
      case _ => None
    }
    def apply(tm1 : Term) = ApplySpine(OMS(path),tm1)
  }
  object Pred1 {
    val path = MMTGF.dpath ? "LogicSyntax" ? "that"
    def unapply(tm : Term) = tm match {
      case ApplySpine(OMS(p),List(tm1)) if p.module != path.module => Some((p,tm1))
      case _ => None
    }
    def apply(p : GlobalName, tm1 : Term) = ApplySpine(OMS(p),tm1)
  }
  object Pred2 {
    val path = MMTGF.dpath ? "LogicSyntax" ? "that"
    def unapply(tm : Term) = tm match {
      case ApplySpine(OMS(p),List(tm1,tm2)) if p.module != path.module => Some((p,tm1,tm2))
      case _ => None
    }
    def apply(p : GlobalName, tm1 : Term, tm2 : Term) = ApplySpine(OMS(p),tm1,tm2)
  }

}