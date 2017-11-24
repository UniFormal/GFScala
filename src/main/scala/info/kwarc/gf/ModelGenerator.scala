package info.kwarc.gf

import info.kwarc.mmt.api.GlobalName
import info.kwarc.mmt.api.objects.{OMS, Term}
import info.kwarc.mmt.lf.ApplySpine

import scala.collection.mutable

class ModelGenerator {
  import Convenience._



  def models(tm : Term*) : List[List[Term]] = ???
  def prove(query : Term, world : Term*) : List[List[Term]] = models(not(query) :: world.toList:_*)

}

object Convenience {
  implicit class orApply(tm : Term) {
    def or(tm2 : Term) = Convenience.or(tm,tm2)
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
  implicit class eq(tm : Term) {
    def ieq(tm2 : Term) = Convenience.eq.apply(tm,tm2)
  }
  object eq {
    val path = MMTGF.dpath ? "LogicSyntax" ? "eq"
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