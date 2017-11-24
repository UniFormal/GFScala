package info.kwarc.gf

import info.kwarc.gf.util._
import info.kwarc.mmt.api.utils.{File, ShellCommand}
import org.grammaticalframework.pgf.PGF

import scala.collection.immutable.HashMap

class Grammar(private[gf] val pgf : PGF) {
  def languages : HashMap[String,Language] =
    HashMap(asScala(pgf.getLanguages).toList.map(p => (p._1,ConcasScala(p._2,pgf))):_*)

  def categories : List[String] = pgf.getCategories

  override def equals(obj: scala.Any): Boolean = obj match {
    case that : Grammar => this.pgf == that.pgf
    case _ => false
  }
}

object Grammar {
  def apply(filenames : String*) : Grammar = {
    if (filenames.length == 1 && File(filenames.head).getExtension.contains("pgf"))
      return new Grammar(PGF.readPGF(filenames.head))
    val rets = filenames.map(makeString)
    val parent = rets.head._2.up
    val command = "gf" :: "-make" :: rets.flatMap(_._1).map(parent.relativize(_).toString).toList
    // println(parent + " : " + command.mkString(" "))
    val ret = ShellCommand.runIn(parent, command :_*)
    ret foreach println
    val ofile = parent / rets.head._1.head.setExtension("pgf").name
    val file = if (ofile.exists()) ofile else ofile.up.children.find(_.getExtension contains "pgf").getOrElse(throw ???)
    // println(file)
    new Grammar(PGF.readPGF(file.toString()))
  }


  private def makeString(s : String) : (List[File],File) = {
    val file = File(s)
    if (file.exists && file.children.isEmpty && file.getExtension.contains("gf"))
      (file :: Nil,file.getParentFile)
    else if (file.isDirectory) {
      (file.descendants.filter(_.getExtension.contains("gf")),file)
    } else throw ???
  }

}