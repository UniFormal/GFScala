package info.kwarc.gf


/*
object Test {

  def main(args: Array[String]): Unit = {
    val gr = Grammar("/home/jazzpirate/work/MathHub/Teaching/LBS/source/Frag1-syntactic")
    println(gr.categories)
    println(gr.languages)
    val en = gr.languages.head._2
    val parse = en.parse("the dog ran").head._1
    println(parse)
    println(en.linearize(parse))
  }

}

*/

import info.kwarc.mmt.api
import info.kwarc.mmt.api.{DPath, LocalName}
import info.kwarc.mmt.api.frontend.{Logger, Run}
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.objects.{OMSemiFormal, Term}
import info.kwarc.mmt.api.ontology.{DeclarationTreeExporter, DependencyGraphExporter, PathGraphExporter}
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.web.JSONBasedGraphServer
import info.kwarc.mmt.lf.{Apply, ApplySpine}

import scala.concurrent.Future

/** An abstract class for test methods. Instantiates a controller, sets the mathpath for archives,
  * loads the AlignmentsServer (so you can run a Server without getting an error message.
  *
  * You just need to give archivepath and instantiate the run method with your own arbitrary code
  *
  * @param archivepath    : the path to your archives
  * @param logprefixes    : A list of logprefixes to log
  * @param alignmentspath : the path to .align files (doesn't need to be set, therefore defaults to
  *                         empty string)
  * @param serverport     : Optional port to start a server. If None, no server is started
  * @param gotoshell      : if true, it will drop to the MMT shell afterwards
  * @param logfile        : If defined, will log into file
  */
abstract class Test(archivepath : String,
                    logprefixes : List[String] = Nil,
                    alignmentspath : String = "",
                    serverport : Option[Int] = None,
                    gotoshell : Boolean = true,
                    logfile : Option[String] = None) extends Logger {
  val controller = Run.controller
  def logPrefix = "user"
  def report = controller.report

  // If you want to log additional stuff, just put it in this list

  controller.handleLine("log console")
  if (logfile.isDefined) controller.handleLine("log html " + logfile.get)// /home/raupi/lmh/mmtlog.txt")
  ("test" :: logprefixes) foreach (s => controller.handleLine("log+ " + s))
  controller.handleLine("extension info.kwarc.mmt.lf.Plugin")
  controller.handleLine("extension info.kwarc.mmt.odk.Plugin")
  controller.handleLine("extension info.kwarc.mmt.pvs.Plugin")
  // controller.handleLine("extension info.kwarc.mmt.metamath.Plugin")
  controller.handleLine("mathpath archive " + archivepath)
  // controller.handleLine("extension info.kwarc.mmt.api.ontology.AlignmentsServer " + alignmentspath)


  def doFirst : Unit = {}

  def run : Unit

  /*
  def log(s : String) = {
    controller.report("user",s)
    controller.report.flush
  }
  */

  def main(args: Array[String]): Unit = try {

    controller.extman.addExtension(new DependencyGraphExporter)
    controller.extman.addExtension(new DeclarationTreeExporter)
    controller.extman.addExtension(new JSONBasedGraphServer)
    controller.extman.addExtension(new PathGraphExporter)
    doFirst
    if (serverport.isDefined) {
      //controller.handleLine("clear")
      controller.handleLine("server on " + serverport.get)
    }
    if (gotoshell) {
      Future {Run.main(Array())}(scala.concurrent.ExecutionContext.global)
      Thread.sleep(1000)
    }
    run
  } catch {
    case e: api.Error => println(e.toStringLong)
      sys.exit
  }

  def hl(s : String) = controller.handleLine(s)
}

/**
  * As an example, here's my default. All test files of mine just extend this:
  */
abstract class DennisTest(prefixes : String*) extends Test(
  "/home/jazzpirate/work/MathHub",
  prefixes.toList,
  "",
  Some(8080),
  true,
  Some("/home/jazzpirate/work/mmtlog.html")
) {
}

object GFMMT extends DennisTest("gf") {

  def present(tm : Term) = controller.presenter.asString(tm)

  lazy val gf = {
    val ret = new MMTGF
    controller.extman.addExtension(ret)
    ret
  }
  val mg = new ModelGenerator

  override def run: Unit = {

    val gr = gf.getGrammar(MMTGF.dpath ? "frag1log")

    val en = gr.languages("frag1SynEN")
    val utterance = en.parseMMT("Prudence is the lecturer and Bertie liked the lecturer .").head._1
    val query = en.parseMMT("Bertie liked Prudence .").head._1
    val query2 = en.parseMMT("Prudence liked Bertie .").head._1
    println("Utterance: " + present(utterance))
    println("Models:")
    mg.models(utterance).map(_.map(present)) foreach (s => println(" - " + s))
    println("Bertie liked Prudence: " + mg.prove(query,utterance).isEmpty)
    println("Prudence liked Bertie: Countermodels: " + mg.prove(query2,utterance).map(_.map(present).mkString("[",", ","]")).mkString(", "))
  }

}