package cropsitedb.actors

import akka.actor.{Actor, Props, ActorLogging}
import akka.event.Logging

import java.io.File
import java.io.FileReader

import au.com.bytecode.opencsv.CSVReader

import play.api.db.DB
import anorm._
import cropsitedb.helpers.AnormHelper

import play.api.Play.current

class ProcessACMO extends Actor with ActorLogging {
  def receive = {
    case msg: Messages.ProcessFile => processing(msg)
    case _ => {}
  }

  def processing(msg: Messages.ProcessFile) = {
    val acmoFile = new File(msg.filePath)
    if (acmoFile.exists) {
      processACMO(msg.dsid, acmoFile)
    } else {
      log.error("File not found")
    }
  }

  def processACMO(dsid: String, f: File) {

    val r = new FileReader(f)
    val c = new CSVReader(r)
    var l = c.readNext
    var h:List[String] = List()
    var acc:List[List[Tuple2[String,String]]] = List()

    val filecmss = f.getName().split("-").reverse.drop(1).take(1).apply(0)
    val cmss = filecmss.take(2) match {
      case "CM" => filecmss
      case _ => ""
    }

    while (Option(l).isDefined) {
      val line = l.toList
      line.head match {
        case "#" => h = line.tail.map(_.toLowerCase).map(_.replace("#","NUM"))
        case "*" => {
          acc = acc :+ h.zip(line.tail)
        }
        case _ => {} // Do Nothing
      }
      l = c.readNext
    }

    DB.withTransaction { implicit c =>
      acc.foreach { merged =>
        val entry = ("dataset_id", dsid) :: ("cmss", cmss) :: merged
        SQL("INSERT INTO acmo_metadata ("+AnormHelper.varJoin(entry)+") VALUES ("+AnormHelper.valJoin(entry)+")").on(entry.map(AnormHelper.agmipToNamedParam(_)):_*).execute()
      }
    }
    c.close
    r.close
  }
}
