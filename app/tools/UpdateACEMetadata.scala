package cropsitedb.tools

import play.api.db.DB
import anorm._

import cropsitedb.helpers.{AnormHelper, GeoHashHelper, CropsiteDBConfig}
import java.io.{File,IOException}
import java.nio.file.{Files,FileVisitor,FileVisitResult,Path,Paths}
import java.nio.file.attribute.BasicFileAttributes
import org.agmip.ace.{AceDataset,AceExperiment}
import org.agmip.ace.io.AceParser
import org.agmip.ace.util._
import org.agmip.tools.AgmipFileIdentifier
import scala.collection.JavaConversions._

import play.core.StaticApplication
import play.api.Play.current

object UpdateACEMetadata extends App {
  new StaticApplication(new java.io.File("."))
  Console.println("Working on it")

  DB.withTransaction { implicit c =>
    val datasetIds = SQL("SELECT title,dsid from ace_datasets WHERE frozen={frozen}").on('frozen -> false)().map(r =>
      (r[String]("dsid"),r[String]("title"))).toList
    datasetIds.foreach { case (dsid:String, title:String) =>
      Console.println("Applying changes to " + title + " [" + dsid + "]")
      val path = dsPath(dsid)
      if (Files.isReadable(path) && Files.isDirectory(path)) {
        Files.walkFileTree(path, new FileVisitor[Path] {
          def visitFileFailed(file: Path, ex: IOException) = FileVisitResult.CONTINUE
          def visitFile(file: Path, attrs: BasicFileAttributes) = {
            AgmipFileIdentifier(file.toFile) match {
              case "ACE" => {
                aceHandler(dsid, file)
              }
              case _ => {}
            }
            FileVisitResult.CONTINUE
          }
          def preVisitDirectory(dir: Path, attrs: BasicFileAttributes) = FileVisitResult.CONTINUE
          def postVisitDirectory(dir: Path, ex: IOException) = FileVisitResult.CONTINUE
        })
      }
    }
  }
  play.api.Play.stop()

  private def dsPath(dsid: String): Path = {
    Paths.get(CropsiteDBConfig.localFileStore, "uploads", dsid)
      .toAbsolutePath.normalize
  }

  private def aceHandler(dsid: String, file: Path) {
    Console.println("Processing " + file.toString)
    val dataset = AceParser.parseACEB(file.toFile)
    updateFertilizerAndIrrig(dsid, dataset.getExperiments.toList)
  }

  private def updateFertilizerAndIrrig(dsid: String, experiments: List[AceExperiment]) {
    experiments.foreach { ex =>
      val id = ex.getId()
      val fert = Option(AceFunctions.deepGetValue(ex, "fecd")) match {
        case None => "N"
        case Some(_) => "Y"
      }
      val irrig = Option(AceFunctions.deepGetValue(ex, "irop")) match {
        case None => "N"
        case Some(_) => "Y"
      }
      DB.withTransaction { implicit c =>
      SQL("UPDATE ace_metadata SET fertilizer={fert}, irrig={irrig} WHERE eid={id} AND dsid={dsid}")
        .on('fert -> fert, 'irrig -> irrig, 'id -> id, 'dsid -> dsid).executeUpdate()
      }
    }
  }
}
