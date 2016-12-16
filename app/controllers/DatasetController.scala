package cropsitedb.controllers

import akka.actor.ActorSelection

import java.io.IOException
import java.io.File

import java.nio.file.FileAlreadyExistsException
import java.nio.file.Files
import java.nio.file.FileSystems
import java.nio.file.FileVisitor
import java.nio.file.FileVisitResult
import java.nio.file.{Path, Paths}
import java.nio.file.attribute.BasicFileAttributes

import scala.concurrent.Future

import play.api._
import play.api.mvc._

import play.api.db.DB
import anorm._
import cropsitedb.actors.{Messages, ProcessDOME}
import cropsitedb.helpers.{AnormHelper, CropsiteDBConfig}

import play.api.libs.concurrent.Akka
import play.api.libs.json._
import play.api.libs.functional.syntax._

import cropsitedb.helpers.DatasetHelper

import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import play.api.Logger

import com.google.common.io.{Files => GFiles}
import org.agmip.tools.{Seamer, AgmipFileIdentifier}

object DatasetController extends Controller {
  val baseDir = CropsiteDBConfig.localFileStore
  def tmpDir = Paths.get(baseDir, "tmp")

  def index = Action {
    Ok(Json.obj())
  }

  /*
   * Create Dataset (/dataset/create)
   * Create a directory for this dataset to store files and add a reference to the database.
   */
  def  createDataset = Action(parse.json) { request =>
    val dscRequest = request.body.validate[DatasetHelper.CreateDatasetRequest]
    dscRequest.fold(
      errors => {
        BadRequest(Json.obj("error"->"Missing fields for dataset creation"))
      },
      dscReq => {
        val dsid = java.util.UUID.randomUUID().toString
        val destDir = dsPath(dsid, dscReq.freeze)
        Files.createDirectories(destDir)
        DB.withTransaction { implicit c =>
          SQL("INSERT INTO ace_datasets(dsid, title, email, frozen) VALUES ({d}, {t}, {e}, {f})").on("d"->dsid, "t"->dscReq.title, "e"->dscReq.email.toLowerCase, "f"->dscReq.freeze.getOrElse(false)).execute()
        }
        Ok(Json.obj("dsid"->dsid, "title"->dscReq.title))
      }
      )
  }

  def addToDataset(dsid: String) = Action(parse.multipartFormData) { implicit request =>
    request.body.file("file").map { f =>
      val fileName    = f.filename
      //val contentType = Files.probeContentType(f.ref.file.toPath)
      val contentType = AgmipFileIdentifier(f.ref.file)
      Logger.debug("Uploading "+fileName+" - "+contentType)
      DB.withTransaction { implicit c =>
        var copyFail: Option[String] = None
        SQL("SELECT * FROM ace_datasets WHERE dsid={d}").on("d"->dsid).apply()
          .map { r =>
            val p = dsPath(r[String]("dsid"), Option(r[Boolean]("frozen")))
            val dest = p.resolve(fileName)
            try {
              Files.move(f.ref.file.toPath, dest)
            } catch {
              case faeEx: FileAlreadyExistsException => {
                Logger.debug(s"File already exists! $dest")
                copyFail = Some(s"$fileName already exists, please rename")
              }
            }
          }
          copyFail match {
            case None => Ok(Json.obj("filetype"->contentType))
            case Some(x: String) => BadRequest(Json.obj("error"->x))
          }
      }
      }.getOrElse {
        BadRequest(Json.obj("error"->"No file uploaded"))
      }
  }

  def deleteFromDataset(dsid: String) = Action(parse.json) { implicit request =>
    val jsonReq = request.body.validate[DatasetHelper.DeleteFromDatasetRequest]
    jsonReq.fold(
      errors => {
        BadRequest(Json.obj("error"->"Missing fields for removing file from dataset"))
      },
      req => {
        DB.withTransaction { implicit c =>
          SQL("SELECT * FROM ace_datasets WHERE dsid={d} AND email={e}")
            .on("d"->dsid, "e"->req.email.toLowerCase)
            .apply().map { r =>
              val p = dsPath(r[String]("dsid"), Option(r[Boolean]("frozen")))
              val f = p.resolve(req.file)
              Files.deleteIfExists(f)
            }
            Ok(Json.obj())
        }
      }
      )
  }

  def deleteDataset = Action(parse.json) { implicit request =>
    val dsdRequest = request.body.validate[DatasetHelper.DeleteDatasetRequest]
    dsdRequest.fold(
      errors => {
        BadRequest(Json.obj("error"->"Missing fields for dataset deletion"))
      },
      dsdReq => {
        DB.withTransaction { implicit c =>
          Logger.info("Checking the SQL")
          SQL("SELECT * FROM ace_datasets WHERE dsid={d} AND email={e}")
            .on("d"->dsdReq.dsid, "e"->dsdReq.email.toLowerCase)
            .apply().map{ r =>
              Logger.info("Attempting to delete..."+dsdReq.dsid)
              val d = r[String]("dsid")
              val p = dsPath(d, Option(r[Boolean]("frozen")))
              // First remove the SQL references
              SQL("DELETE FROM ace_metadata WHERE dsid={d}")
                .on("d"->d)
                .execute()
              SQL("DELETE FROM acmo_metadata WHERE dsid={d}")
                .on("d"->d)
                .execute()
              SQL("DELETE FROM alink_metadata WHERE dsid={d}")
                .on("d"->d)
                .execute()
              SQL("DELETE FROM dome_metadata WHERE dsid={d}")
                .on("d"->d)
                .execute()
              Logger.info("Deleting "+p+" from all")
              deleteDSFiles(p)
            }
            SQL("DELETE FROM ace_datasets WHERE dsid={d} AND email={e}")
              .on("d"->dsdReq.dsid, "e"->dsdReq.email.toLowerCase)
              .execute()
            Ok(Json.obj("deleted" -> dsdReq.dsid))
        }
      }
      )
  }

  def finalizeDataset(dsid: String) = Action(parse.json) { implicit request =>
    val fdsRequest = request.body.validate[DatasetHelper.FinalizeDatasetRequest]
    fdsRequest.fold(
      errors => {
        BadRequest(Json.obj("error"->"Missing fields for dataset finalization"))
      },
      req => {
        DB.withTransaction { implicit c =>
          SQL("SELECT * FROM ace_datasets WHERE dsid={d} AND email={e}")
            .on("d"->dsid, "e"->req.email.toLowerCase)
            .apply().map { r =>
              val frozen = Option(r[Boolean]("frozen"))
              val dst = dsPath(r[String]("dsid"), frozen)
              frozen match {
                case None => {}
                case Some(true) => {}
                case Some(false) => {
                  Files.walkFileTree(dst, new FileVisitor[Path] {
                    def visitFileFailed(file: Path, ex: IOException) = FileVisitResult.CONTINUE
                    def visitFile(file: Path, attrs: BasicFileAttributes) = {
                      val fileType = AgmipFileIdentifier(file.toFile)
                      val proc:Option[ActorSelection] = fileType match {
                        case "ACE" => {
                          Logger.debug("RUNNING ACE")
                          Some(Akka.system.actorSelection("akka://application/user/process-aceb"))
                        }
                        case "DOME" => {
                          Logger.debug("RUNNING DOME")
                          Some(Akka.system.actorSelection("akka://application/user/process-dome"))
                        }
                        case "ACMO" => {
                          Logger.debug("RUNNING ACMO")
                          Some(Akka.system.actorSelection("akka://application/user/process-acmo"))
                        }
                        case "ALINK" => {
                          Logger.debug("RUNNING ALINK")
                          Some(Akka.system.actorSelection("akka://application/user/process-alnk"))
                        }
                        case _ => {None}
                      }
                      proc match {
                        case Some(exec) => exec ! Messages.ProcessFile(dsid, file.toAbsolutePath.toString)
                        case None => {}
                      }
                      FileVisitResult.CONTINUE
                    }
                    def preVisitDirectory(dir: Path, attrs: BasicFileAttributes) = FileVisitResult.CONTINUE
                    def postVisitDirectory(dir: Path, ex: IOException) = FileVisitResult.CONTINUE
                  })
                }
              }
            }
            Ok(Json.obj())
        }
      }
      )
  }

  def dsPath(dsid: String, frozen: Option[Boolean]): Path  = {
    val base = CropsiteDBConfig.localFileStore
    val dest = if(! (frozen.getOrElse(false))) "uploads" else "freezer"
    FileSystems.getDefault().getPath(base, dest, dsid).toAbsolutePath.normalize
  }

  def deleteDSFiles(path: Path) {
    if (Files.exists(path) && Files.isDirectory(path)) {
      Files.walkFileTree(path, new FileVisitor[Path] {
        def visitFileFailed(file: Path, exc: IOException) = FileVisitResult.CONTINUE

        def visitFile(file: Path, attrs: BasicFileAttributes) = {
          Logger.info("Deleteing "+file)
          Files.delete(file)
          FileVisitResult.CONTINUE
        }

        def preVisitDirectory(dir: Path, attrs: BasicFileAttributes) = FileVisitResult.CONTINUE

        def postVisitDirectory(dir: Path, exc: IOException) = {
          Logger.info("Deleting "+dir)
          Files.delete(dir)
          FileVisitResult.CONTINUE
        }
      })
    }
  }
}
