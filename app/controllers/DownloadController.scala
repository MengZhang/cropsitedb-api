package cropsitedb.controllers

import play.api._
import play.api.mvc._
import play.api.db.DB
import play.api.libs.iteratee.Enumerator
import play.api.libs.json._
import play.api.libs.functional.syntax._
import anorm._
import anorm.SqlParser._

import java.io.{File, FileWriter, IOException}
import java.nio.file.{ Path, Paths, Files, FileSystems, FileSystem, StandardCopyOption, FileVisitor, FileVisitResult }
import java.nio.file.attribute.{ PosixFilePermission, PosixFilePermissions, BasicFileAttributes }
import java.net.URI
import java.util.Date
import java.util.zip.GZIPOutputStream;
import java.text.SimpleDateFormat
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

import cropsitedb.helpers._

import org.agmip.ace.AceDataset
import org.agmip.ace.AceExperiment
import org.agmip.ace.AceWeather
import org.agmip.ace.AceSoil
import org.agmip.ace.io._
import org.agmip.ace.util._
import org.agmip.ace.lookup.LookupCodes
import org.agmip.acmo.util._

import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.Logger

object DownloadController extends Controller {
  val baseDir = CropsiteDBConfig.localFileStore
  def tmpDir = Paths.get(baseDir, "tmp")

  def download = Action.async(parse.json) { implicit request =>
    Files.createDirectories(tmpDir)
    val dlReqRes = request.body.validate[DownloadHelper.DownloadRequest]
    dlReqRes.fold(
      errors => {
        Future { BadRequest(Json.obj("error" -> "Invalid download request")) }
      },
      dlReq => {
        val dlReqId  = java.util.UUID.randomUUID.toString
        val fileTypes = fileTypeBuilder(dlReq.fileTypes)
        val dlPath = Paths.get(baseDir, "downloads",  dlReqId+".zip")
        Files.createDirectories(dlPath)
        withZipFilesystem(dlPath) { dl =>
          dlReq.downloads.foreach { dataset =>
            val destPath = dl.getPath(fetchCleanDSName(dataset.dsid))
            if (fileTypes.contains("ACEB")) {
              Logger.debug("RUNNING ACE DOWNLOAD PROCESSING")
              //ACEB Processing
              val acebPath   = destPath.resolve("ACE_dataset.aceb")
              val sourcePath = Paths.get(baseDir, "uploads", dataset.dsid, "ACE_dataset.aceb")
              buildACEB(sourcePath, acebPath, tmpDir.resolve(dlReqId), dataset)
            }
            if (fileTypes.contains("ACMO")) {
              Logger.debug("RUNNING ACMO DOWNLOAD PROCESSING")
              // Gather ACMO information
              val acmoPath   = destPath.resolve("acmo.csv")
              val tmpFile    = Files.createTempFile(tmpDir, "acmo", ".tmp")
              Files.createDirectories(acmoPath.getParent)
              val acmoWriter = new FileWriter(tmpFile.toFile)
              acmoWriter.write(AcmoUtil.generateAcmoHeader)
              DB.withTransaction { implicit c =>
                val variables = AcmoUtil.generateAcmoHeader.split("\n").last.drop(2).toLowerCase.replace("#", "NUM")
                val baseQuery = s"SELECT $variables FROM acmo_metadata WHERE dataset_id={dsid}";
                val ordering = " ORDER BY cmss, crop_model, exname"
                val acmos = dataset.eids match {
                  case None => SQL(baseQuery+ordering).on('dsid -> dataset.dsid)
                  case Some(ids) => SQL(baseQuery + " AND eid IN ({eids})"+ordering).on('dsid -> dataset.dsid, 'eids -> ids)
                }
                acmos.apply().foreach(l => {
                  val line = l.asList.map(entry => entry.asInstanceOf[Option[String]]).map(entry => entry.getOrElse(""))
                  acmoWriter.write("\"*\",\"" + line.mkString("\",\"") + "\"\n")
                })
              }
              acmoWriter.close
              Files.move(tmpFile, acmoPath)
            }
              // Get any necessary domes (if requested)
            if (fileTypes.contains("DOME") && fileTypes.contains("ACEB")) {
              val sourcePath = Paths.get(baseDir, "uploads", dataset.dsid, "alldomes.dome")
              val domePath = destPath.resolve("alldomes.dome")
              sourcePath.toFile.exists match {
                case false => Logger.debug("DOME FILE NOT FOUND: " + sourcePath)
                case true => Files.copy(sourcePath, domePath)
              }
              // Check for ALINK information
            }
            makeMetadataFile(destPath, dataset.dsid, dataset.eids)
          }
        }
        val destUrl = routes.DownloadController.serve(dlReqId).absoluteURL(true)
        Future { Ok(Json.obj("url" -> destUrl)) }
      })
  }

  def serveDataset(dsid: String) = Action { implicit request =>
    // Check to see if the zip file exists in the downloads/datasets directory
    val destFile  = Paths.get(baseDir, "downloads", "datasets", dsid+".zip")
    val sourceDir = Paths.get(baseDir, "uploads", dsid)
    Files.exists(sourceDir) match {
      case false => BadRequest(Json.obj("error" -> "Invalid dataset id"))
      case true  => {
        Files.exists(destFile) match {
          case true => {
            Ok.sendFile(
              content = destFile.toAbsolutePath.toFile,
              fileName = _ => "agmip_download.zip"
            )
          }
          case false => {
            Files.createDirectories(destFile.getParent)
            withZipFilesystem(destFile) { ds =>
              val destPath = ds.getPath(fetchCleanDSName(dsid))
              Files.walkFileTree(sourceDir, new FileVisitor[Path] {
                def visitFileFailed(file: Path, ex: IOException) = FileVisitResult.CONTINUE
                def visitFile(file: Path, attrs: BasicFileAttributes) = {
                  val destDir = destPath.resolve(sourceDir.relativize(file.getParent).toString)
                  Files.copy(file, destDir.resolve(file.getFileName.toString))
                  FileVisitResult.CONTINUE
                }
                def preVisitDirectory(dir: Path, attrs: BasicFileAttributes) = {
                  val relPath =  sourceDir.relativize(dir)
                  Files.createDirectories(destPath.resolve(relPath.toString))
                  FileVisitResult.CONTINUE
                }
                def postVisitDirectory(dir: Path, ex: IOException) = FileVisitResult.CONTINUE
              })
              makeMetadataFile(destPath, dsid, None)
            }
            Ok.sendFile(
              content = destFile.toAbsolutePath.toFile,
              fileName = _ => "agmip_download.zip"
            )
          }
        }
      }
    }
  }

  def serve(dlid: String) = Action { implicit request =>
    // By default, we should be downloading the .ZIP files
    val download = Paths.get(baseDir, "downloads", dlid+".zip")
    if (Files.isReadable(download.toAbsolutePath)) {
      Ok.sendFile(
        content = download.toAbsolutePath.toFile,
        fileName = _ => "agmip_download.zip"
      )
    } else {
      BadRequest("Missing file")
    }
  }

  def fileTypeBuilder(ft: Int): List[String] = {
    fileTypeBuilderL(ft, 1, List())
  }

  def fileTypeBuilderL(ft: Int, test: Int, l: List[String]): List[String] = {
    test match {
      case 1 =>
        if ((ft & test) != 0) fileTypeBuilderL(ft, 2, l :+ "ACEB") else fileTypeBuilderL(ft, 2, l)
      case 2 =>
        if ((ft & test) != 0) fileTypeBuilderL(ft, 4, l :+ "DOME") else fileTypeBuilderL(ft, 4, l)
      case 4 =>
        if ((ft & test) != 0) l :+ "ACMO" else l
      case _ =>
        l
    }
  }

  private def buildACEB(source: Path, dest: Path, tmp: Path, data: DownloadHelper.DSIDRequest):Option[String] = {
    Files.isReadable(source) match {
      case false => { Some("Missing the source file: "+source) }
      case true  => {
        Files.exists(dest) match {
          case true  => Some("Download file conflict. Please try again")
          case false => {
            val issues: Path = Files.createDirectories(dest.getParent)
            Files.createDirectories(tmp)
            var sids:Set[String] = Set()
            var wids:Set[String] = Set()
            val destDS = new AceDataset()
            val sourceDS = AceParser.parseACEB(source.toAbsolutePath.toFile)
            data.eids.foreach(println)
            sourceDS.getExperiments.toList.foreach { ex =>
              val eid = ex.getId(false)
              if (data.eids.getOrElse(List()).contains(eid)) {
                destDS.addExperiment(ex.rebuildComponent())
                sids = sids + ex.getValueOr("sid", "INVALID")
                wids = wids + ex.getValueOr("wid", "INVALID")
              }
            }
            sourceDS.getSoils.toList.foreach { s =>
              if (sids.contains(s.getId)) {
                destDS.addSoil(s.rebuildComponent)
              }
            }
            sourceDS.getWeathers.toList.foreach { w =>
              if (wids.contains(w.getId(false))) {
                destDS.addWeather(w.rebuildComponent)
              }
            }
            destDS.linkDataset
            DB.withTransaction { implicit c =>
              destDS.getExperiments.toList.foreach { ex =>
                SQL("UPDATE ace_metadata SET download_count = download_count + 1 WHERE dsid={dsid} AND eid={eid}")
                  .on("dsid"->data.dsid, "eid"->ex.getId(true))
                  .executeUpdate()
              }
            }
            val tmpFile = Files.createTempFile(tmp, data.dsid, ".tmp").toAbsolutePath
            AceGenerator.generateACEB(tmpFile.toFile, destDS)
            Files.move(tmpFile, dest)
            None
          }
        }
      }
    }
  }

  private def buildACMOFiles() = {
  }

  private def makeMetadataFile(destDir: Path, dsid: String, eids: Option[Seq[String]]) = {
    val mdf = MetadataFilter.INSTANCE
    val mdPath    = destDir.resolve("metadata.csv")
    val mdTmpFile = Files.createTempFile(tmpDir, "metadata", ".csv")
    val mdWriter = new FileWriter(mdTmpFile.toFile)
    val descLine: ListBuffer[String] = new ListBuffer()
    val headLine: ListBuffer[String] = new ListBuffer()
    mdf.getExportMetadata().foreach { col =>
      descLine += (mdf.getDescriptions().apply(col))
      headLine += (col)
    }
    mdWriter.write("\""+(descLine.mkString("\",\""))+"\"\n")
    mdWriter.write("\""+(headLine.mkString("\",\""))+"\"\n")
    DB.withTransaction { implicit c =>
      val query = eids match {
        case None => SQL("SELECT * FROM ace_metadata WHERE dsid={dsid}").on('dsid -> dsid)
        case Some(ids) => SQL("SELECT * FROM ace_metadata WHERE dsid={dsid} AND eid in ({eids})").
        on('dsid -> dsid, 'eids -> ids)
      }
      query.apply().foreach {
        line => {
          val dataLine: ListBuffer[String] = new ListBuffer()
          // Now do something with each row of data.
          mdf.getExportMetadata().foreach { col =>
            col.toUpperCase match {
              case "PDATE"|"HDATE" => {
                val d: String = line[Option[Date]](col) match {
                  case Some(dt) => AnormHelper.df2.format(dt)
                  case None => ""
                }
                dataLine += (d)
              }
                  case _ => dataLine += (line[Option[String]](col)).getOrElse("")
            }
          }
          mdWriter.write("\""+(dataLine.mkString("\",\""))+"\"\n")
        }
      }
    }
    mdWriter.close
    Files.move(mdTmpFile, mdPath)
  }

  private def fetchCleanDSName(id: String):String = {
    DB.withConnection { implicit c =>
      try {
        SQL("SELECT title FROM ace_datasets WHERE dsid = {dsid}").on("dsid"->id).as(scalar[Option[String]].single).getOrElse(id)
      } catch {
        case _ : Throwable => { id }
      }
    }
  }

  private def withZipFilesystem(zipFile: Path, overwrite: Boolean = true)(f: FileSystem => Unit) {
    if (overwrite) Files deleteIfExists zipFile
    val env = Map("create" -> "true").asJava
  val uri = new URI("jar", zipFile.toUri().toString(), null)

  val system = FileSystems.newFileSystem(uri, env)
  try {
    f(system)
    } finally {
      system.close()
    }
  }
}
