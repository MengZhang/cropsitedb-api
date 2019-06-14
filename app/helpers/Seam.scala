package cropsitedb.helpers

import java.io.File
import java.io.FileInputStream
import java.io.IOException
import java.nio.file.Files
import java.nio.file.FileVisitor
import java.nio.file.FileVisitResult
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.Path
import java.util.zip.GZIPInputStream

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

import com.fasterxml.jackson.core.{JsonFactory, JsonParser, JsonToken}

import org.agmip.ace._
import org.agmip.ace.io._
import org.agmip.ace.util._

object Seamer {
  var eids: ListBuffer[String] = ListBuffer()
  var wids: ListBuffer[String] = ListBuffer()
  var sids: ListBuffer[String] = ListBuffer()
  var dids: ListBuffer[String] = ListBuffer()

  var ace: Int   = 0
  var dome: Int  = 0
  var acmo: Int  = 0
  var alink: Int = 0
  
  def main(args: Array[String]) {
    val dir = new File(args(0))

    Files.walkFileTree(dir.toPath, new FileVisitor[Path] {
      def visitFileFailed(file: Path, ex: IOException) = FileVisitResult.CONTINUE
      def visitFile(file: Path, attrs: BasicFileAttributes) = {
        val fileType = AgmipFileIdentifier(file.toFile)
        fileType match {
          case "ACE" => {
            ace = ace + 1
            extractAceIds(file.toFile)
          }
          case "DOME" => {
            dome = dome + 1
            extractDomeIds(file.toFile)
          }
          case "ACMO" => {
            acmo = acmo + 1
          }
          case "ALINK" => {
            alink = alink + 1
          }
          case _ => {}
        }
        FileVisitResult.CONTINUE
      }
      def preVisitDirectory(dir: Path, attrs: BasicFileAttributes) = FileVisitResult.CONTINUE
      def postVisitDirectory(dir: Path, ex: IOException) = FileVisitResult.CONTINUE
    })

    println(s"Found $ace ACE files")
    println(s"Found $dome DOME files")
    println(s"Found $acmo ACMO files")
    println(s"Found $alink ALINK files")

    val eidNum = eids.size()
    val sidNum = sids.size()
    val widNum = wids.size()
    val domeNum = dids.size()

    println(s"Number of experiments found: $eidNum")
    println(s"Number of soils found: $sidNum")
    println(s"Number of weathers found: $widNum")
    println(s"Number of domes found: $domeNum")
  }

  def extractAceIds(file: File) = {
    val ds: AceDataset = AceParser.parseACEB(file)
    val e = ds.getExperiments.toList
    val s = ds.getSoils.toList
    val w = ds.getWeathers.toList

    mergeIds(e.map(x => x.getId(false)), eids)
    mergeIds(s.map(x => x.getId(false)), sids)
    mergeIds(w.map(x => x.getId(false)), wids)


  }

  def mergeIds(source: List[String], dest: ListBuffer[String]) {
    source.foreach { elem =>
      dest.contains(elem) match {
        case true => {}
        case false => dest += elem
      }
    }
  }

  def extractDomeIds(file: File) = {
    val fis = new FileInputStream(file)
    val gis = new GZIPInputStream(fis)
    val jp = new JsonFactory().createParser(gis)

    var domes:ListBuffer[String] = ListBuffer()

    try {
      var level: Boolean = false
      var currentDome: String = ""
      // Seek to info
      
      while(Option(jp.nextToken()).isDefined) {
        jp.getCurrentToken match {
          case JsonToken.START_OBJECT => {
            level match {
              case true => {
                jp.skipChildren
                level = false
              }
              case false => {
                jp.nextToken
                currentDome = jp.getCurrentName
                domes = domes :+ currentDome
                level = true
              }
            }
          }
          case JsonToken.FIELD_NAME => {
            level match {
              case true => {}
              case false => {
                domes = domes :+ jp.getCurrentName
                level = true
              }
            }
          }
          case JsonToken.END_OBJECT => {
            level = false
          }
          case _ => {}
        }
      }
    } catch {
      case _ : Throwable => {
        val t:List[List[String]]  = List(List())
      }
    } finally {
      jp.close
      gis.close
      fis.close
    }
    println(s"$domes")
    mergeIds(domes.toList, dids)
  }
}
