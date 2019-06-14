package cropsitedb.helpers

import java.io.File
import java.io.FileInputStream
import java.io.IOException
import java.nio.file.Files
import java.util.zip.GZIPInputStream

import scala.io.Source

import au.com.bytecode.opencsv.CSVReader

import com.fasterxml.jackson.core.JsonFactory
import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.core.JsonParseException
import com.fasterxml.jackson.core.JsonToken

import org.apache.tika.Tika

import play.api.Logger


object AgmipFileIdentifier {
  val jsonF = new JsonFactory();

  def apply(f: File): String = {
    val t = new Tika
    val contentType = t.detect(f)
    Logger.debug("Raw contentType: "+contentType)
    contentType match {
      case "application/gzip" => gzFileType(f)
      case "text/plain"       => textFileType(f)
      case "text/csv"         => textFileType(f)
      case _                  => "Supplemental File"
    }
  }

  def gzFileType(f: File):String = {
    // Need to ungzip this thing
    val file  = new GZIPInputStream(new FileInputStream(f))
    val jsonP = jsonF.createParser(file)
    try {
      val first = jsonP.nextToken()
      val second = jsonP.nextToken()
      first match {
        case JsonToken.START_OBJECT => {
          jsonP.getCurrentName match {
            case "experiments" | "weathers" | "soils" => "ACE"
            case _ => {
              val third = jsonP.nextToken()
              val fourth = jsonP.nextToken()
              jsonP.getCurrentName match {
                case "generators" | "rules" | "info" => "DOME"
                case _ => "Supplemental"
              }
            }
          }
        }
        case _ => "Supplemental"
      }
    } catch {
      case jpe: JsonParseException => "Supplemental"
      case ioe: IOException => {
        Logger.debug(ioe.toString)
        "Supplemental"
      }
    } finally {
      jsonP.close
      file.close
    }
  }

  def textFileType(f: File):String = {
    val file = Source.fromFile(f)
    try {
      val lines = file.getLines
      lines.foreach { l =>
        l.take(1) match {
          case "!" | "*" => {}
          case "#" => {
            l.contains("CROP_MODEL") match {
              case true => return "ACMO"
              case false => return "ALINK"
            }
          }
          case _ => return "Supplemental"
        }
      }
      "Supplemental"
    } finally {
      file.close
    }
  }
}
