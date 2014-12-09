package cropsitedb.helpers

import anorm.{NamedParameter, SeqParameter}
import java.text.SimpleDateFormat
import scala.collection.mutable.Buffer

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

object AnormHelper {
  val df = new SimpleDateFormat("yyyyMMdd")

  def varJoin(x: List[(String,_)]):String = {
    x.foldLeft(""){ (a,n) => a + "," + n._1 }.drop(1)
  }

  def valJoin(x: List[(String,_)]):String = {
    "{"+(x.foldLeft(""){ (a,n) => a + "},{" + n._1}.drop(3))+"}"
  }

  def agmipToNamedParam(x: (String,String)):NamedParameter = {
    x._1 match {
      case d1 if d1.endsWith("date") => new NamedParameter(d1, Some(df.parse(x._2)))
      case d2 if d2.endsWith("dat")  => new NamedParameter(d2, Some(df.parse(x._2)))
      case s => new NamedParameter(s, Some(x._2))
    }
  }

  def dynQueryToNamedParam(x: (String,Seq[String])):NamedParameter = {
    x._1 match {
      case d1 if d1.endsWith("date") => new NamedParameter(d1, SeqParameter(x._2.map{ z => Some(df.parse(z))}))
      case d2 if d2.endsWith("dat")  => new NamedParameter(d2, SeqParameter(x._2.map{ z => Some(df.parse(z))}))
      case cr if cr == "crid"        => new NamedParameter(cr, x._2.toSeq.map(v => Some(v.toUpperCase)))
      case s => new NamedParameter(s, x._2.toSeq.map(Some(_)))
    }
  }

  // By default, using IN to help with the querying
  def dynGenericBuilder(x: Map[String, Seq[String]], oper: String) = {
    x.keys.foldLeft("") { (a,n) => a + " "+oper+ " "+n+" IN ({"+n+"})" }.drop(2+oper.length)
  }

  def dynIntersectBuilder(x: Map[String, Seq[String]]): String = {
    dynGenericBuilder(x, "AND")
  }

  def dynUnionBuilder(x: Map[String, Seq[String]]): String = {
    dynGenericBuilder(x, "OR")
  }
}


object DatasetHelper {
  case class CreateDatasetRequest(email: String, title: Option[String], freeze: Option[Boolean])
  case class DeleteDatasetRequest(email: String, dsid: String)

  implicit val CreateDatasetRequestReads = Json.reads[CreateDatasetRequest]
  implicit val DeleteDatasetRequestReads = Json.reads[DeleteDatasetRequest]
}

// TODO: Needs support for emails / logins
// TODO: Needs support for Galaxy extensions (GALAXY_URL and TOOL_ID)
object DownloadHelper {
  case class DownloadRequest(email: Option[String], galaxyUrl: Option[String], toolId: Option[String], fileTypes: Int, downloads: Seq[DSIDRequest])
  case class DSIDRequest(dsid: String, eids: Seq[String])

  implicit val DSIDRequestReads = Json.reads[DSIDRequest]
  /* implicit val DSIDRequestReads: Reads[DSIDRequest] = (
    (JsPath \ "dsid").read[String] and
    (JsPath \ "eids").lazyRead(Reads.seq[String])
  )(DSIDRequest.apply _) */

  implicit val DownloadRequestReads: Reads[DownloadRequest] = (
    (JsPath \ "email").readNullable[String] and
    (JsPath \ "galaxy_url").readNullable[String] and
    (JsPath \ "tool_id").readNullable[String] and
    (JsPath \ "types").read[Int] and
    (JsPath \ "downloads").read[Seq[DSIDRequest]]//lazyRead(Reads.seq[DSIDRequest])
  )(DownloadRequest.apply _)
}

object GeoJsonHelper {
  case class GeoJsonPoint(lat: Option[String], lng: Option[String], count: Long, geohash: Option[String])
  val startString = """{"type":"FeatureCollection","features":[""";
  val endString   = """]}"""

  def buildLocations(l: Seq[GeoJsonPoint]): String = {
    val locs = l.foldLeft(startString) { (a,n) => a + """{"type":"Feature","geometry":{"type":"Point","coordinates":["""+n.lng.get+","+n.lat.get+"""]},"properties":{"""+(if (n.geohash.isDefined) """"geohash":""""+n.geohash.get+"""",""" else "")+""""count":""""+n.count+""""}},""" }
      locs.dropRight(if (locs.endsWith(",")) 1 else 0) + endString
  }
}

object GeoHashHelper {
  case class NaviLL(lat: Option[String], lng: Option[String])
  case class NaviPoint(lat: Option[String], lng: Option[String], geohash: Option[String], countryISO: Option[String], adm0: Option[String], adm1: Option[String], adm2: Option[String], error: Option[String])
  case class GeoHashList(crid: Option[String], locations: Seq[String])

  implicit val naviLLWrites    = Json.writes[NaviLL]
  implicit val naviLLReads     = Json.reads[NaviLL]
  implicit val naviPointWrites = Json.writes[NaviPoint]
  implicit val naviPointReads  = Json.reads[NaviPoint]
  implicit val geoHashListReads = Json.reads[GeoHashList]
  implicit val geoHashListWrites = Json.writes[GeoHashList]

}
