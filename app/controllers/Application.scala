package cropsitedb.controllers

import play.api._
import play.api.mvc._

import play.api.db.DB
import play.api.libs.json._
import play.api.libs.functional.syntax._
import anorm._

import play.api.libs.ws._

import java.io.File
import java.util.Date
import java.text.SimpleDateFormat
import scala.collection.JavaConversions._

import play.api.libs.concurrent.Akka

import cropsitedb.actors.{Messages,ProcessACEB}
import cropsitedb.helpers._

import org.agmip.ace.AceDataset;
import org.agmip.ace.AceExperiment;
import org.agmip.ace.AceWeather;
import org.agmip.ace.AceSoil;
import org.agmip.ace.io._
import org.agmip.ace.util._
import org.agmip.ace.lookup.LookupCodes

import play.api.Play.current

import play.Logger

object Application extends Controller {

  case class CropCache(crid: String, name: String)

  val df = new SimpleDateFormat("yyyyMMdd")

  /*  def headers = List(
    "Access-Control-Allow-Origin" -> "*",
    "Access-Control-Allow-Methods" -> "GET, POST, OPTIONS, DELETE, PUT",
    "Access-Control-Max-Age" -> "3600",
    "Access-Control-Allow-Headers" -> "Origin, Content-Type, Accept, Authorization, Referer, Host, DNT, Accept-Encoding, Accept-Language, User-Agent",
    "Access-Control-Allow-Credentials" -> "true"
  )

  def rootOptions = options("/")
  def options(url: String) = Action { request =>
    NoContent.withHeaders(headers : _*)
  }*/


 // Root index - Documentation
 def index  = TODO

 // Upload
 def upload = Action(parse.multipartFormData) { implicit request =>
   // The DSID is assigned to a complete dataset as it is uploaded
   // Hopefully people are uploading related information.
   val acebProc = Akka.system.actorSelection("akka://application/user/process-aceb")

     val dsid = java.util.UUID.randomUUID().toString
     val freeze = request.body.dataParts.get("freeze").map(_.contains("true")).isDefined
     val filePath = "./uploads" //if(freeze) "./freezer" else "./uploads"
     request.body.file("aceb").map { aceb =>
       import java.io.File
       val filename = aceb.filename
       val contentType = aceb.contentType
       aceb.ref.moveTo(new File(s"$filePath/$dsid.aceb"))
       acebProc ! Messages.ProcessFile(dsid, filePath)
       Ok("Finished")
       }.getOrElse {
         BadRequest("No file selected")
       }
 }

 def structureQueryOutput(e:Stream[Row]):JsValue = {
   val v = e.map(r => r.asMap).map { lvl1 => lvl1.map {
     case (k,v) => {
       val nk = k.drop(13)
       val nv:Option[String] = v match {
         case Some(d1:Date) if nk.endsWith("date") => Some(AnormHelper.df.format(d1))
         case Some(d2:Date) if nk.endsWith("dat")  => Some(AnormHelper.df.format(d2))
         case Some(s:String)  => Some(s)
         case None     => None
         case _        => None
       }
       (nk,nv)
     }
   }}
   val x = v.map { z => z.collect {case (k, Some(v)) => (k,v) } }
   Json.toJson(x.toList)
 }

 def geohashQuery = Action(parse.json) { implicit request =>
   val qReqRes = request.body.validate[GeoHashHelper.GeoHashList]
   qReqRes.fold(
     errors => {
       BadRequest(Json.obj("error" -> "Invalid query"))
     },
     qReq => {
       DB.withConnection { implicit c =>
         val e = qReq.crid match {
           case Some(cr:String) => SQL("SELECT * FROM ace_metadata WHERE fl_geohash IN ({geohashes}) AND crid={crid} ORDER BY (dsid,crid,exname)").on('geohashes -> qReq.locations, 'crid -> cr).apply
           case _       => SQL("SELECT * FROM ace_metadata WHERE fl_geohash IN ({geohashes}) ORDER BY (dsid,crid,exname)").on('geohashes -> qReq.locations).apply
         }
         Ok(structureQueryOutput(e))
       }
     }
     )
 }

 // Generic GET Search
 def query = Action { implicit request =>
   val params = request.queryString
   DB.withConnection { implicit c =>
     val e = SQL("SELECT * FROM ace_metadata WHERE "+AnormHelper.dynIntersectBuilder(params)).
     on(params.map(AnormHelper.dynQueryToNamedParam(_)).toSeq:_*)
     //Logger.info(e.toString)
     Ok(structureQueryOutput(e.apply))
   }
 }

 // Download POST
 def download = Action(parse.json) { implicit request =>
   val dlReqRes = request.body.validate[DownloadHelper.DownloadRequest]
   dlReqRes.fold(
     errors => {
       BadRequest(Json.obj("error" -> "Invalid download request"))
     },
     dlReq => {
       val dlReqId  = java.util.UUID.randomUUID.toString
       val dest     = new AceDataset
       val destFile = new File("./downloads/"+dlReqId+".aceb")
       dlReq.downloads.foreach { download =>
         val sourceFile = new File("./uploads/"+download.dsid+".aceb")
         var sids:Set[String] = Set()
         var wids:Set[String] = Set()
         if (sourceFile.canRead) {
           val source = AceParser.parseACEB(sourceFile)
           source.linkDataset
           source.getExperiments.toList.foreach { ex =>
             val eid = ex.getId
             if (download.eids.contains(eid)) {
               dest.addExperiment(ex.rebuildComponent())
               sids = sids + ex.getSoil.getId
               wids = wids + ex.getWeather.getId
             }
           }
           source.getSoils.toList.foreach { s =>
             if (sids.contains(s.getId)) {
               dest.addSoil(s.rebuildComponent)
             }
           }
           source.getWeathers.toList.foreach { w =>
             if (wids.contains(w.getId)) {
               dest.addWeather(w.rebuildComponent)
             }
           }
           dest.linkDataset
         } else {
           BadRequest(Json.obj("error" -> "Missing dataset file"))
         }
       }
       Logger.info("Generating ACEB");
       AceGenerator.generateACEB(destFile, dest)
       val destUrl = routes.Application.serve(dlReqId).absoluteURL()
       if(dlReq.galaxyUrl.isDefined && dlReq.toolId.isDefined) {
         // Need to send this information to the Galaxy server
         val client = WS.client
         client.url(dlReq.galaxyUrl.get).post(Map("URL"->Seq(destUrl), "TOOL_ID"->Seq(dlReq.toolId.get)))
         Logger.info("Sending to galaxy")
         Ok(Json.obj())
       } else {
         Ok(Json.obj("url" -> destUrl))
       }
     }
     )
 }

 // Download GET
 def serve(dlid: String) = Action { implicit request =>
   val download = new File("./downloads/"+dlid+".aceb")
   if (download.canRead) {
     Ok.sendFile(download)
   } else {
     BadRequest("Missing file")
   }
 }

 implicit val cropCacheWrites: Writes[CropCache] = (
   (JsPath \ "crid").write[String] and
   (JsPath \ "name").write[String]
 )(unlift(CropCache.unapply))

 def cropCache = Action {
   DB.withConnection { implicit c =>
     val q = SQL("SELECT DISTINCT crid FROM ace_metadata ORDER BY crid")
     val crops = q().map { r => {
       val crid = r[Option[String]]("crid")
       CropCache(crid.get, LookupCodes.lookupCode("crid", crid.get, "cn"))
     }}.toList

     Ok(Json.toJson(crops))
   }
 }

 def locationCache = Action { implicit request =>
   val params = request.getQueryString("crid")
   DB.withConnection { implicit c =>
     val q = params match {
       case Some(p: String) => SQL("SELECT fl_lat, fl_long, fl_geohash, count(*) AS count FROM ace_metadata  WHERE crid={crid} GROUP BY fl_lat, fl_long, fl_geohash").on("crid"->p.toUpperCase)
       case _               => SQL("SELECT fl_lat, fl_long, fl_geohash, count(*) AS count FROM ace_metadata GROUP BY fl_lat, fl_long, fl_geohash")
     }
     val loc = q().map { r => {
       val lat = r[Option[String]]("fl_lat")
       val lng = r[Option[String]]("fl_long")
       val gh  = r[Option[String]]("fl_geohash")
       val c   = r[Long]("count")


       if(gh.isDefined && gh.get == "") {
         GeoJsonHelper.GeoJsonPoint(Some("0.0"), Some("0.0"), c, Some(""))
       } else {
         GeoJsonHelper.GeoJsonPoint(lat.map{l:String => if(l.endsWith(".")) l.dropRight(1) else l},lng.map{l:String => if(l.endsWith(".")) l.dropRight(1) else l}, c, gh)
       }
     }}.toList

     Ok(GeoJsonHelper.buildLocations(loc))
   }
 }

 def bulkQuery() = Action { implicit request =>
   // Step 1. Lookup the site and key to make sure we get a host hit.
   // Step 2. Lookup the host and get the last time accessed
   // Step 3. Query metadata for everything since the last call
   Ok("Done.")
 }
}
