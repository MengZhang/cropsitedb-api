package cropsitedb.controllers

import java.util.Calendar
import java.util.Date

import anorm._
import play.api._
import play.api.db.DB
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.api.mvc._

import cropsitedb.helpers.JsonHelper

import play.api.Play.current


object MetadataController extends Controller {
  case class MetadataRequest(key: String)
  implicit val MetadataRequestReads = Json.reads[MetadataRequest]

  def dumpMetadata = Action(parse.json) { implicit request =>
    val qReqRes = request.body.validate[MetadataRequest]
    qReqRes.fold(
      errors => {
        BadRequest(Json.obj("error" -> "Invalid query"))
      },
      qReq => {
        DB.withConnection { implicit c =>
          val securityQ = SQL("SELECT site,bulk_loading FROM extern_hosts WHERE site_key={qKey}").on('qKey -> qReq.key).apply.headOption
          securityQ match {
            case None => BadRequest(Json.obj("error" -> "Invalid query"))
            case Some(siteRow) => {
              val site = siteRow[String]("site")
              val collabMode = siteRow[Option[Int]]("bulk_loading") match {
                case None => false
                case Some(x) => x match {
                  case 0 => false
                  case _ => true
                }
              }
              val lastQ = if (collabMode) {
                None
              } else {
                SQL("SELECT last_seen FROM bulk_loading WHERE site={qSite}").on('qSite -> site).apply.headOption
              }
              val now = new Date()
              val cal = Calendar.getInstance()

              val check = lastQ match {
                case None => {
                  if(collabMode) {} else {
                    val insertResult = SQL("INSERT INTO bulk_loading VALUES ({qSite}, {qDate})").on('qSite -> site, 'qDate -> now).execute()
                  }
                  cal.set(Calendar.YEAR, 1981)
                  cal.set(Calendar.MONTH, Calendar.JULY)
                  cal.set(Calendar.DAY_OF_MONTH, 8)
                  cal
                }
                case Some(lastRow) => {
                  val last = lastRow[Date]("last_seen")
                  cal.setTime(last)
                  val updateResult = SQL("UPDATE bulk_loading SET last_seen = {qDate} WHERE site = {qSite}").on('qSite -> site, 'qDate -> now).execute()
                  cal
                }
              }
              check.set(Calendar.HOUR_OF_DAY, 0)
              check.set(Calendar.MINUTE, 0)
              check.set(Calendar.SECOND, 0)
              check.set(Calendar.MILLISECOND, 1)
              check.add(Calendar.DAY_OF_MONTH, -1)
              val q = SQL("SELECT * FROM ace_metadata WHERE created > {qDate} AND (NOT (api_source <> {source} or api_source IS NULL OR {source} IS NULL) OR (api_source IS NULL AND {source} IS NULL))")
                .on('qDate -> new Date(check.getTimeInMillis()), 'source -> "AgMIP").apply
              Ok(JsonHelper.structureQueryOutput(q))
            }
          }
        }
      }
    )
  }
}






