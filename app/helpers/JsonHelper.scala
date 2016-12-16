package cropsitedb.helpers

import java.util.Date

import play.api.libs.json._
import play.api.libs.functional.syntax._
import anorm._

import com.fasterxml.jackson.core.{JsonFactory, JsonParser, JsonToken}


object JsonHelper {
  val factory = new JsonFactory()

  def toListTuple2(p: JsonParser, collected: List[Tuple2[String,String]]): List[Tuple2[String,String]] = {
    val t = p.nextToken
    t match {
      case JsonToken.END_OBJECT => collected
      case JsonToken.FIELD_NAME => {
        val field = p.getCurrentName()
        p.nextToken
        val value = Option(p.getText())
        value match {
          case None => toListTuple2(p, collected)
          case Some(v) => (if (v.length > 0) toListTuple2(p, Tuple2(field,v) :: collected) else toListTuple2(p, collected))
        }
      }
      case _ => toListTuple2(p, collected)
    }
  }

    def structureQueryOutput(e:Stream[Row]):JsValue = {
    val v = e.map(r => r.asMap).map { lvl1 => lvl1.map {
      case (k,v) => {
        val nk = k.drop(13)
        val nv:Option[JsValue] = v match {
          case Some(d1:Date) if nk.endsWith("date") => Some(JsString(AnormHelper.df.format(d1)))
          case Some(d2:Date) if nk.endsWith("dat")  => Some(JsString(AnormHelper.df.format(d2)))
          case Some(s:String) if (nk == "obs_vars") => Some(JsArray(s.split(" ").map(z => JsString(z))))
          case Some(s:String) => Some(JsString(s))
          case None     => None
          case _        => None
        }
        (nk,nv)
      }
    }}
    val x = v.map { z => z.collect { case (k, Some(v)) => (k, v) }}
    Json.toJson(x.toList)
  }
}
