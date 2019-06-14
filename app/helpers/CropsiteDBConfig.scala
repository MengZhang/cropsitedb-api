package cropsitedb.helpers

import play.api.Play

object CropsiteDBConfig {
  val naviUrl = rewriteUrl(Play.current.configuration.getString("navi.baseurl").getOrElse("https://api.agmip.org/navi/1"))
  val agtrialsUrl = rewriteUrl(Play.current.configuration.getString("extern.agtrials.url").getOrElse("http://localhost:8083/api/agtrials"))
  val localFileStore = rewriteUrl(Play.current.configuration.getString("datastore.local.directory").getOrElse("."))

  def rewriteUrl(url: String):String = {
    if (url.endsWith("/")) {
      url.substring(0, url.length-1)
    } else {
      url
    }
  }
}


