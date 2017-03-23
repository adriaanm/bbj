package bbj

import play.api.libs.ws.{ WS, Response }
import play.api.libs.json.{ JsValue, Json, JsNull, Reads, JsSuccess, JsError, JsPath, JsResultException }
import play.api.data.validation.ValidationError
import java.util.Date
import play.api.libs.json.JsResult
import scala.concurrent.Future
import scala.concurrent.Await
import play.api.libs.json.JsObject
import scala.concurrent.duration.Duration
import java.io.FileInputStream
import java.io.File
import java.io.DataInputStream
import java.io.InputStream
import java.io.FileOutputStream
import scala.collection.mutable.ArrayBuffer

trait JsonConnection {
  implicit lazy val defaultContext: scala.concurrent.ExecutionContext = play.api.libs.concurrent.Execution.Implicits.defaultContext

  def getString(x: String) = play.api.Play.current.configuration.getString(x)

  def user: Option[String]
  def pass: Option[String]

  // try to authorize, or fall back to non-auth
  // need to authorize to get voters & watchers, apparently...
  def tryAuthUrl(url: String) = {
    val req = WS.url(url)
    if (user.isEmpty || pass.isEmpty) req
    else req.withAuth(user.get, pass.get, com.ning.http.client.Realm.AuthScheme.BASIC)
  }

  //  2011-05-18T15:37:07.000+0200
  implicit val dateRead = Reads.dateReads("yyyy-MM-dd'T'HH:mm:ss.SSSZ")

  def validate[T](tp: String)(x: Option[T]): JsResult[T] =
    x.map(JsSuccess apply _) getOrElse JsError(Seq(JsPath() -> Seq(ValidationError("validate.error.expected." + tp))))

  def optField(x: JsValue)(name: String): Option[JsValue] = x match {
    case o: JsObject => o.value.get(name)
    case _           => None
  }

  def name(json: JsValue) = (json \ "name").asOpt[String]
  def self(json: JsValue) = (json \ "self").asOpt[String]
  def id(json: JsValue) = (json \ "id").asOpt[String] map Integer.parseInt

  def lazyList[T](json: JsValue, countField: String, elemsField: String)(implicit creator: Reads[T]): Future[List[T]] =
    if ((json \ countField).as[Int] == 0) Future.successful(Nil)
    else {
      val self = (json \ "self").as[String]
      tryAuthUrl(self).get().map { req =>
        try {  (req.json \ elemsField).as[List[T]] }
        catch { case x: Exception => println(s"failed to parse list $elemsField in ${req.json} "); Nil }
      }
    }
}
