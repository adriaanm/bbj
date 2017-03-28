package bbj

import akka.stream.ActorMaterializer
import play.api.data.validation.ValidationError
import play.api.libs.json._
import play.api.libs.ws.ahc.AhcWSClient
import play.api.libs.ws.{WS, WSAuthScheme, WSRequest}

import scala.concurrent.{ExecutionContext, Future}

trait JsonConnection {
  // http://stackoverflow.com/questions/7615645/ssl-handshake-alert-unrecognized-name-error-since-upgrade-to-java-1-7-0
  System.setProperty("jsse.enableSNIExtension", "false")

  implicit val context: ExecutionContext
  implicit val materializer: ActorMaterializer

  implicit val sslClient: AhcWSClient = AhcWSClient()

  def tryAuthUrl(url: String): WSRequest

  //  2011-05-18T15:37:07.000+0200
  implicit val dateRead = Reads.dateReads("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
  implicit val dateWrite = Writes.dateWrites("yyyy-MM-dd'T'HH:mm:ssZ")

  implicit val instantRead = Reads.instantReads("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
  implicit val instantWrite = Writes.DefaultInstantWrites

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
      try {
        tryAuthUrl(self).get().map { req =>
          try {  (req.json \ elemsField).as[List[T]] }
          catch { case x: Exception => println(s"failed to parse list $elemsField in ${req.json} "); Nil }
        }
      } catch {
        case e => Future.failed(e)
      }
    }
}
