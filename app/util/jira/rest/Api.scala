package util.jira.rest

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

trait JIRAConnection {
  // try to authorize, or fall back to non-auth
  // need to authorize to get voters & watchers, apparently...
  def tryAuthUrl(url: String) = {
    val getString = play.api.Play.current.configuration.getString(_: String)
    (for (user <- getString("jira.user");
         pass <- getString("jira.password"))
      yield WS.url(url).withAuth(user, pass, com.ning.http.client.Realm.AuthScheme.BASIC)).getOrElse(WS.url(url))
  }
}

trait Validation extends JIRAConnection {
  //  2011-05-18T15:37:07.000+0200
  implicit val dateRead = Reads.dateReads("yyyy-MM-dd'T'HH:mm:ss.SSSZ")

  def validate[T](tp: String)(x: Option[T]): JsResult[T] =
    x.map(JsSuccess apply _) getOrElse JsError(Seq(JsPath() -> Seq(ValidationError("validate.error.expected." + tp))))

  def optField(x: JsValue)(name: String): Option[JsValue] = x match {
    case o: JsObject => o.value.get(name)
    case _ => None
  }

  def name(json: JsValue) = (json \ "name").asOpt[String]
  def self(json: JsValue) = (json \ "self").asOpt[String]
  def id(json: JsValue)   = (json \ "id").asOpt[String] map Integer.parseInt

  def lazyList[T](json: JsValue, countField: String, elemsField: String)(implicit creator: Reads[T]): Future[List[T]] = Future.successful(Nil)
//    if ((json \ countField).as[Int] == 0) Future.successful(Nil)
//    else {
//      import play.api.libs.concurrent.Execution.Implicits._
//      val self = (json \ "self").as[String]
//      tryAuthUrl(self).get().map{ req =>
//        (req.json \ elemsField).as[List[T]]
//      }
//    }
}

object User extends Validation {
  implicit object reads extends Reads[User] {
    def reads(json: JsValue): JsResult[User] = validate("user")(for (
      self <- self(json);
      name <- name(json)
    ) yield User(self, name))
  }

  private val users = collection.mutable.HashMap[String, User]()
  def apply(self: String, name: String) =
    users.getOrElseUpdate(self, new User(self, name))

}
class User(val self: String, val name: String) {
  override def toString = name
}

object Status extends Validation {
  implicit object reads extends Reads[Status] {
    def reads(json: JsValue): JsResult[Status] = validate(s"status; got $json")(for (
      self        <- self(json);
      id          <- id(json);
      name        <- name(json);
      description <- (json \ "description").asOpt[String]
    ) yield Status(self, id, name, description))
  }

  private val stati = collection.mutable.HashMap[String, Status]()
  def apply(self: String, id: Int, name: String, description: String) =
    stati.getOrElseUpdate(self, new Status(self, id, name, description))
}
class Status(val self: String, val id: Int, val name: String, val description: String)  {
  override def toString = name
}

object Resolution extends Validation {
  implicit object reads extends Reads[Resolution] {
    def reads(json: JsValue): JsResult[Resolution] = validate(s"resolution; got $json")(for (
      self        <- self(json);
      id          <- id(json);
      name        <- name(json);
      description <- (json \ "description").asOpt[String]
    ) yield Resolution(self, id, name, description))
  }

  private val resolutions = collection.mutable.HashMap[String, Resolution]()
  def apply(self: String, id: Int, name: String, description: String) =
    resolutions.getOrElseUpdate(self, new Resolution(self, id, name, description))
}
class Resolution(val self: String, val id: Int, val name: String, val description: String)  {
  override def toString = name
}

object Version extends Validation {
  implicit object reads extends Reads[Version] {
    def reads(json: JsValue): JsResult[Version] = validate(s"version; got $json")(for (
      self            <- self(json);
      id              <- id(json);
      name            <- name(json);
      userReleaseDate <- (optField(json)("userReleaseDate")).map(_.asOpt[String]).orElse(Some(None));
      releaseDate     <- (optField(json)("releaseDate")).map(_.asOpt[Date](Reads.DefaultDateReads)).orElse(Some(None));
      archived        <- (json \ "archived").asOpt[Boolean];
      released        <- (json \ "released").asOpt[Boolean]
    ) yield new Version(self, id, name, userReleaseDate, releaseDate, archived, released))
  }

  private val versions = collection.mutable.HashMap[String, Version]()
  def apply(self: String, id: Int, name: String, userReleaseDate: Option[String], releaseDate: Option[Date], archived: Boolean, released: Boolean) =
    versions.getOrElseUpdate(self, new Version(self, id, name, userReleaseDate, releaseDate, archived, released))
}
class Version(val self: String, val id: Int, val name: String, val userReleaseDate: Option[String], val releaseDate: Option[Date], val archived: Boolean, val released: Boolean)  {
  override def toString = name
}

object Comment extends Validation {
  implicit object reads extends Reads[Comment] {
    def reads(json: JsValue): JsResult[Comment] = validate(s"comment; got $json")(for (
      author       <- (json \ "author").asOpt[User];
      body         <- (json \ "body").asOpt[String];
      updateAuthor <- (json \ "updateAuthor").asOpt[User];
      created      <- (json \ "created").asOpt[Date];
      updated      <- (json \ "updated").asOpt[Date]
    ) yield Comment(author, body, updateAuthor, created, updated))
  }
}
case class Comment(author: User, body: String, updateAuthor: User, created: Date, updated: Date) {
  override def toString = s"on $created, $author said '$body' ($updated, $updateAuthor)"
}

object Attachment extends Validation {
  implicit object reads extends Reads[Attachment] {
    def reads(json: JsValue): JsResult[Attachment] = validate(s"attachment; got $json")(for (
      filename   <- (json \ "filename").asOpt[String];
      author     <- (json \ "author").asOpt[User];
      created    <- (json \ "created").asOpt[Date];
      size       <- (json \ "size").asOpt[Int];
      mimeType   <- (json \ "mimeType").asOpt[String];
      properties <- (optField(json)("properties")).map(_.asOpt[JsObject]).orElse(Some(None));
      content    <- (json \ "content").asOpt[String]
    ) yield Attachment(filename, author, created, content, size, mimeType, properties))
  }
}
case class Attachment(filename: String, author: User, created: Date, content: String, size: Int, mimeType: String, properties: Option[JsObject])

object IssueLink extends Validation {
  implicit object reads extends Reads[IssueLink] {
    def reads(json: JsValue): JsResult[IssueLink] = validate(s"issue link; got $json")(for (
      name    <- name(json \ "type");
      inward  <- (json \ "type" \ "inward").asOpt[String];
      outward <- (json \ "type" \ "outward").asOpt[String];
      outwardIssue <- (for(
          out <- (optField(json)("outwardIssue"));
          out <- out.asOpt[JsObject]) yield (out \ "key").asOpt[String]).orElse(Some(None));
      inwardIssue <- (for(
          in  <- (optField(json)("inwardIssue"));
          in  <- in.asOpt[JsObject]) yield (in \ "key").asOpt[String]).orElse(Some(None))
    ) yield IssueLink(name, outward, outwardIssue, inward, inwardIssue))
  }
}
case class IssueLink(name: String, outward: String, outwardIssue: Option[String], inward: String, inwardIssue: Option[String])

object api extends Validation {
  import play.api.libs.concurrent.Execution.Implicits._

  private val jsonRepo = "/Users/adriaan/jira"
  private def jiraUrl(uri: String) = "https://issues.scala-lang.org/rest/api/latest" + uri
  private def downloadIssue(i: Int) = tryAuthUrl(jiraUrl(s"/issue/SI-${i}?expand=changelog")).get()

  // write the inputStream to a FileOutputStream and make sure the recevied Json parses (sometimes the server returns garbage!?)
  private def copyJsonToFile(in: InputStream, f: File, bufferSize: Int = 1024) = /*f.synchronized*/ {
    val bytes = new Array[Byte](bufferSize)
    val out = new FileOutputStream(f)
    val contents = ArrayBuffer[Byte]()

    @annotation.tailrec
    def write(): Unit =
      in.read(bytes) match {
        case -1 =>
        case count =>
          val read = new Array[Byte](count)
          Array.copy(bytes, 0, read, 0, read.size)
          contents ++= read
          out.write(bytes, 0, count)
          write()
      }

    try { write(); Json.parse(new String(contents.toArray)) }
    catch { case e =>
      println("FILE EXCEPTION: "+ e)
      f.delete()
      throw e
    }
    finally { out.flush(); out.close() }
  }

  // this needs to run until fixpoint...
  // TODO:
  // - convert into proper monadic computation that tracks whether something was changed and repeat
  // - do twice and make sure the contents don't change --> use git repo?
  private def tryDownloadIssues(issues: List[Int]): List[Future[Option[Int]]] =
    issues.map { i =>
      try {
        val f = new File(s"$jsonRepo/${i}.json")
        if (f.exists) Future.successful(None) // assume corrupted files are deleted by copyToFile
        else downloadIssue(i).map { resp =>
          copyJsonToFile(resp.ahcResponse.getResponseBodyAsStream, f)
          println("YEP: "+ i)
          None
        } recover { case e =>
          println(s"MEH: $i with $e")
          Some(i)
        }
      } catch {
        case e: Exception =>
          println(s"$i failed: $e")
          Future.successful(Some(i))
      }
    }

  private def downloadIssues(): Future[Unit]  = {
    def loop(failed: List[Int]): Future[Unit] =
      Future.sequence(tryDownloadIssues(failed)).map(_.flatten).flatMap {
        case Nil => Future.successful(())
        case failed =>
          println("retrying: "+ failed)
          loop(failed)
      }

    loop((firstIssueKey to lastIssueKey).toList)
  }

  def getIssue(key: String): Future[Issue] = //tryAuthUrl(jiraUrl(s"/issue/$key?expand=changelog")).get().map(req => parseIssue(req.json))
    Future {
      val f = new File(s"$jsonRepo/${key.substring(3)}.json") // drop "SI-" prefix
      val data = new Array[Byte](f.length.asInstanceOf[Int])
      (new DataInputStream(new FileInputStream(f))).readFully(data)
      parseIssue(Json.parse(new String(data)))
    }

  val firstIssueKey = 1
  val lastIssueKey = 1000 // 6856
  def issues: IndexedSeq[Future[Issue]] = {
   val keys = (firstIssueKey to lastIssueKey)
   if (keys.exists{i => !(new File(s"$jsonRepo/${i}.json")).exists})
     Await.ready(downloadIssues(), Duration.Inf )

   keys.map (i => getIssue("SI-"+i) recover { case e: Exception =>
     e.printStackTrace()
     Issue("SI-"+i, Map("!!!exception" -> e), Nil)
   })
  }

  def parseIssue(i: JsValue): Issue =
    Issue((i \ "key").as[String], (i \ "fields").as[Map[String, JsValue]].map { case (k, v) => parseField(k, v) }, (i \ "changelog" \ "histories").as[List[JsValue]])

  def parseField(field: String, v: JsValue): (String, Any) = (field,
    try field match {
      case "description"       => v.asOpt[String]
      case "summary"           => v.as[String]
      case "reporter"          => v.as[User]
      case "assignee"          => v.asOpt[User]
      case "environment"       => v.asOpt[String]
      case "status"            => v.as[Status]
      case "resolution"        => v.asOpt[Resolution]
      case "resolutiondate"    => v.asOpt[Date]
      case "created"           => v.as[Date]
      case "updated"           => v.as[Date]
      case "duedate"           => v.asOpt[Date]
      case "versions"          => v.as[List[Version]] // affected version
      case "fixVersions"       => v.as[List[Version]]
      case "issuetype"         => (v \ "name").as[String] // IssueType
      case "priority"          => (v \ "name").as[String] // Priority
      case "labels"            => v.as[List[String]]
      case "issuelinks"        => v.as[List[IssueLink]]
      case "components"        => v.as[List[JsObject]].map(c => (c \ "name").as[String])
      case "comment"           => (v \ "comments").as[List[Comment]] // List[Comment]
      case "attachment"        => v.as[List[Attachment]]
      case "issuekey"          => v.as[String]
      case "project"           => (v \ "key").as[String] // Project
      case "votes"             => lazyList[User](v, "votes", "voters") // List[Vote]
      case "watches"           => lazyList[User](v, "watchCount", "watchers") // List[Watch]
      case "customfield_10005" => v.asOpt[List[User]] // trac cc
      case "customfield_10101" => v.asOpt[List[String]] // flagged
      case "customfield_10104" => v.asOpt[Float] // Story points
      case "customfield_10105" => v.asOpt[Float] // business value
      case "subtasks"          => v
      case "workratio"         => v.asOpt[Float]
    } catch {
      case e: Exception => throw new Exception(s"Error parsing field $field : $v", e)
    })
}

// TODO: scalac bug, why does this have to come after the companion object?
case class Issue(key: String, fields: Map[String, Any], changelog: List[JsValue]) {
}