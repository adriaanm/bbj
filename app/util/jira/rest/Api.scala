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

trait Common {
  implicit lazy val defaultContext: scala.concurrent.ExecutionContext = play.api.libs.concurrent.Execution.Implicits.defaultContext

  def getString(x: String) = play.api.Play.current.configuration.getString(x)

  // try to authorize, or fall back to non-auth
  // need to authorize to get voters & watchers, apparently...
  def tryAuthUrl(url: String) = {
    (for (
      user <- getString("jira.user");
      pass <- getString("jira.password")
    ) yield WS.url(url).withAuth(user, pass, com.ning.http.client.Realm.AuthScheme.BASIC)).getOrElse(WS.url(url))
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

object User extends Common {
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

object Status extends Common {
  implicit object reads extends Reads[Status] {
    def reads(json: JsValue): JsResult[Status] = validate(s"status; got $json")(for (
      self <- self(json);
      id <- id(json);
      name <- name(json);
      description <- (json \ "description").asOpt[String]
    ) yield Status(self, id, name, description))
  }

  private val stati = collection.mutable.HashMap[String, Status]()
  def apply(self: String, id: Int, name: String, description: String) =
    stati.getOrElseUpdate(self, new Status(self, id, name, description))
}
class Status(val self: String, val id: Int, val name: String, val description: String) {
  override def toString = name
}

object Resolution extends Common {
  implicit object reads extends Reads[Resolution] {
    def reads(json: JsValue): JsResult[Resolution] = validate(s"resolution; got $json")(for (
      self <- self(json);
      id <- id(json);
      name <- name(json);
      description <- (json \ "description").asOpt[String]
    ) yield Resolution(self, id, name, description))
  }

  private val resolutions = collection.mutable.HashMap[String, Resolution]()
  def apply(self: String, id: Int, name: String, description: String) =
    resolutions.getOrElseUpdate(self, new Resolution(self, id, name, description))
}
class Resolution(val self: String, val id: Int, val name: String, val description: String) {
  override def toString = name
}

/*
issues.flatMap(_.fields("versions").asInstanceOf[List[Version]]).toSet
Set(Scala 2.10.0-M4, Scala 2.8.1, Scala 2.10.0-M7, Scala 2.9.2, Scala 2.10.0, Scala 2.11.0, Scala 2.10.0-M3, Scala 2.10.0-RC5, Scala 2.9.0,
    Scala 2.9.1, Scala 2.10.0-M5, Scala 2.9.3-RC1, Scala 2.10.0-RC2, Scala 2.7.7, Scala 2.9.0-1, Scala 2.10.0-M6, Scala 2.10.1-RC1, Scala 2.10.0-M2, Scala 2.10.0-M1, Scala 2.8.0, macro-paradise, Scala 2.11.0-M1, Scala 2.10.0-RC1, Scala 2.10.0-RC3)
 */
object Version extends Common {
  implicit object reads extends Reads[Version] {
    def reads(json: JsValue): JsResult[Version] = validate(s"version; got $json")(for (
      self <- self(json);
      id <- id(json);
      name <- name(json);
      userReleaseDate <- (optField(json)("userReleaseDate")).map(_.asOpt[String]).orElse(Some(None));
      releaseDate <- (optField(json)("releaseDate")).map(_.asOpt[Date](Reads.DefaultDateReads)).orElse(Some(None));
      archived <- (json \ "archived").asOpt[Boolean];
      released <- (json \ "released").asOpt[Boolean]
    ) yield Version(self, id, name, userReleaseDate, releaseDate, archived, released))
  }

  private val versions = collection.mutable.HashMap[String, Version]()
  def apply(self: String, id: Int, name: String, userReleaseDate: Option[String], releaseDate: Option[Date], archived: Boolean, released: Boolean) =
    versions.getOrElseUpdate(self, new Version(self, id, name, userReleaseDate, releaseDate, archived, released))
}
class Version(val self: String, val id: Int, val name: String, val userReleaseDate: Option[String], val releaseDate: Option[Date], val archived: Boolean, val released: Boolean) {
  override def toString = name
}

object Comment extends Common {
  implicit object reads extends Reads[Comment] {
    def reads(json: JsValue): JsResult[Comment] = validate(s"comment; got $json")(for (
      author <- (json \ "author").asOpt[User];
      body <- (json \ "body").asOpt[String];
      updateAuthor <- (json \ "updateAuthor").asOpt[User];
      created <- (json \ "created").asOpt[Date];
      updated <- (json \ "updated").asOpt[Date]
    ) yield Comment(author, body, updateAuthor, created, updated))
  }
}
case class Comment(author: User, body: String, updateAuthor: User, created: Date, updated: Date) {
  override def toString = s"on $created, $author said '$body' ($updated, $updateAuthor)"
}

object Attachment extends Common {
  implicit object reads extends Reads[Attachment] {
    def reads(json: JsValue): JsResult[Attachment] = validate(s"attachment; got $json")(for (
      filename <- (json \ "filename").asOpt[String];
      author <- (json \ "author").asOpt[User];
      created <- (json \ "created").asOpt[Date];
      size <- (json \ "size").asOpt[Int];
      mimeType <- (json \ "mimeType").asOpt[String];
      properties <- (optField(json)("properties")).map(_.asOpt[JsObject]).orElse(Some(None));
      content <- (json \ "content").asOpt[String]
    ) yield Attachment(filename, author, created, content, size, mimeType, properties))
  }
}
case class Attachment(filename: String, author: User, created: Date, content: String, size: Int, mimeType: String, properties: Option[JsObject])

/**
scala> issues.flatMap(_.fields("issuelinks").asInstanceOf[List[IssueLink]].map(_.name)).distinct
Vector(Relates, Duplicate, Blocks, Cloners)

scala> issues.flatMap(_.fields("issuelinks").asInstanceOf[List[IssueLink]].map(_.inward)).distinct
Vector(relates to, is duplicated by, is blocked by, is cloned by)

scala> issues.flatMap(_.fields("issuelinks").asInstanceOf[List[IssueLink]].map(_.outward)).distinct
Vector(relates to, duplicates, blocks, clones)
 *
 */
object IssueLink extends Common {
  implicit object reads extends Reads[IssueLink] {
    def reads(json: JsValue): JsResult[IssueLink] = validate(s"issue link; got $json")(for (
      name <- name(json \ "type");
      inward <- (json \ "type" \ "inward").asOpt[String];
      outward <- (json \ "type" \ "outward").asOpt[String];
      outwardIssue <- (for (
        out <- (optField(json)("outwardIssue"));
        out <- out.asOpt[JsObject]
      ) yield (out \ "key").asOpt[String]).orElse(Some(None));
      inwardIssue <- (for (
        in <- (optField(json)("inwardIssue"));
        in <- in.asOpt[JsObject]
      ) yield (in \ "key").asOpt[String]).orElse(Some(None))
    ) yield IssueLink(name, outward, outwardIssue, inward, inwardIssue))
  }
}
case class IssueLink(name: String, outward: String, outwardIssue: Option[String], inward: String, inwardIssue: Option[String]) {
  // assert:
  (name, outward, inward) match {
    case ("Relates", "relates to", "relates to") =>
    case ("Duplicate", "is duplicated by", "duplicates") =>
    case ("Duplicate", "duplicates", "is duplicated by") =>
    case ("Blocks", "is blocked by", "blocks") =>
    case ("Blocks", "blocks", "is blocked by") =>
    case ("Cloners", "is cloned by", "clones") =>
    case ("Cloners", "clones", "is cloned by") =>
  }
}

/** to experiment from the play console:

new play.core.StaticApplication(new java.io.File("."))
import util.jira.rest._
val issues = concurrent.Await.result(api.issues, concurrent.duration.Duration.Inf)

 */
object api extends Common {
  private def readFully(in: InputStream, bufferSize: Int = 1024): Array[Byte] = {
    val bytes = new Array[Byte](bufferSize)
    val contents = ArrayBuffer[Byte]()

    @annotation.tailrec
    def read(): Unit =
      in.read(bytes) match {
        case -1 =>
        case count =>
          val data = new Array[Byte](count)
          Array.copy(bytes, 0, data, 0, count)
          contents ++= data
          read()
      }

    read()
    contents.toArray
  }

  private lazy val cacheDir = {
    val cacheDir = getString("jira.cacheDir") getOrElse "/Users/adriaan/jira"
    assert(new File(cacheDir).isDirectory(), s"Please create cache directory $cacheDir to avoid hammering the jira server.")
    cacheDir
  }

  private def fileFor(key: Int) = new File(s"$cacheDir/${key}.json")
  private def jiraUrl(uri: String) = "https://issues.scala-lang.org/rest/api/latest" + uri

  def getIssue(i: Int): Future[Option[Issue]] = {
    def loadCachedIssue =
      Future (Some {
        val f = fileFor(i)
        val data = new Array[Byte](f.length.asInstanceOf[Int])
        (new DataInputStream(new FileInputStream(f))).readFully(data)
        parseIssue(data)
      })

    def downloadIssueAndCache =
      tryAuthUrl(jiraUrl(s"/issue/SI-${i}?expand=changelog")).get().map { resp =>
        val contents = readFully(resp.ahcResponse.getResponseBodyAsStream)
        try Some(parseIssue(contents)) // don't bother writing to disk if it doesn't parse
        finally {
          val out = new FileOutputStream(fileFor(i))
          try out.write(contents, 0, contents.size)
          finally { out.flush(); out.close() }
          println("YEP: " + i)
        }
      }

    loadCachedIssue recoverWith {
      case _ => // if we couldn't load from disk, ask jira thrice
        def download(retries: Int): Future[Option[Issue]] =
          downloadIssueAndCache recoverWith {
            case e if retries == 0 => throw e
            // don't retry when server says the issue doesn't exist
            case e: NoSuchElementException =>
              println(s"issue does not exist: $i ($e)")
              Future.successful(None)
            case e =>
              println(s"MEH: $i with $e (cause: e.getCause())")
              e.printStackTrace()
              download(retries - 1)
          }
        download(3)
    }
  }

  lazy val issues: Future[IndexedSeq[Issue]] =
    Future.sequence { (1 to 6882).map { getIssue } }.map(_.flatten)

  def parseIssue(data: Array[Byte]): Issue = parseIssue(Json.parse(new String(data)))
  def parseIssue(i: JsValue): Issue =
    optField(i)("errorMessages").map(_.as[List[String]]) match {
      case Some(errorMessages) =>
        if (errorMessages contains "Issue Does Not Exist") throw new NoSuchElementException(errorMessages.mkString("\n"))
        else throw new IllegalArgumentException(errorMessages.mkString("\n"))
      case None =>
        Issue(
          (i \ "key").as[String],
          (i \ "fields").as[Map[String, JsValue]].map { case (k, v) => parseField(k, v) },
          (i \ "changelog" \ "histories").as[List[JsValue]])
    }

  def parseField(field: String, v: JsValue): (String, Any) = (field,
    try field match {
      case "project"           => (v \ "key").as[String] // Project
      case "issuekey"          => v.as[String]
      case "summary"           => v.as[String]
      case "reporter"          => v.as[User]
      case "created"           => v.as[Date]
      case "updated"           => v.as[Date]
      case "issuetype"         => (v \ "name").as[String] // IssueType
      case "priority"          => (v \ "name").as[String] // Priority
      case "status"            => v.as[Status]

      case "assignee"          => v.asOpt[User]
      case "description"       => v.asOpt[String]
      case "environment"       => v.asOpt[String]
      case "resolution"        => v.asOpt[Resolution]
      case "resolutiondate"    => v.asOpt[Date]
      case "duedate"           => v.asOpt[Date]
      case "versions"          => v.as[List[Version]] // affected version
      case "fixVersions"       => v.as[List[Version]]
      case "labels"            => v.as[List[String]]
      case "issuelinks"        => v.as[List[IssueLink]]
      case "components"        => v.as[List[JsObject]].map(c => (c \ "name").as[String])
      case "comment"           => (v \ "comments").as[List[Comment]] // List[Comment]
      case "attachment"        => v.as[List[Attachment]]

      case "votes"             => lazyList[User](v, "votes", "voters") // Future[List[Vote]]
      case "watches"           => lazyList[User](v, "watchCount", "watchers") // Future[List[Watch]]

      case "customfield_10005" => v.asOpt[List[User]] // trac cc
      case "customfield_10101" => v.asOpt[List[String]] // flagged
      case "customfield_10104" => v.asOpt[Float] // Story points
      case "customfield_10105" => v.asOpt[Float] // business value
      case "subtasks" =>
        assert(v.as[List[JsValue]].isEmpty, "subtasks not supported"); v
      case "workratio" => v.asOpt[Float]
    } catch {
      case e: Exception => throw new Exception(s"Error parsing field $field : $v", e)
    })
}

// TODO: scalac bug, why does this have to come after the companion object?
case class Issue(key: String, fields: Map[String, Any], changelog: List[JsValue])