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

/* to experiment from the play console:

  new play.core.StaticApplication(new java.io.File("."))
  import bbj._
  object jira extends JiraConnection { val issues = new Issues {} }
  import jira.issues._
  val issues = concurrent.Await.result(jira.allIssues, concurrent.duration.Duration.Inf)
  issues.flatMap(_.fields("issuelinks").asInstanceOf[List[IssueLink]])

*/
trait JiraConnection extends JsonConnection {
  lazy val user = getString("jira.user")
  lazy val pass = getString("jira.password")

  val issues: Issues
  import issues._

  implicit object readUser extends Reads[User] {
    def reads(json: JsValue): JsResult[User] = validate("user")(for (
      self <- self(json);
      name <- name(json);
      emailAddress <- (optField(json)("emailAddress")).map(_.asOpt[String]).orElse(Some(None));  // don't get this field when reading voters/watchers from a lazy list
      displayName <- (json \ "displayName").asOpt[String]
    ) yield User(self, name, displayName, emailAddress))
  }

  private def cleanEmail(email: String): Option[String] = email match {
    case "non-valid@e-mail.null" => None
    case ""                      => None
    case e                       => Some(e)
  }

  private val users = collection.mutable.HashMap[String, User]()
  def User(self: String, name: String, displayName: String, emailAddress: Option[String]) =
    users.synchronized {
      users.getOrElseUpdate(self, new User(name, displayName, emailAddress.flatMap(cleanEmail))(
          tryAuthUrl(self + "&expand=groups").get().map { req => (req.json \ "groups" \\ "name").map(_.as[String]).toList }))
    }

  def allUsers = users.values

  /*
issues.flatMap(_.fields("versions").asInstanceOf[List[Version]]).toSet
Set(Scala 2.10.0-M4, Scala 2.8.1, Scala 2.10.0-M7, Scala 2.9.2, Scala 2.10.0, Scala 2.11.0, Scala 2.10.0-M3, Scala 2.10.0-RC5, Scala 2.9.0,
    Scala 2.9.1, Scala 2.10.0-M5, Scala 2.9.3-RC1, Scala 2.10.0-RC2, Scala 2.7.7, Scala 2.9.0-1, Scala 2.10.0-M6, Scala 2.10.1-RC1, Scala 2.10.0-M2, Scala 2.10.0-M1, Scala 2.8.0, macro-paradise, Scala 2.11.0-M1, Scala 2.10.0-RC1, Scala 2.10.0-RC3)
 */
  implicit object readVersion extends Reads[Version] {
    def reads(json: JsValue): JsResult[Version] = validate(s"version; got $json")(for (
      self <- self(json);
      name <- name(json);
      description <- (optField(json)("description")).map(_.asOpt[String]).orElse(Some(None));
      userReleaseDate <- (optField(json)("userReleaseDate")).map(_.asOpt[String]).orElse(Some(None));
      releaseDate <- (optField(json)("releaseDate")).map(_.asOpt[Date](Reads.DefaultDateReads)).orElse(Some(None));
      archived <- (json \ "archived").asOpt[Boolean];
      released <- (json \ "released").asOpt[Boolean]
    ) yield Version(self, name, description, userReleaseDate, releaseDate, archived, released))
  }

  private def parseUserReleaseDate(d: String): Option[Date] = {
    val df = new java.text.SimpleDateFormat("d/MMM/yy")
    df.setLenient(false)
    try { Some(df.parse(d)) } catch {
      case _: java.text.ParseException => None
    }
  }

  private val versions = collection.mutable.HashMap[String, Version]()
  def Version(self: String, name: String, description: Option[String], userReleaseDate: Option[String], releaseDate: Option[Date], archived: Boolean, released: Boolean) =
    versions.synchronized { versions.getOrElseUpdate(self, new Version(name, description, releaseDate orElse userReleaseDate.flatMap(parseUserReleaseDate), archived, released)) }

  def allVersions = versions.values

  implicit object readComment extends Reads[Comment] {
    def reads(json: JsValue): JsResult[Comment] = validate(s"comment; got $json")(for (
      author <- (json \ "author").asOpt[User];
      body <- (json \ "body").asOpt[String];
      updateAuthor <- (json \ "updateAuthor").asOpt[User];
      created <- (json \ "created").asOpt[Date];
      updated <- (json \ "updated").asOpt[Date]
    ) yield Comment(author, body, updateAuthor, created, updated))
  }

  implicit object readAttachment extends Reads[Attachment] {
    def reads(json: JsValue): JsResult[Attachment] = validate(s"attachment; got $json")(for (
      filename <- (json \ "filename").asOpt[String];
      author <- (json \ "author").asOpt[User];
      created <- (json \ "created").asOpt[Date];
      size <- (json \ "size").asOpt[Int];
      mimeType <- (json \ "mimeType").asOpt[String];
      properties <- (optField(json)("properties")).map(_.asOpt[JsObject]).orElse(Some(None));
      content <- (json \ "content").asOpt[String]
    ) yield Attachment(filename, author, created, content, size, mimeType, properties map (_.value.toMap) getOrElse Map()))
  }

  /** scala> issues.flatMap(_.fields("issuelinks").asInstanceOf[List[IssueLink]].map(_.name)).distinct
   *  Vector(Relates, Duplicate, Blocks, Cloners)
   *
   *  scala> issues.flatMap(_.fields("issuelinks").asInstanceOf[List[IssueLink]].map(_.inward)).distinct
   *  Vector(relates to, is duplicated by, is blocked by, is cloned by)
   *
   *  scala> issues.flatMap(_.fields("issuelinks").asInstanceOf[List[IssueLink]].map(_.outward)).distinct
   *  Vector(relates to, duplicates, blocks, clones)
   *
   */
  def readIssueLink(selfKey: String): Reads[IssueLink] = new Reads[IssueLink] {
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
    ) yield {
      val ilt = IssueLinkType(name, inward, outward)
      (inwardIssue, outwardIssue) match {
        case (Some(i), None) => IssueLink(ilt, i, selfKey) // read as: s"$i ${ilt.name} $selfKey"
        case (None, Some(o)) => IssueLink(ilt, selfKey, o) // read as: s"$selfKey ${ilt.name} $o"
      }
    })
  }

  def IssueLinkType(name: String, inward: String, outward: String) =
    (name, inward, outward) match {
      case ("Relates", "relates to", "relates to")         => Relates
      case ("Duplicate", "is duplicated by", "duplicates") => Duplicates
      case ("Blocks", "is blocked by", "blocks")           => Blocks
      case ("Cloners", "is cloned by", "clones")           => Clones
    }

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
    val cacheDir = "/Users/adriaan/jira"
    assert(new File(cacheDir).isDirectory(), s"Please create cache directory $cacheDir to avoid hammering the jira server.")
    cacheDir
  }

  private def fileFor(key: Int) = new File(s"$cacheDir/${key}.json")
  private def jiraUrl(uri: String) = "https://issues.scala-lang.org/rest/api/latest" + uri

  
  def getIssue(projectId: String, i: Int): Future[Option[Issue]] = {
    def loadCachedIssue =
      Future(Some {
        val f = fileFor(i)
        val data = new Array[Byte](f.length.asInstanceOf[Int])
        (new DataInputStream(new FileInputStream(f))).readFully(data)
        parseIssue(data)
      })

    def downloadIssueAndCache =
      tryAuthUrl(jiraUrl(s"/issue/$projectId-${i}?expand=changelog")).get().map { resp =>
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
              println(s"MEH: $i with $e (cause: ${e.getCause()})")
              e.printStackTrace()
              download(retries - 1)
          }
        download(3)
    }
  }

//  lazy val allIssues: Future[IndexedSeq[Issue]] =
//    Future.sequence { (1 to lastIssue).map { getIssue(projectId, _) } }.map(_.flatten)

  def parseIssue(data: Array[Byte]): Issue = parseIssue(Json.parse(new String(data)))
  def parseIssue(i: JsValue): Issue =
    optField(i)("errorMessages").map(_.as[List[String]]) match {
      case Some(errorMessages) =>
        if (errorMessages contains "Issue Does Not Exist") throw new NoSuchElementException(errorMessages.mkString("\n"))
        else throw new IllegalArgumentException(errorMessages.mkString("\n"))
      case None =>
        val selfKey = (i \ "key").as[String]
        Issue(
          selfKey,
          (i \ "fields").as[Map[String, JsValue]].map { case (k, v) => parseField(selfKey, k, v) } + ("issuekey" -> selfKey)) // , (i \ "changelog" \ "histories").as[List[JsValue]])
    }

  def parseField(selfKey: String, field: String, v: JsValue): (String, Any) = (field,
    try field match {
      case "project"           => (v \ "key").as[String] // Project
      case "issuekey"          => v.as[String] // not normally parsed -- overridden in parseIssue
      case "summary"           => v.as[String]
      case "reporter"          => v.as[User]
      case "created"           => v.as[Date]
      case "updated"           => v.as[Date]
      case "issuetype"         => (v \ "name").as[String] // IssueType: (Bug, Improvement, Suggestion, New Feature)
      case "priority"          => (v \ "name").as[String] // Priority: (Critical, Major, Minor, Blocker, Trivial)
      case "status"            => (v \ "name").as[String] // Status: (Open, Closed)

      case "assignee"          => v.asOpt[User]
      case "description"       => v.asOpt[String]
      case "environment"       => v.asOpt[String] // TODO: extract labels -- this field is extremely messy
      case "resolution"        => optField(v)("name").map(_.as[String]) // "Fixed", "Not a Bug", "Won't Fix", "Cannot Reproduce", "Duplicate", "Out of Scope", "Incomplete", "Fixed, Backport Pending"
      case "resolutiondate"    => v.asOpt[Date]
      case "duedate"           => v.asOpt[Date]
      case "versions"          => v.as[List[Version]] // affected version
      case "fixVersions"       => v.as[List[Version]]
      case "labels"            => v.as[List[String]]
      case "issuelinks"        =>
        implicit val readIL = readIssueLink(selfKey); v.as[List[IssueLink]]
      case "components"        => v.as[List[JsObject]].map(c => (c \ "name").as[String])
      case "comment"           => (v \ "comments").as[List[Comment]] // List[Comment]
      case "attachment"        => v.as[List[Attachment]]

      case "votes"             => lazyList[User](v, "votes", "voters") // Future[List[User]]
      case "watches"           => lazyList[User](v, "watchCount", "watchers") // Future[List[User]]

      case "customfield_10005" => v.asOpt[List[User]] // trac cc
      case "customfield_10101" => v.asOpt[List[String]] // flagged -- never set
      //      case "customfield_10104" => v.asOpt[Float] // Story points
      //      case "customfield_10105" => v.asOpt[Float] // business value
      case "subtasks" =>
        assert(v.as[List[JsValue]].isEmpty, "subtasks not supported"); v
      case "workratio" => v.asOpt[Float] // always -1
    } catch {
      case e: Exception => throw new Exception(s"Error parsing field $field : $v", e)
    })
}
