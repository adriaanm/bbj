package util.jira.rest

import play.api.libs.ws.{ WS, Response }
import play.api.libs.json.{ JsValue, Json, JsNull, Reads, JsSuccess, JsError, JsPath, JsResultException }
import play.api.data.validation.ValidationError
import java.util.Date
import play.api.libs.json.JsResult
import scala.concurrent.Future
import scala.concurrent.Await


trait Validation {
  implicit val dateRead = Reads.IsoDateReads

  def validate[T](tp: String)(x: Option[T]): JsResult[T] =
    x.map(JsSuccess apply _) getOrElse JsError(Seq(JsPath() -> Seq(ValidationError("validate.error.expected."+tp))))
}

object User extends Validation {
  implicit object reads extends Reads[User] {
    def reads(json: JsValue): JsResult[User] = validate("user")(for (
      self <- (json \ "self").asOpt[String];
      name <- (json \ "name").asOpt[String]
    ) yield User(self, name))
  }

  private val users = collection.mutable.HashMap[String, User]()
  def apply(self: String, name: String) =
    users.getOrElseUpdate(self, new User(self, name))

}
class User(val self: String, val name: String)

object Status extends Validation {
  implicit object reads extends Reads[Status] {
    def reads(json: JsValue): JsResult[Status] = validate("status")(for (
      self <- (json \ "self").asOpt[String];
      id   <- (json \ "id").asOpt[Int];
      name <- (json \ "name").asOpt[String];
      description <- (json \ "description").asOpt[String]
    ) yield Status(self, id, name, description))
  }

  private val stati = collection.mutable.HashMap[String, Status]()
  def apply(self: String, id: Int, name: String, description: String) =
    stati.getOrElseUpdate(self, new Status(self, id, name, description))
}
class Status(val self: String, val id: Int, val name: String, val description: String)

object Resolution extends Validation {
  implicit object reads extends Reads[Resolution] {
    def reads(json: JsValue): JsResult[Resolution] = validate("resolution")(for (
      self <- (json \ "self").asOpt[String];
      id   <- (json \ "id").asOpt[Int];
      name <- (json \ "name").asOpt[String];
      description <- (json \ "description").asOpt[String]
    ) yield Resolution(self, id, name, description))
  }

  private val resolutions = collection.mutable.HashMap[String, Resolution]()
  def apply(self: String, id: Int, name: String, description: String) =
    resolutions.getOrElseUpdate(self, new Resolution(self, id, name, description))
}
class Resolution(val self: String, val id: Int, val name: String, val description: String)


object Version extends Validation {
  implicit object reads extends Reads[Version] {
    def reads(json: JsValue): JsResult[Version] = validate("version")(for (
      self <- (json \ "self").asOpt[String];
      id   <- (json \ "id").asOpt[Int];
      name <- (json \ "name").asOpt[String];
      userReleaseDate <- (json \ "userReleaseDate").asOpt[String];
      releaseDate <- (json \ "releaseDate").asOpt[Date](Reads.DefaultDateReads);
      archived <- (json \ "archived").asOpt[Boolean];
      released <- (json \ "released").asOpt[Boolean]
    ) yield new Version(self, id, name, userReleaseDate, releaseDate, archived, released))
  }

  private val versions = collection.mutable.HashMap[String, Version]()
  def apply(self: String, id: Int, name: String, userReleaseDate: String, releaseDate: Date, archived: Boolean, released: Boolean) =
    versions.getOrElseUpdate(self, new Version(self, id, name, userReleaseDate, releaseDate, archived, released))
}
class Version(val self: String, val id: Int, val name: String, val userReleaseDate: String, val releaseDate: Date, val archived: Boolean, val released: Boolean)



object Comment extends Validation {
  implicit object reads extends Reads[Comment] {
    def reads(json: JsValue): JsResult[Comment] = validate("comment")(for (
      author <- (json \ "author").asOpt[User];
      body <- (json \ "body").asOpt[String];
      updateAuthor <- (json \ "updateAuthor").asOpt[User];
      created <- (json \ "created").asOpt[Date];
      updated <- (json \ "updated").asOpt[Date]
    ) yield Comment(author, body, updateAuthor, created, updated))
  }
}
case class Comment(author: User, body: String, updateAuthor: User, created: Date, updated: Date)



object api {
  private def jiraUrl(uri: String) = "https://issues.scala-lang.org/rest/api/latest" + uri

  import play.api.libs.concurrent.Execution.Implicits._

  //  val milestones =  WS.url("https://api.github.com/repos/scala/scala/milestones").get().map(_.json.as[Seq[JsValue]].map{case ms => ((ms \ "title").as[String], (ms \ "number"))}.toMap)
  //
  //  
  //  def makeToken(user: String, pass: String) = for (
  //      resp <- WS.url("https://api.github.com/authorizations").withAuth(user, pass, com.ning.http.client.Realm.AuthScheme.BASIC).post(Json.toJson(Map("scopes" -> Json.toJson(Seq(Json.toJson("public_repo"))), "note" -> Json.toJson("gh")))))
  //        yield (resp.json \ "token").as[String]

  def getIssue(key: String): Future[Issue] = {
    WS.url(jiraUrl(s"/issue/$key?expand=changelog")).get().map(req => parseIssue(req.json))
  }

  def parseIssue(i: JsValue): Issue =
    Issue((i \ "key").as[String], (i \ "fields").as[Map[String, JsValue]], (i \ "changelog" \ "histories").as[List[JsValue]])

}

// TODO: scalac bug, why does this have to come after the companion object?
case class Issue(key: String, fields: Map[String, JsValue], changelog: List[JsValue]) {
  def apply(field: String): Any = {
    val v = fields(field)
    try {
      field match {
        case "description"       => v.as[String]
        case "reporter"          => v.as[User]
        case "assignee"          => v.as[User]
        case "environment"       => v.as[String]
        case "status"            => v.as[Status]
        case "resolution"        => v.as[Resolution]
        case "resolutiondate"    => v.as[Date]
        case "created"           => v.as[Date]
        case "updated"           => v.as[Date]
        case "duedate"           => v.as[Date]
        case "versions"          => v.as[List[Version]] // affected version
        case "fixVersions"       => v.as[List[Version]]
        case "issuetype"         => v // IssueType
        case "priority"          => v // Priority
        case "labels"            => v // List[String]
        case "issuelinks"        => v // List[IssueLink]
        case "components"        => v // List[Component]
        case "comment"           => (v \ "comments").as[List[Comment]] // List[Comment]
        case "attachment"        => v // List[Attachment]
        case "issuekey"          => v.as[String]
        case "votes"             => v // List[Vote]
        case "project"           => v // Project
        case "watches"           => v // List[Watch]
        case "customfield_10005" => v // List[User] // trac cc
        case "customfield_10101" => v // List[String] // flagged
        case "customfield_10104" => v // Float // Story points
        case "customfield_10105" => v // Float // business value
        case "subtasks"          => v // List[IssueLink]
        case "workratio"         => v // Float
      }
    } catch {
      case _ : JsResultException => null
    }
  }
}
