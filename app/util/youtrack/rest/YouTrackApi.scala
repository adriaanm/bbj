package util.youtrack.rest

import play.api.libs.ws.WS
import java.util.Date

case class Project(projectId: String, name: String, description: String, lead: User, startingNumber: Int = 1)
case class User(name: String, displayName: String, emailAddress: Option[String])

case class Version(name: String, description: String, releaseDate: Option[Date], archived: Boolean, released: Boolean)

sealed class IssueLinkType(val name: String, val outward: String, val inward: String, val flipped: Boolean = false) {
  def directed = inward != outward
}
object Relates extends IssueLinkType("Relates", "relates to", "relates to")
case class Duplicates(override val flipped: Boolean) extends IssueLinkType("Duplicate", "is duplicated by", "duplicates", flipped)
case class Blocks(override val flipped: Boolean) extends IssueLinkType("Blocks", "is blocked by", "blocks", flipped)
case class Clones(override val flipped: Boolean) extends IssueLinkType("Cloners", "is cloned by", "clones", flipped)

object Issue {
  def xIssueType(t: Any): String = t match {
    case "Bug"         => "TODO"
    case "Improvement" => "TODO"
    case "Suggestion"  => "TODO"
    case "New Feature" => "TODO"
  }

  def xPriority(t: Any): String = t match {
    case "Blocker"  => "4"
    case "Critical" => "3"
    case "Major"    => "2"
    case "Minor"    => "1"
    case "Trivial"  => "0"
  }

  def xResolution(r: Any): String = r match {
    case "Fixed"                   => "Fixed"
    case "Won't Fix"               => "Won't Fix"
    case "Cannot Reproduce"        => "Can't Reproduce"
    case "Duplicate"               => "Duplicate"
    case "Incomplete"              => "Incomplete"
    case "Fixed, Backport Pending" => "TODO"
    case "Out of Scope"            => "TODO"
    case "Not a Bug"               => "TODO"
  }

}

case class Issue(key: String, fields: Map[String, Any]) {
  import Issue._

  def keyNoProject = key.split("-")(1)
  /*       
      case "project"           => (v \ "key").as[String] // Project
      case "issuekey"          => v.as[String]
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
      case "issuelinks"        => v.as[List[IssueLink]]
      case "components"        => v.as[List[JsObject]].map(c => componentToLabel((c \ "name").as[String]))
      case "comment"           => (v \ "comments").as[List[Comment]] // List[Comment]
      case "attachment"        => v.as[List[Attachment]]

      case "votes"             => lazyList[User](v, "votes", "voters") // Future[List[Vote]]
      case "watches"           => lazyList[User](v, "watchCount", "watchers") // Future[List[Watch]]

      case "customfield_10005" => v.asOpt[List[User]] // trac cc
    */
  def f(n: String) = <value>{ fields(n) }</value>
  def df(n: String) = <value>{ fields(n).asInstanceOf[Date].getTime }</value>
  def uf(n: String) = <value>{ fields(n).asInstanceOf[User].name }</value>
  def x(n: String, f: Any => String) = <value>{ f(fields(n)) }</value>
  def fieldXml(name: String) = <field name={ name }>{
    name match {
      case "numberInProject" => <value>keyNoProject</value> // long    single    required    Issue number in project. Should be unique
      case "summary"         => f(name) // string    single    required    Issue summary
      case "description"     => f(name) // string    single    optional    Full issue description
      case "created"         => df(name) // timestamp   single    required    Issue creation time
      case "updated"         => df(name) // timestamp   single    optional    Last issue update time
      case "updaterName"     => // string    single    optional    Login name of last issue updater
      case "resolved"        => df("resolutiondate") // timestamp    single    optional    Issue resolve time
      case "reporterName"    => uf("reporter") // string    single    required    Login name of issue reporter
      case "assigneeName"    => uf("assignee") // string    single    optional    Login name of issue assignee. added to assigneeGroup if necessary
      case "type"            => x("issuetype", xIssueType) // string    single    required    Issue type name. See Get Issue Types
      case "priority"        => x("priority", xPriority) // integer   single    required    Issue priority ordinal. See Get Issue Priorities
      case "state" => // string    single    required    Issue state name. See Get Issue States
        if (fields("status") == "Open") <value>Open</value>
        else x("resolution", xResolution)
      case "subsystem"      => // string    single    optional    Issue subsystem name. See GET Subsystems
      case "affectsVersion" => // string    multi   optional    Names of versions affected by the issue. See GET Versions
      case "fixedVersion"   => // string    multi   optional    Names of versions where the issue is fixed. See GET Versions
      case "voterName"      => // string    multi   optional    Login names of users who voted for the issue
      case "fixedInBuild"   => // string    single    optional    Name of project build where the issue is fixed. Created if necessary.

    }
  }</field>
  def toXml = <issue></issue>
}

case class IssueLink(kind: IssueLinkType, outwardIssue: Option[String], inwardIssue: Option[String]) {
  def toXml = <link></link>
}

trait Connection {
  implicit lazy val defaultContext: scala.concurrent.ExecutionContext = play.api.libs.concurrent.Execution.Implicits.defaultContext

  def getString(x: String) = play.api.Play.current.configuration.getString(x)

  lazy val user = getString("youtrack.user")
  lazy val pass = getString("youtrack.password")

  // try to authorize, or fall back to non-auth
  // need to authorize to get voters & watchers, apparently...
  def tryAuthUrl(url: String) = {
    val req = WS.url(url)
    if (user.isEmpty || pass.isEmpty) req
    else req.withAuth(user.get, pass.get, com.ning.http.client.Realm.AuthScheme.BASIC)
  }

  def issueUrl(issueId: String) =
    s"http://localhost:8080/issue/$issueId"

  def url(sub: String) =
    s"http://localhost:8080/$sub"

  def executeCommand(issueId: String, command: String, params: (String, String)*) =
    tryAuthUrl(issueUrl(issueId) + "/execute").withQueryString(params: _*).post("")

  def createProject(p: Project) =
    tryAuthUrl(url("admin/project") + p.projectId).withQueryString(
      ("projectName", p.name),
      ("description", p.description),
      ("projectLeadLogin", p.lead.name),
      ("lead", p.lead.name),
      ("startingNumber", p.startingNumber.toString)).put(<empty/>)

  def createUsers(users: Seq[User]) =
    tryAuthUrl(url("import/users")).put(<list>
                                          {
                                            users.map {
                                              case User(id, dn, None)=> <user login={ id } fullName={ dn }/>
                                              case User(id, dn, Some(e))=> <user login={ id } fullName={ dn } email={ e }/>
                                            }
                                          }
                                        </list>)

  def createVersion(projectId: String, v: Version) =
    tryAuthUrl(url(s"admin/project/$projectId/version/${v.name}")).withQueryString(
      ("description", v.description),
      ("isReleased", v.released.toString),
      ("isArchived", v.archived.toString)).put(<empty/>)
  //  createAttachmentFromAttachment
  def createIssueLinkType(ilt: IssueLinkType) =
    tryAuthUrl(url("admin/issueLinkType/") + ilt.name).withQueryString(
      ("outwardName", ilt.outward),
      ("inwardName", ilt.inward),
      ("directed", ilt.directed.toString)).put(<empty/>)

  def importIssues(projectId: String, issues: Seq[Issue]) =
    tryAuthUrl(url(s"import/$projectId/issues")).put(<issues>{ issues map (_.toXml) }</issues>)

  def importLinks(links: Seq[IssueLink]) =
    tryAuthUrl(url("import/links")).put(<list>{ links map (_.toXml) }</list>)

  //  addValueToBundle
  //  getBundle
  //  createCustomFieldDetailed
  //  createProjectCustomFieldDetailed
  //  getCustomFields
  //  getProjectCustomField
  //  getProjectCustomFields
}