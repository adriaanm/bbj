package bbj
import java.util.Date
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import play.api.http.ContentTypeOf
import play.api.http.Writeable

trait YouTrackConnection extends JsonConnection {
  lazy val user = getString("youtrack.user")
  lazy val pass = getString("youtrack.password")

  val issues: Issues
  import issues._

  def warning(s: String) = println(s)

  def url(sub: String) = new java.net.URI("http", null, "localhost", 8080, s"/rest/$sub", null, null).toString

  import play.api.mvc.Codec
  implicit def contentTypeOf_Xml(implicit codec: Codec): ContentTypeOf[xml.Elem] = {
    ContentTypeOf[xml.Elem](Some("application/xml; charset=" + codec.charset))
  }
  implicit def writeableOf_Xml(implicit codec: Codec): Writeable[xml.Elem] = {
    Writeable[xml.Elem](xml => codec.encode(xml.toString))
  }

  def executeCommand(issueId: String, command: String, params: (String, String)*) =
    tryAuthUrl(url(s"issue/$issueId/execute")).withQueryString(params: _*).post("")

  def createProject(p: Project) =
    tryAuthUrl(url("admin/project/") + p.projectId).withQueryString(
      ("projectName", p.name),
      ("description", p.description),
      ("projectLeadLogin", p.lead),
      ("lead", p.lead),
      ("startingNumber", p.startingNumber.toString)).put(<empty/>)

  def createVersion(projectId: String, v: Version) =
    tryAuthUrl(url(s"admin/project/$projectId/version/${v.name}")).withQueryString(
      ("description", v.description.getOrElse("")),
      ("releaseDate", v.releaseDate.map(dateYT).getOrElse("")),
      ("isReleased", v.released.toString),
      ("isArchived", v.archived.toString)).put(<empty/>)

  def createIssueLinkType(ilt: IssueLinkType) =
    tryAuthUrl(url("admin/issueLinkType/${ilt.name}")).withQueryString(
      ("outwardName", ilt.outward),
      ("inwardName", ilt.inward),
      ("directed", ilt.directed.toString)).put(<empty/>)

  object field {
    def params(typeName: String, isPrivate: Boolean, defaultVisibility: Boolean, autoAttached: Boolean, emptyFieldText: String, params: (String, String)*) =
      (params.toMap ++ Map(
        ("type", typeName),
        ("isPrivate", isPrivate.toString),
        ("defaultVisibility", defaultVisibility.toString),
        ("autoAttached", autoAttached.toString),
        ("emptyFieldText", emptyFieldText))).toSeq
  }

  // ignores kindPrefix if projectId is specified
  def customFieldUrl(projectId: Option[String], kindPrefix: String = "") =
    s"""admin${projectId map ("/project/" + _) getOrElse ""}/customfield${if (projectId.isEmpty) kindPrefix else ""}"""

  // kind = "field" | bundleName
  def createCustomField(projectId: Option[String], kind: String, name: String, params: (String, String)*) =
    tryAuthUrl(url(s"${customFieldUrl(projectId, "/"+kind)}/$name")).withQueryString(params: _*).put(<empty/>)

  def customFields(projectId: Option[String]) =
    tryAuthUrl(url(customFieldUrl(projectId))).get().map(req => (req.xml \\ "projectCustomField" \\ "@name").map(_.toString).toList)

  def usersToXml(users: Iterable[User]): xml.Elem =
    <list>
      {
        users.flatMap {
          case User(id, dn, Some(e))=> <user login={ id } fullName={ dn } email={ e }/>
        }
      }
    </list>

  def importUsers(users: Iterable[User]) =
    tryAuthUrl(url("import/users")).put(usersToXml(users))

  def importIssues(projectId: String, issues: Seq[Issue]) =
    tryAuthUrl(url(s"import/$projectId/issues")).put(<issues>{ issues map issueToXml }</issues>)

  def importLinks(links: Seq[IssueLink]) =
    tryAuthUrl(url("import/links")).put(<list>{ links map issueLinkToXml }</list>)

  //  createAttachmentFromAttachment

  //  addValueToBundle
  //  getBundle
  //  createCustomFieldDetailed
  //  createProjectCustomFieldDetailed
  //  getCustomFields
  //  getProjectCustomField
  //  getProjectCustomFields

  def issueLinkToXml(i: IssueLink): xml.Elem = ??? // <link typeName={i.kind.name} source={i.sourceKey} target={i.targetKey}/>

  // unix epoch time
  def dateYT(d: Date) = d.getTime.toString

  def issueToXml(i: Issue): xml.Elem = {
    val fields = i.fields

    def fieldXml(n: String): xml.NodeSeq = {
      def opt(f: Any => xml.Elem) = fields(n) match {
        case Some(x) => f(x)
        case _       => xml.NodeSeq.Empty
      }
      def f(v: Any = fields(n)) = <value>{ v }</value>
      def df(v: Any = fields(n)) = <value>{ dateYT(v.asInstanceOf[Date]) }</value>
      def uf(v: Any = fields(n)) = <value>{ v.asInstanceOf[User].name }</value>
      def lf(v: Any = fields(n))(f: Any => String): xml.NodeSeq = v.asInstanceOf[List[Any]].map { x =>
        <value>{ f(x) }</value>
      }
      def flf(v: Any = fields(n))(f: Any => String): xml.NodeSeq = Await.result(v.asInstanceOf[Future[List[Any]]], Duration.Inf).map { x =>
        <value>{ f(x) }</value>
      }

      n match {
        case "comment" => fields(n).asInstanceOf[List[Comment]] map {
          case Comment(author, body, _, created, updated) => <comment author={ author.name } text={ body } created={ dateYT(created) } updated={ dateYT(updated) }/>
        }
        // dealt with separately
        case "labels" | "issuelinks" | "attachment" => xml.NodeSeq.Empty
        case _ =>
          <field name={ n }>{
            n match {
              case "numberInProject" => f() //  long        single    required    Issue number in project. Should be unique
              case "summary"         => f() //  string      single    required    Issue summary
              case "reporterName"    => uf() // string      single    required    Login name of issue reporter
              case "created"         => df() // timestamp   single    required    Issue creation time
              case "updated"         => df() // timestamp   single    optional    Last issue update time
              case "type"            => f() //  string      single    required    Issue type name. See Get Issue Types
              case "priority"        => f() //  integer     single    required    Issue priority ordinal. See Get Issue Priorities
              case "state"           => f() //  string      single    required    Issue state name. See Get Issue States

              case "assigneeName"    => opt(uf(_)) // string single   optional    Login name of issue assignee. added to assigneeGroup if necessary
              case "description"     => opt(f(_)) //  string single   optional    Full issue description
              case "environment"     => opt(f(_))
              case "resolved"        => opt(df(_)) // timestamp  single optional  Issue resolve time
              case "due"             => opt(df(_))
              case "affectsVersion"  => lf() { case Version(n, _, _, _, _) => n } // string    multi   optional    Names of versions affected by the issue. See GET Versions
              case "fixedVersion"    => lf() { case Version(n, _, _, _, _) => n } // string    multi   optional    Names of versions where the issue is fixed. See GET Versions
              case "subsystem"       => f() //  string    single    optional    Issue subsystem name. See GET Subsystems

              case "fixedInBuild"    => f() //  string    single    optional    Name of project build where the issue is fixed. Created if necessary.
              case "updaterName"     => f() //  string    single    optional    Login name of last issue updater
              case "voterName"       => flf() { case User(n, _, _) => n } // string    multi   optional    Login names of users who voted for the issue
              case "watcherName"     => flf() { case User(n, _, _) => n }
              case _ =>
                println(s"TODO field: $n = ${fields(n)}")
                xml.NodeSeq.Empty
            }
          }</field>
      }
    }

    <issue>{ fields.keys.flatMap(fieldXml) }</issue>
  }
}