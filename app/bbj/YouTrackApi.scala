package bbj
import java.util.Date

trait YouTrackConnection extends JsonConnection {
  lazy val user = getString("youtrack.user")
  lazy val pass = getString("youtrack.password")

  val issues: Issues
  import issues._

  def issueUrl(issueId: String) =
    s"http://localhost:8080/issue/$issueId"

  def url(sub: String) =
    s"http://localhost:8080/$sub"

  def executeCommand(issueId: String, command: String, params: (String, String)*) =
    tryAuthUrl(issueUrl(issueId) + "/execute").withQueryString(params: _*).post("")

  def createProject(p: Project) =
    tryAuthUrl(url("admin/project/") + p.projectId).withQueryString(
      ("projectName", p.name),
      ("description", p.description),
      ("projectLeadLogin", p.lead.name),
      ("lead", p.lead.name),
      ("startingNumber", p.startingNumber.toString)).put(<empty/>)

  def createVersion(projectId: String, v: Version) =
    tryAuthUrl(url(s"admin/project/$projectId/version/${v.name}")).withQueryString(
      ("description", v.description.getOrElse("")),
      ("isReleased", v.released.toString),
      ("isArchived", v.archived.toString)).put(<empty/>)

  def createIssueLinkType(ilt: IssueLinkType) =
    tryAuthUrl(url("admin/issueLinkType/") + ilt.name).withQueryString(
      ("outwardName", ilt.outward),
      ("inwardName", ilt.inward),
      ("directed", ilt.directed.toString)).put(<empty/>)

  def importUsers(users: Seq[User]) =
    tryAuthUrl(url("import/users")).put(
      <list>
        {
          users.map {
            case User(id, dn, None)=> <user login={ id } fullName={ dn }/>
            case User(id, dn, Some(e))=> <user login={ id } fullName={ dn } email={ e }/>
          }
        }
      </list>)

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
      def commentToXml(c: Comment) =
               <comment author={ c.author.name } text={ c.body } created={ dateYT(c.created) } updated={ dateYT(c.updated) }/>

      def opt(f: Any => xml.Elem) = fields(n) match {
        case Some(x) => f(x)
        case _ => xml.NodeSeq.Empty
      }
      def f(v: Any = fields(n))  = <value>{ v }</value>
      def df(v: Any = fields(n)) = <value>{ dateYT(v.asInstanceOf[Date]) }</value>
      def uf(v: Any = fields(n)) = <value>{ v.asInstanceOf[User].name }</value>
      def lf(v: Any = fields(n))(f: Any => String): xml.NodeSeq = v.asInstanceOf[List[Any]].map{ x =>
               <value>{ f(x) }</value> }

      <field name={ n }>{
        n match {
          case "numberInProject" => f() // long    single    required    Issue number in project. Should be unique
          case "summary"         => f() // string    single    required    Issue summary
          case "reporterName"    => uf() // string    single    required    Login name of issue reporter
          case "created"         => df() // timestamp   single    required    Issue creation time
          case "updated"         => df() // timestamp   single    optional    Last issue update time
          case "type"            => f() // string    single    required    Issue type name. See Get Issue Types
          case "priority"        => f() // integer   single    required    Issue priority ordinal. See Get Issue Priorities
          case "state"           => f() // string    single    required    Issue state name. See Get Issue States

          case "assigneeName"    => opt(uf(_)) // string    single    optional    Login name of issue assignee. added to assigneeGroup if necessary
          case "description"     => opt(f(_)) // string    single    optional    Full issue description
          case "environment"     => opt(f(_))
          case "resolved"        => opt(df(_)) // timestamp    single    optional    Issue resolve time
          case "due"             => opt(df(_))
          case "affectsVersion"  => lf(){ case Version(n, _, _, _, _) => n } // string    multi   optional    Names of versions affected by the issue. See GET Versions
          case "fixedVersion"    => lf(){ case Version(n, _, _, _, _) => n } // string    multi   optional    Names of versions where the issue is fixed. See GET Versions
          case "subsystem"       => f() // string    single    optional    Issue subsystem name. See GET Subsystems
          case "comment"         => fields(n).asInstanceOf[List[Comment]] map commentToXml

//          case "voterName"       => lf { case User(n, _, _) => n } // string    multi   optional    Login names of users who voted for the issue
//          case "watcherName"     => lf { case User(n, _, _) => n }
          case "fixedInBuild"    => f() // string    single    optional    Name of project build where the issue is fixed. Created if necessary.
          case "updaterName"     => f() // string    single    optional    Login name of last issue updater
          case "labels" | "issuelinks" | "attachment" => xml.NodeSeq.Empty
          case _ =>
            println(s"TODO field: $n = ${fields(n)}")
            xml.NodeSeq.Empty
        }
      }</field>
    }

    <issue>{ fields.keys.flatMap(fieldXml) }</issue>
  }
}