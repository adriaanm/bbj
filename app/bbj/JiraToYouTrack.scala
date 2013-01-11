package bbj
import java.util.Date

/** 
 * new play.core.StaticApplication(new java.io.File("."))
import bbj._
import migrate._
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.concurrent.Await.result
import issueTranslation.jiraToYouTrack

def getIssues(from: Int, to: Int) = result(Future.sequence { (from to to).map { jira.getIssue } }.map(_.flatten), Duration.Inf)

val translated = getIssues(1, 500).map(jiraToYouTrack.mapOver).asInstanceOf[Seq[youTrack.issues.Issue]]

translated.flatMap(_.fields("fixedVersion").asInstanceOf[List[youTrack.issues.Version]])
res12 map (v => youTrack.createVersion("SI", v))

result(youTrack.importIssues("SI", translated), Duration.Inf).ahcResponse.getResponseBody


val allIssues = result(jira.allIssues, Duration.Inf)
val allFieldValues = allIssues.flatMap(_.fields).groupBy(_._1).map{case (k, vs) => (k, vs.map(_._2).distinct)}
allFieldValues.keys

allFieldValues("labels").asInstanceOf[Vector[List[String]]].flatten.distinct
 */
object migrate {
  object issueTranslation extends TranslateJiraToYouTrack

  val jira = new JiraConnection { val issues = issueTranslation }
  val youTrack = new YouTrackConnection { val issues = issueTranslation }

  val addToStatesBundle = List("Backport Pending", "Not a Bug")
  /*def create_value(target, value, field_name, field_type, project_id):
    if field_type.startswith('user'):
        create_user(target, value)
        value['name'] = value['name'].replace(' ', '_')
    if field_name in jira.EXISTING_FIELDS:
        return
    if field_name.lower() not in [field.name.lower() for field in target.getProjectCustomFields(project_id)]:
        if field_name.lower() not in [field.name.lower() for field in target.getCustomFields()]:
            target.createCustomFieldDetailed(field_name, field_type, False, True, False, {})
        if field_type in ['string', 'date', 'integer']:
            target.createProjectCustomFieldDetailed(project_id, field_name, "No " + field_name)
        else:
            bundle_name = field_name + " bundle"
            create_bundle_safe(target, bundle_name, field_type)
            target.createProjectCustomFieldDetailed(project_id, field_name, "No " + field_name, {'bundle': bundle_name})
    if field_type in ['string', 'date', 'integer']:
        return
    project_field = target.getProjectCustomField(project_id, field_name)
    bundle = target.getBundle(field_type, project_field.bundle)
    try:
        if 'name' in value:
            target.addValueToBundle(bundle, value['name'])
        elif 'value' in value:
            target.addValueToBundle(bundle, value['value'])
    except YouTrackException:
        pass*/
  import concurrent.ExecutionContext.Implicits.global

  val allFieldValues = for (issues <- jira.allIssues) yield issues.flatMap(_.fields).groupBy(_._1).map { case (k, vs) => (k, vs.map(_._2).distinct) }
  def createFieldTypes = {
    def createFieldType(fieldName: String, possibleValues: Iterable[Any]) = fieldName match {
      // new
      case "votes"             => // TODO
      case "watches"           => // TODO
      case "environment"       => // TODO
      case "status"            => // needs new value: "Not a Bug"
      case "fixVersions"       => // populate with values
      case "versions"          => // populate with values
      
      // builtin
      case "assignee"          =>
      case "created"           =>
      case "summary"           =>
      case "description"       =>
      case "issuetype"         =>
      case "priority"          =>
      case "reporter"          =>
      case "resolution"        =>
      case "resolutiondate"    =>
      case "updated"           =>

      // separate
      case "attachment"        =>
      case "comment"           =>
      case "components"        =>
      case "customfield_10005" =>
      case "issuelinks"        =>
      case "labels"            =>
      case "project"           =>
        
      // unused
      case "customfield_10101" =>
      case "subtasks"          =>
      case "duedate"           =>
      case "workratio"         =>
    }
  }
}

trait TranslateJiraToYouTrack extends Issues {

  // KEY: issuetype has VALUES: Vector(Bug, Improvement, Suggestion, New Feature)
  def xType(t: String): String = t match {
    case "Bug"         => "Bug"
    case "Improvement" => "Feature"
    case "Suggestion"  => "Feature"
    case "New Feature" => "Feature"
  }

  // KEY: priority has VALUES: Vector(Critical, Major, Minor, Blocker, Trivial)  
  def xPriority(t: String): Int = t match {
    case "Blocker"  => 4
    case "Critical" => 3
    case "Major"    => 2
    case "Minor"    => 1
    case "Trivial"  => 0
  }

  // KEY: resolution has VALUES: Vector(Some(Fixed), Some(Not a Bug), None, Some(Won't Fix), Some(Cannot Reproduce), Some(Duplicate), Some(Out of Scope), Some(Incomplete), Some(Fixed, Backport Pending))
  // KEY: status has VALUES: Vector(Closed, Open)
  // convert resolution to state (youtrack does not have both status and resolution)
  def xState(r: Option[String]): String = r match {
    case None                            => "Open"
    case Some("Fixed")                   => "Fixed"
    case Some("Won't Fix")               => "Won't Fix"
    case Some("Cannot Reproduce")        => "Can't Reproduce"
    case Some("Duplicate")               => "Duplicate"
    case Some("Incomplete")              => "Incomplete"
    case Some("Out of Scope")            => "Won't Fix"
    case Some("Fixed, Backport Pending") => "To Backport"
    case Some("Not a Bug")               => "Invalid"
  }

  // TODO
  def xComponent(c: String): String = c match {
    case "Scaladoc Tool"          => "Scaladoc Tool"
    case "Misc Compiler"          => "Misc Compiler"
    case "Misc Library"           => "Misc Library"
    case "Specification"          => "Specification"
    case "Eclipse Plugin (EOL)"   => "Eclipse Plugin (EOL)"
    case "Packaging"              => "Packaging"
    case "Documentation and API"  => "Documentation and API"
    case "Repl / Interpreter"     => "Repl / Interpreter"
    case "Pattern Matcher"        => "Pattern Matcher"
    case "Build, Developer Tools" => "Build, Developer Tools"
    case "XML Support"            => "XML Support"
    case "Jira"                   => "Jira"
    case "Website"                => "Website"
    case "Actors Library"         => "Actors Library"
    case "MSIL Backend"           => "MSIL Backend"
    case "Parser Combinators"     => "Parser Combinators"
    case "Enumeration"            => "Enumeration"
    case "Type Checker"           => "Type Checker"
    case "Swing Library"          => "Swing Library"
    case "Continuations"          => "Continuations"
    case "Collections"            => "Collections"
    case "Specialization"         => "Specialization"
    case "Type Inference"         => "Type Inference"
    case "Reflection"             => "Reflection"
    case "Optimizer"              => "Optimizer"
    case "Compiler Backend"       => "Compiler Backend"
    case "Presentation Compiler"  => "Presentation Compiler"
    case "Macros"                 => "Macros"
    case "Concurrent Library"     => "Concurrent Library"
    case "Partest"                => "Partest"
  }

  // TODO: determine unusedField dynamically
  //  scala> val allFieldValues = issues.flatMap(_.fields).groupBy(_._1).map{case (k, vs) => (k, vs.map(_._2).distinct)}
  //  scala> allFieldValues.filter(_._2.size <= 1).keys
  //    res45: Iterable[String] = Set(duedate, customfield_10101, subtasks, workratio)
  val unusedField = Set("duedate", "customfield_10101", "subtasks", "workratio")
  val separateField = Set("status", "project", "labels", "issuelinks", "components", "attachment", "customfield_10005")

  // Set(priority, assignee, votes, issuelinks, reporter, project, description, versions, duedate, fixVersions, customfield_10101, components, 
  // resolutiondate, watches, updated, subtasks, labels, resolution, status, comment, environment, attachment, customfield_10005, issuetype, summary, workratio, created)
  def xFieldName(n: String): Option[String] = n match {
    case n if unusedField(n) | separateField(n) => None // not used or treated separately

    case "issuekey"                             => Some("numberInProject")
    case "summary"                              => Some(n)
    case "reporter"                             => Some("reporterName")
    case "created"                              => Some(n)
    case "updated"                              => Some(n)
    case "issuetype"                            => Some("type")
    case "priority"                             => Some(n)

    case "assignee"                             => Some("assigneeName")
    case "description"                          => Some(n)
    case "environment"                          => None // TODO: Some(n) // custom
    case "resolution"                           => Some("state")
    case "resolutiondate"                       => Some("resolved")
    case "versions"                             => Some("affectsVersion")
    case "fixVersions"                          => Some("fixedVersion")
    case "comment"                              => Some("comment")

    case "votes"                                => None // TODO: Some("voterName")
    case "watches"                              => None // TODO: Some("watcherName") // also includes customfield_10005

    case _                                      => println(s"IGNORING FIELD $n"); None
  }

  def xField(n: String, v: Any) = {
    xFieldName(n) map (n => (n, (n, v) match {
      case ("numberInProject", n: String) => n.split("-")(1)
      case ("type", t: String)            => xType(t)
      case ("priority", p: String)        => xPriority(p)
      case ("state", r: Option[String])   => xState(r)
      case (_, j)                         => jiraToYouTrack.mapOver(j)
    }))
  }

  def validUserName(name: String) = !name.contains(" ")
  def cleanUserName(name: String) = name.replace(" ", "_")
  val BOGUS_EMAIL = "bogus@empty.in.jira"

  // TODO: is the user transform done consistently?
  object jiraToYouTrack extends IssuesTransform {
    def apply(x: User): User = x match {
      case User(id, dn, Some(em)) if validUserName(id) => x
      case User(id, dn, em)                            => User(cleanUserName(id), dn, em orElse Some(BOGUS_EMAIL))(x.groups)
    }
    def apply(x: Version): Version = x
    def apply(x: IssueLinkType): IssueLinkType = x

    override def mapOver(x: Any): Any =
      x match {
        //        case Comment(a, b, ua, c, u)         => Comment(apply(a), b, apply(ua), c, u)
        //        case Attachment(f, a, c, d, s, m, p) => Attachment(f, apply(a), c, d, s, m, p)
        //        case IssueLink(ilt, o, i)            => IssueLink(apply(ilt), o, i)
        case Issue(key, fields) => Issue(key, fields flatMap { case (n, v) => xField(n, v) })
        case x                  => super.mapOver(x)
      }

  }

  /*
   allFieldValues("labels").asInstanceOf[Vector[List[String]]].flatten.distinct
res42: scala.collection.immutable.Vector[String] = Vector(postponed, partialfunction, java-sandbox, depmet, gadt, pattern-matching, type-inference, scaladoc-links, scaladoc, community, spec, string-interpolation, .NET, parametric-extractor, structural-types, xml, existential, error-messages, minimized, parser, tcpoly, soundness, performance, usability, outer-references, switch, java-reflection, should-not-compile, has-pull-request, tailrec, compiler-api, needs_discussion, case-class, grand-challenge, compiler-crash, name-mangling, lub, java-interop, annotations, named-default-args, backport, try-catch, emacs, typetag, varargs, collections, inner-class, scaladoc-types, implicit, binary-compatibility, scalap, runtime-crash, lazy-val, scaladoc-usecases, dependent-types, optimizer, implicit...
scala> allFieldValues("labels").asInstanceOf[Vector[List[String]]].flatten.distinct.size
res43: Int = 181
   * 
   * */

}
