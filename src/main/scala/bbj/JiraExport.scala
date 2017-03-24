package bbj
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import play.api.libs.ws.ahc.AhcWSClient
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.concurrent.Await.result


object export {
  implicit val system = ActorSystem("BBJ")
  import system.dispatcher

  implicit val materializer = ActorMaterializer()

  val jira = new JiraConnection {
    val user = Option(System.getenv("jiraUser"))
    val pass = Option(System.getenv("jiraPwd"))
    System.setProperty("jsse.enableSNIExtension", "false")
    // http://stackoverflow.com/questions/7615645/ssl-handshake-alert-unrecognized-name-error-since-upgrade-to-java-1-7-0
    val sslClient: AhcWSClient = AhcWSClient()
  }

  // TODO: not yet fully reliable
  def allIssues = (1 to 10250).grouped(1024) map { batch =>
    try result(Future.sequence(batch map jira.getIssue("SI")), Duration.Inf)
    catch {
      case e => println(s"GAH $e"); Nil
    }
  }

  val labelFreq = {
    val is = allIssues.flatten.flatten.toList
    val labelMap = is.map(i => (i, Labels.fromIssue(i)))
    val labels = is.flatMap(Labels.fromIssue).toSet
    labels.map(l => (l, labelMap.collect{case (i, ls) if ls contains l => i.key}.size)).toList.sortBy(- _._2)
  }


  def apply() = allIssues.toList


  // "summary"           => v.as[String]
  // "description"       => v.asOpt[String]

  // "assignee"          => v.asOpt[User]
  // "reporter"          => v.as[User]
  // "creator"           => v.as[User]

  // "created"           => v.as[Date]
  // "updated"           => v.as[Date]
  // "lastViewed"        => v.asOpt[Date]
  // "resolutiondate"    => v.asOpt[Date]
  // "duedate"           => v.asOpt[Date]

  // "issuetype"         => (v \ "name").as[String] // IssueType: (Bug, Improvement, Suggestion, New Feature)
  // "priority"          => (v \ "name").as[String] // Priority: (Critical, Major, Minor, Blocker, Trivial)
  // "resolution"        => optField(v)("name").map(_.as[String]) // "Fixed", "Not a Bug", "Won't Fix", "Cannot Reproduce", "Duplicate", "Out of Scope", "Incomplete", "Fixed, Backport Pending"
  // "components"        => v.as[List[JsObject]].map(c => (c \ "name").as[String])
  // "labels"            => v.as[List[String]]
  // "environment"       => v.asOpt[String] // TODO: extract labels -- this field is extremely messy

  // "versions"          => v.as[List[Version]] // affected version
  // "fixVersions"       => v.as[List[Version]]


  // "issuelinks"        => implicit val readIL = readIssueLink(selfKey); v.as[List[IssueLink]]
  // "comment"           => (v \ "comments").as[List[Comment]] // List[Comment]
  // "attachment"        => v.as[List[Attachment]]
  // "votes"             => lazyList[User](v, "votes", "voters") // Future[List[User]]
  // "watches"           => lazyList[User](v, "watchCount", "watchers") // Future[List[User]]



  object Labels {
    def fromIssue(issue: Issue): List[String] = {
      issue.issueType.flatMap(Type).toList ++
        issue.priority.flatMap(Priority) ++
        issue.resolution.flatten.flatMap(Resolution) ++
        issue.components.toList.flatten.flatMap(Component) ++
        issue.labels.getOrElse(Nil)
    }

    def Type(t: Any): Option[String] = t match {
      case "Improvement" => Some("improvement")
      case "Suggestion" => Some("feature")
      case "New Feature" => Some("feature")
      case _ => None
    }

    // KEY: priority has VALUES: Vector(Critical, Major, Minor, Blocker, Trivial)
    def Priority(t: Any): Option[String] = t match {
      case "Blocker" => Some("blocker")
      case "Critical" => Some("critical")
//      case "Major" => 2
      case "Minor" => Some("quickfix")
      case "Trivial" => Some("quickfix")
      case _ => None
    }

    def Resolution(r: Any): Option[String] = r match {
      case Some("Fixed") => None
      case Some("Won't Fix") => Some("wontfix")
      case Some("Cannot Reproduce") => Some("needinfo")
      case Some("Duplicate") => Some("duplicate")
      case Some("Incomplete") => Some("needinfo")
      case Some("Out of Scope") => Some("wontfix")
      case Some("Fixed, Backport Pending") => Some("backport")
//      case None => None
//      case Some("Not a Bug") =>
      case _ => None
    }

    def Component(c: String): Option[String] = c match {
      case "Scaladoc Tool" => Some("scaladoc")
      case "Specification" => Some("spec")
      case "Documentation and API" => Some("docs")
      case "Repl / Interpreter" => Some("repl")
      case "Pattern Matcher" => Some("patmat")
      case "Build, Developer Tools" => Some("build")
      case "Enumeration" => Some("enum")
      case "Type Checker" => Some("typer")
      case "Collections" => Some("collections")
      case "Specialization" => Some("specialization")
      case "Type Inference" => Some("infer")
      case "Reflection" => Some("reflection")
      case "Optimizer" => Some("opt")
      case "Compiler Backend" => Some("backend")
      case "Presentation Compiler" => Some("interactive")
      case "Macros" => Some("macros")
      case _ => None
      // case "Misc Compiler" => "Misc Compiler"
      // case "Misc Library" => "Misc Library"
      // case "Eclipse Plugin (EOL)" => "Eclipse Plugin (EOL)"
      // case "Packaging" => "Packaging"
      // case "XML Support" => "XML Support"
      // case "Jira" => "Jira"
      // case "Website" => "Website"
      // case "Actors Library" => "Actors Library"
      // case "MSIL Backend" => "MSIL Backend"
      // case "Parser Combinators" => "Parser Combinators"
      // case "Swing Library" => "Swing Library"
      // case "Continuations" => "Continuations"
      // case "Concurrent Library" => "Concurrent Library"
      // case "Partest" => "Partest"
    }

  }

}
