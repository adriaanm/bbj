package bbj
import java.util.Date

object migrate {
  object issueTranslation extends TranslateJiraToYouTrack

  val jira = new JiraConnection { val issues = issueTranslation }
  val youTrack = new YouTrackConnection { val issues = issueTranslation }
}

trait TranslateJiraToYouTrack extends Issues {
  def xType(t: String): String = t match {
    case "Bug"         => "Bug"
    case "Improvement" => "Feature"
    case "Suggestion"  => "Feature"
    case "New Feature" => "Feature"
  }

  def xPriority(t: String): Int = t match {
    case "Blocker"  => 4
    case "Critical" => 3
    case "Major"    => 2
    case "Minor"    => 1
    case "Trivial"  => 0
  }

  // convert resolution to state (youtrack does not have both status and resolution)
  def xState(r: Option[String]): String = r match {
    case None                            => "Open"
    case Some("Fixed")                   => "Fixed"
    case Some("Won't Fix")               => "Won't Fix"
    case Some("Cannot Reproduce")        => "Can't Reproduce"
    case Some("Duplicate")               => "Duplicate"
    case Some("Incomplete")              => "Incomplete"
    case Some("Out of Scope")            => "Won't Fix"
    case Some("Fixed, Backport Pending") => "Backport Pending"
    case Some("Not a Bug")               => "Not a Bug"
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

  def xFieldName(n: String): Option[String] = n match {
    case "project"           => None // separate
    case "issuekey"          => Some("numberInProject")
    case "summary"           => Some(n)
    case "reporter"          => Some("reporterName")
    case "created"           => Some(n)
    case "updated"           => Some(n)
    case "issuetype"         => Some("type")
    case "priority"          => Some(n)
    case "status"            => None // see "resolution"

    case "assignee"          => Some("assigneeName")
    case "description"       => Some(n)
    case "environment"       => Some(n) // custom
    case "resolution"        => Some("state")
    case "resolutiondate"    => Some("resolved")
    case "duedate"           => Some("due")
    case "versions"          => Some("affectsVersion")
    case "fixVersions"       => Some("fixedVersion")
    case "labels"            => None // separate
    case "issuelinks"        => None // separate
    case "components"        => None // translated to labels as youtrack supports only one subsystem: TODO infer subsystem from subsystem label (pick least frequent label?)
    case "comment"           => Some("comment")
    case "attachment"        => None // separate

    case "votes"             => Some("voterName")
    case "watches"           => Some("watcherName") // also includes customfield_10005

    case "customfield_10005" => None // see "watches"

    case _ => println(s"IGNORING FIELD $n"); None
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
      case User(id, dn, Some(em)) if validUserName (id) => x
      case User(id, dn, em) => User(cleanUserName(id), dn, em orElse Some(BOGUS_EMAIL))(x.groups)
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

  val customFields = List("environment", "due")
  val addToStatesBundle = List("Backport Pending", "Not a Bug")

}
