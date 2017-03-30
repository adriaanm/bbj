package bbj

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer

import scala.concurrent.Await.result
import scala.concurrent.Future
import scala.concurrent.duration.Duration


object export {
  implicit val system = ActorSystem("BBJ")
  import system.dispatcher

  implicit val materializer: ActorMaterializer = ActorMaterializer()

  object jira extends JiraConnection {
    val materializer = export.materializer
    val context = system.dispatcher
  }

  // TODO: not yet fully reliable
  def allIssues = (1 to 10250).grouped(1024) map { batch =>
    result(Future.sequence(batch map (i => jira.getIssue(s"SI-$i"))), Duration.Inf)
  }

  // filter on project because some issues were moved -- we are only moving the SI ones
  // 57 issues were moved, 43 were deleted, so we end up with 100 less than we fetch
  lazy val issues = allIssues.flatten.flatten.filter(_.fields("project") == "SI").toList

  lazy val labelFreq = {
    val labelMap = issues.map(i => (i, Labels.fromIssue(i)))
    val labels = issues.flatMap(Labels.fromIssue).toSet
    labels.map(l => (l, labelMap.collect{case (i, ls) if ls contains l => i.key}.size)).toList.sortBy(- _._2)
  }

  lazy val assignees = issues.flatMap(_.assignee)

  lazy val bodies = issues.flatMap(_.comments.map(_.body)) ++ issues.flatMap(_.description)

  def apply() = allIssues.toList


  object github extends GithubConnection {
    val materializer = export.materializer
    val context = system.dispatcher

    def createBugRepo = createRepo(new Repository(repoName, true, Some("The bug tracker for the Scala programming language. Please do not use for questions or feature requests.")))

    def createLabels = Future.sequence(Labels.all.map(name => createLabel(Label(name))))

    def createMilestones = {
      val allFixVersions = issues.flatMap(_.fixVersions).distinct
      val milestones = Milestones.all.map { name =>
        val version = allFixVersions.find(_.toString == name)
        val date = version.flatMap(_.releaseDate).map(d => d.toInstant)
        val state = version.map(v => if (!v.released) "open" else "closed")

        Milestone(title = name, description = version.flatMap(_.description), state = state, due_on = date)
      }

      milestones.map(m => result(createMilestone(m), Duration.Inf))
    }

    def createIssues(from: Int, to: Int) =
      issues.toIterator.slice(from, to).map(exportIssue).map(i => result(createIssue(i), Duration.Inf)).toList

    lazy val allMilestones: List[github.Milestone] = result(github.milestones, Duration.Inf)
  }

  object toMarkdown {
    import fastparse.all._
    type PS = Parser[String]

    private val join: ((String, String)) => String = { case (x,y) => x+y }
    private val join3: ((String, String, String)) => String = { case (x,y,z) => x+y+z }
    private def remark(open: String, close: String)(x: String) = open + x + close
    private def remark(open: String)(x: String) = open + x + open

    private val wsChar = CharPred(_.isWhitespace)
    private val lineEnd = P("\n") | End
    private val wsCharNotEOL = !lineEnd ~ wsChar
    val skipToNextLine: P0 = lineEnd | (wsCharNotEOL.rep ~ lineEnd).map(_ => ())

    def wsSep(p: PS): PS = (p ~ (wsNotEOL ~ p).map(join).rep.map(_.mkString(""))).map(join)

    // non-empty whitespace, including lineend, or the end of input (used to demarcate words)
    lazy val ws: PS = wsChar.rep(1).! | End.map(_ => "")
    lazy val wsNotEOL: PS = wsCharNotEOL.rep(1).! | End.map(_ => "")

    lazy val num = P(CharPred(_.isDigit)).rep(min=1)

    //
    def marked(delim: String): PS =
      P(op(delim) ~ wsSep(!op(delim) ~ (markedWord | unmarkedWord)) ~ op(delim)).map(_.mkString(""))

    private def unmarkedWord: PS = (!ws ~ AnyChar).!.rep(1).map(_.mkString(""))

    def op(s: String) = P(!(&("\\")) ~ s)

    // a word, no whitespace on either side
    lazy val markedWord: PS =
      verbatim | insert | superscript | subscript | del | strong | emphasis // | link | issueRef | foot

    lazy val insert      : PS = marked("+") map remark("<ins>", "</ins>")
    lazy val superscript : PS = marked("^") map remark("<sup>", "</sup>")
    lazy val subscript   : PS = marked("~") map remark("<sub>", "</sub>")
    lazy val del         : PS = marked("-") map remark("~~")
    lazy val strong      : PS = marked("*") map remark("**")
    lazy val emphasis    : PS = marked("_") map remark("*")


    // TODO
//    lazy val link: PS = "[ | ]"    "[$1]($2)"
//    val issueRefUrl = P("https://issues.scala-lang.org/browse/SI-"~num)
//    lazy val issueRef: PS = """\bSI-(\d+)\b"""
//
//    lazy val foot: PS  = "[ ]( )" "<$1>$2"


    // consume whole lines, assuming we're at the start of a line, consuming up to and including line end
    lazy val anyLine: PS = ((!lineEnd ~ AnyChar).rep ~ lineEnd).!
    lazy val wikiLine: PS =
      block | (header ~ wsNotEOL ~ wikiLineRest).map(join3) | (bullets ~ wsNotEOL ~ wikiLineRest).map(join3) | wikiLineRest

    def codeLine(delim: String): PS = (!blockEnd(delim) ~ anyLine).!

    lazy val wikiLineRest: PS = (wsSep(markedWord | unmarkedWord) ~ lineEnd.!).map(join)

    // can span lines
    lazy val verbatim = op("{{") ~ (!op("}}") ~ AnyChar.!).rep ~ op("}}") map (_.mkString("`", "", "`"))

    // at start of line
    lazy val header: PS = P("h" ~ CharIn("123456").! ~".") map (i => "#" * i.toInt)
    lazy val bullets: PS = (("-" | "#" | "*").!.rep(1) map ( bs => " "*(bs.length-1)+"-"))
    lazy val block: PS =
      (blockOpen ~ skipToNextLine) flatMap { case (tag, lang) =>
        (codeLine(tag).rep.map(_.mkString("\n")) ~ blockEnd(tag) ~ lineEnd) map { c =>
          s"```$lang\n$c\n```"
        }
      }

    // helper for blocks
    lazy val lang = CharPred(_.isLetter).rep.!
    lazy val codeLang = P("code".! ~ ((":" ~ lang.!) | Pass.map(_ => "scala")))
    lazy val blockOpen = op("{")~ (codeLang | (("quote" | "noformat").! ~ Pass.map(_ => ""))) ~ op("}")
    def blockEnd(tag: String): P0 = op("{")~ tag ~op("}")


    lazy val textile = wikiLine.rep.map(_.mkString("\n"))

    def apply(s: String): String = ""
  }

  def exportIssue(issue: Issue) = {
    val description =
      github.Description(
        title = issue.summary,
        body = issue.description.map(toMarkdown.apply).getOrElse(""),
        created_at = issue.created.toInstant,
        closed_at = issue.resolutionDate.map(_.toInstant),
        updated_at = issue.updated.toInstant,
        assignee = None, // TODO: when no long in private repo, issue.assignee.flatMap(_.toGithub),
        milestone = issue.fixVersions.headOption flatMap Milestones.fromVersion,
        closed = issue.closed,
        labels = Labels.fromIssue(issue))

    def metaComment = {
      val from = s"Imported From: https://issues.scala-lang.org/browse/${issue.key}" // TODO: if we end up redirecting from jira to github, these links won't work :smirk: -- add token to escape the future redirect? or already create an issues-archive subdomain?

      val reporter = s"Reporter: ${issue.reporter}"

      val affected =
        if (issue.affectedVersions.nonEmpty) List(s"Affected Versions: ${issue.affectedVersions.mkString(", ")}")
        else Nil

      val alsoFixedIn =
        if (issue.fixVersions.nonEmpty && issue.fixVersions.tail.nonEmpty) List(s"Other Milestones: ${issue.fixVersions.tail.mkString(", ")}")
        else Nil

      val crossRefs = issue.issuelinks.filterNot(_.reversed).groupBy(_.kind.name).toList.collect { case (kind, links) if links.nonEmpty =>
        val ghrefs = links flatMap (link => Issue.toGithubRef(link.targetKey))
        if (ghrefs.isEmpty) "" else s"${kind} ${ghrefs.mkString(", ")}"
      }.filterNot(_.isEmpty)


      val extras = from :: reporter :: (affected ++ alsoFixedIn ++ crossRefs)

      github.Comment(extras.mkString("\n", "\n", ""), None)
    }


    def ghcomment(c: Comment) = {
      val edited =
        if (c.updated == c.created) ""
        else if (c.updateAuthor != c.author) s" (edited by ${c.updateAuthor} on ${c.updated})"
        else s" (edited on ${c.updated})"

      github.Comment(s"${c.author} said$edited:\n${toMarkdown(c.body)}", Some(c.created.toInstant))
    }

    val comments =
      metaComment :: issue.comments.map(ghcomment)

    // ignored:
    // attachments
    // votesto
    // watches
    // creator (there are only 10 issues where reporter != creator)
    // lastViewed
    // duedate

    github.Issue(description, comments)
  }

  object Milestones {

    def fromVersion(v: Version): Option[Int] =
      github.allMilestones find (_.title == v.name) flatMap (_.number)

    val all = List(
      "Backlog",
      "2.6.1",
      "2.7.0",
      "2.7.1",
      "2.7.2",
      "2.7.3",
      "2.7.4",
      "2.7.5",
      "2.7.6",
      "2.7.7",
      "2.8.0",
      "2.8.1",
      "2.9.0",
      "2.9.0-1",
      "2.9.1",
      "2.9.2",
      "2.9.3-RC1",
      "2.9.3-RC2",
      "2.10.0-M1",
      "2.10.0-M2",
      "2.10.0-M3",
      "2.10.0-M4",
      "2.10.0-M5",
      "2.10.0-M6",
      "2.10.0-M7",
      "2.10.0-RC1",
      "2.10.0-RC2",
      "2.10.0-RC3",
      "2.10.0-RC5",
      "2.10.0",
      "2.10.1-RC1",
      "2.10.1",
      "2.10.2-RC1",
      "2.10.2-RC2",
      "2.10.2",
      "2.10.3-RC1",
      "2.10.3-RC2",
      "2.10.3-RC3",
      "2.10.3",
      "2.10.4-RC1",
      "2.10.4-RC2",
      "2.10.4-RC3",
      "2.10.4",
      "2.10.5",
      "2.10.6",
      "2.11.0-M1",
      "2.11.0-M2",
      "2.11.0-M3",
      "2.11.0-M4",
      "2.11.0-M5",
      "2.11.0-M6",
      "2.11.0-M7",
      "2.11.0-M8",
      "2.11.0-RC1",
      "2.11.0-RC3",
      "2.11.0-RC4",
      "2.11.0",
      "2.11.1",
      "2.11.2",
      "2.11.3",
      "2.11.4",
      "2.11.5",
      "2.11.6",
      "2.11.7",
      "2.11.8",
      "2.11.9",
      "2.12.0-M1",
      "2.12.0-M2",
      "2.12.0-M3",
      "2.12.0-M4",
      "2.12.0-M5",
      "2.12.0-RC1",
      "2.12.0-RC2",
      "2.12.0",
      "2.12.1",
      "2.12.2",
      "2.12.3",
      "2.13.0-M1",
      "2.13.0-M2",
      "2.13.0-M3",
      "2.13.0-M4",
      "2.13.0-RC1")
  }


  object Labels {
    def fromIssue(issue: Issue): List[String] = {
      val raw = Type(issue.issueType).toList ++
          Priority(issue.priority) ++
          Resolution(issue.resolution) ++
          issue.components.flatMap(Component) ++
          issue.fixVersions.flatMap(FixVersion) ++
          issue.labels ++
          issue.environment.map(_.split(' ').toList).getOrElse(Nil) // recover some info from misuse of the environment field (often used for labels...)

      raw map (l => rewrite.getOrElse(l, l)) filter all
    }

    def FixVersion(v: Version): Option[String] = v.name match {
      case "macro-paradise" => Some("macro-paradise")
      case _ => None
    }

    def Type(t: String): Option[String] = t match {
      case "Improvement" => Some("improvement")
      case "Suggestion" => Some("improvement")
      case "New Feature" => Some("improvement")
      case _ => None
    }

    // KEY: priority has VALUES: Vector(Critical, Major, Minor, Blocker, Trivial)
    def Priority(t: String): Option[String] = t match {
      case "Blocker" => Some("blocker")
      case "Critical" => Some("critical")
//      case "Major" => 2
      case "Minor" => Some("quickfix")
      case "Trivial" => Some("quickfix")
      case _ => None
    }

    def Resolution(r: Option[String]): Option[String] = r match {
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

    val rewrite = Map(
      "low-hanging-fruit" -> "quickfix",
      "type-inference" -> "infer",
      "macro" -> "macros",
      "opt" -> "optimizer",
      "inliner" -> "optimizer",
      "compiler-performance" -> "performance",
      "specialized" -> "specialization",
      "pattern-matching" -> "patmat",
      "exhaustiveness" -> "patmat",
      "unreachability" -> "patmat",
      "documentation" -> "docs",
      "wrong-bytecode" -> "bytecode",
      "verifyerror" -> "bytecode",
      "soundness" -> "should-not-compile",
      "unsound" -> "should-not-compile",
      "does-not-compile" -> "should-compile",
      "error-messages" -> "usability",
      "community" -> "help wanted",
      "feature" -> "enhancement")

    val all = Set(
      "improvement",
      "quickfix",
      "help wanted",
      "enhancement",
      "blocker",
      "critical",
      "regression",
      "minimized",
      "has-pull-request",
      "backport",
      "reflection",
      "typer",
      "infer",
      "macros",
      "macro-paradise",
      "backend",
      "optimizer",
      "specialization",
      "patmat",
      "mixin",
      "parser",
      "erasure",
      "repl",
      "interactive",
      "scaladoc",
      "lint",
      "library",
      "collections",
      "docs",
      "spec",
      "build",
      "crash",
      "compiler-crash",
      "runtime-crash",
      "bytecode",
      "should-not-compile",
      "should-compile",
      "separate-compilation",
      "usability",
      "structural-types",
      "applyDynamic",
      "java-interop",
      "implicit",
      "valueclass",
      "performance",
      "named-default-args",
      "existential",
      "annotations",
      "enum",
      "quasiquotes",
      "access",
      "depmet",
      "delayedinit",
      "tcpoly",
      "deprecation",
      "case-class",
      "dependent-types",
      "implicits",
      "serialization",
      "implicit-classes",
      "string-interpolation",
      "lub",
      "positions",
      "package-objects",
      "varargs",
      "overloading",
      "name-mangling",
      "f-bounds",
      "byname",
      "needinfo",
      "wontfix",
      "duplicate")
  }

}
