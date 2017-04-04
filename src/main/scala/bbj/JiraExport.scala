package bbj

import java.nio.file.Files
import java.io.File
import java.time.{Instant, OffsetDateTime, ZoneId}
import java.time.format.{DateTimeFormatter, FormatStyle}

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer

import scala.collection.immutable.IndexedSeq
import scala.concurrent.Await.result
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.Try

object format {
  def dateTime(date: OffsetDateTime) = instant(date.toInstant)

  def instant(instant: Instant) =
    Try {
      DateTimeFormatter.ofLocalizedDateTime(FormatStyle.MEDIUM).withZone(ZoneId.of("UTC")).format(instant)+" UTC"
    } getOrElse(instant.toString)
}

object export {
  implicit val system = ActorSystem("BBJ")
  import system.dispatcher

  implicit val materializer: ActorMaterializer = ActorMaterializer()

  object jira extends JiraConnection {
    val materializer = export.materializer
    val context = system.dispatcher
  }

  // TODO: not yet fully reliable
  def allIssues: Iterator[Option[Issue]] = ((1 to 10250).grouped(1024) map { batch =>
    result(Future.sequence(batch map (i => jira.getIssue(s"SI-$i"))), Duration.Inf)
  }).flatten

  private def deletedIssue(key: String) = Issue(key, Map.empty)
  // filter on project because some issues were moved -- we are only moving the SI ones
  // 57 issues were moved, 43 were deleted, so we end up with 100 less than we fetch
  lazy val issues: List[Issue] = allIssues.zipWithIndex.flatMap { case (issue, idx) => issue.orElse(Some(deletedIssue(s"SI-${idx + 1}"))) }.toList


  lazy val labelFreq = {
    val labelMap = issues.map(i => (i, Labels.fromIssue(i)))
    val labels = issues.flatMap(Labels.fromIssue).toSet
    labels.map(l => (l, labelMap.collect{case (i, ls) if ls contains l => i.key}.size)).toList.sortBy(- _._2)
  }

  lazy val assignees = issues.flatMap(_.assignee)

  lazy val bodies = issues.flatMap(_.comments.map(_.body)) ++ issues.flatMap(_.description)


  // to use git diff to sanity check the markdown conversion
  def translateBodies(doOrig: Boolean) = {
    import scala.collection.JavaConverters._
    val changed = bodies.map(x => (x, bbj.toMarkdown.apply(x))).filterNot{ case (orig, md) => orig == md }
    changed.zipWithIndex.foreach { case ((orig, md), i) =>
      Files.write(new File(s"/Users/adriaan/git/bbj-md/$i.md").toPath, (if (doOrig) orig else md).lines.toList.asJava)
    }
  }

  object github extends GithubConnection {
    val materializer = export.materializer
    val context = system.dispatcher

    def createBugRepo = result(
      createRepo(new Repository(repoName, `private` = true, Some("The bug tracker for the Scala programming language. Please do not use for questions or feature requests."))),
      Duration.Inf)


    def createLabels = result(Future.sequence(
      Labels.all.toList.map(name => createLabel(Label(name)))),
      Duration.Inf)

    def createMilestones = {
      val allVersions = issues.flatMap(i => i.fixVersions ++ i.affectedVersions).distinct
      val milestones = Milestones.all.map { name =>
        val version = allVersions.find(_.toString == name)
        val date = version.flatMap(_.releaseDate).map(d => d.toInstant)
        val state = version.map(v => if (!v.released) "open" else "closed")

        Milestone(title = name, description = version.flatMap(_.description), state = state, due_on = date)
      }

      result(Future.sequence(
        milestones.map(createMilestone)),
        Duration.Inf)
    }

    def inviteAssignees =
      User.toGithub.values.foreach { u =>
        Thread.sleep(1000)
        print(s"Inviting $u: ")
        println(result(inviteToTeam(Team("contributors", Some(2321732)), u), Duration.Inf))
      }

    // ZERO-BASED :roll_eyes:
    def createIssues(from: Int, to: Int) =
      createIssuesAndWait(issues.toIterator.slice(from, to).map { i =>
        print(s"Exporting ${i.key} ${i.summary}")
        exportIssue(i)
      })

    def gogogo =
      createIssues(0, 10250).foreach(println)
  }

  def exportIssue(issue: Issue) = {
    val assignee =
      if (issue.fields.isEmpty) None
      else issue.assignee.flatMap(_.toGithub).filter(User.inContributorTeam)

    val description =
      if (issue.fields.isEmpty) github.Description(
        title = "(Issue was deleted)",
        body = ":wastebasket:",
        created_at = Instant.EPOCH,
        closed_at = Some(Instant.EPOCH),
        updated_at = Instant.EPOCH,
        assignee = assignee,
        milestone = None,
        closed = true,
        labels = Nil)
      else {
        github.Description(
          title = issue.summary,
          body = issue.description.map(toMarkdown.apply).getOrElse(s"(No description for ${issue.key}.)"),
          created_at = issue.created.toInstant,
          closed_at = issue.resolutionDate.map(_.toInstant),
          updated_at = issue.updated.toInstant,
          assignee = assignee,
          milestone = issue.fixVersions.headOption flatMap Milestones.fromVersion,
          closed = issue.closed,
          labels = Labels.fromIssue(issue))
      }

    def metaComment = {
      val from = s"Imported From: https://issues.scala-lang.org/browse/${issue.key}?orig=1"
      val reporter = s"Reporter: ${issue.reporter}"

      val assignees =
        if (assignee.isEmpty) issue.assignee.map(a => s"Assignee: $a").toList
        else Nil

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

      val attachments =
        if (issue.attachments.isEmpty) Nil
        else "Attachments:" :: issue.attachments.map(a => s" - $a")

      val extras = from :: reporter :: (assignees ++ affected ++ alsoFixedIn ++ crossRefs ++ attachments)

      github.Comment(extras.mkString("\n", "\n", ""), Some(issue.created.toInstant))
    }


    def ghcomment(c: Comment) = {
      val edited =
        if (c.updated == c.created) ""
        else {
          val editedOn = format.dateTime(c.updated)
          if (c.updateAuthor != c.author) s" (edited by ${c.updateAuthor} on $editedOn)"
          else s" (edited on $editedOn)"
        }

      github.Comment(s"${c.author} said$edited:\n${toMarkdown(c.body)}", Some(c.created.toInstant))
    }

    val comments =
      if (issue.fields.isEmpty) Nil
      else metaComment :: issue.comments.map(ghcomment)

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
    lazy val allMilestones: List[github.Milestone] = result(github.milestones, Duration.Inf)

    def fromVersion(v: Version): Option[Int] =
      allMilestones find (_.title == v.toString) flatMap (_.number)

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
      "2.9.3",
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
      "2.10.1-RC2",
      "2.10.1-RC3",
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
      "2.11.0-RC2",
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
