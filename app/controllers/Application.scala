package controllers

import play.api._
import play.api.mvc._
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import bbj.JiraConnection
import bbj.Issues

object Application extends Controller {
  import play.api.libs.concurrent.Execution.Implicits._

  object jira extends JiraConnection { val issues = new Issues{} }
  import jira.allIssues
  import jira.issues._

  def renderFailure(t: Throwable) = t match {
    case e: NoSuchElementException => <div></div>
    case _                         => <div class="error">{ t.getMessage }</div>
  }

  def renderIssue(issue: Issue) =
    <div>
      { issue.key }
      :<ul>{
      issue.fields.toList.map {
        case (k @ ("votes" | "watches"), v: Future[List[User]]) => <li> { k }: { Await.result(v, Duration.Inf) } </li>
        case (k, v)                                             => <li> { k }: { v } </li>
      }
    }</ul>
    </div> //<p>{ issue.changelog }</p>

  def index = Action {
    val res: Future[xml.NodeSeq] =
      allIssues.map { _.map(renderIssue)(collection.breakOut) }

    Async { res map { ns: xml.NodeSeq => Ok(views.html.index(ns)) } }
  }

}