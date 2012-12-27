package controllers

import play.api._
import play.api.mvc._
import util.jira.rest.Issue
import scala.concurrent.Future
import util.jira.rest.User
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Application extends Controller {
  import play.api.libs.concurrent.Execution.Implicits._
  import util.jira.rest.api

  def renderFailure(t: Throwable) = t match {
    case e: NoSuchElementException => <div></div>
    case _ => <div class="error">{t.getMessage}</div>
  }


  def renderIssue(issue: Issue) =
    <div> {issue.key}: <ul>{ issue.fields.toList.map {
        case (k@("votes" | "watches"), v: Future[List[User]]) => <li> {k}: {Await.result(v, Duration.Inf)} </li>
        case (k,v) => <li> {k}: {v} </li>
      }}</ul><p>{issue.changelog}</p>
    </div>

  def index = Action {
    val res: Future[xml.NodeSeq] =
      api.issues.map { _.map(renderIssue)(collection.breakOut) }

    Async{res map {ns: xml.NodeSeq => Ok(views.html.index(ns))}}
  }

}