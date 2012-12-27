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

  def renderIssue(issue: Issue) =
    <div> {issue.key}: <ul>{ issue.fields.toList.map {
        case (k@("votes" | "watches"), v: Future[List[User]]) => <li> {k}: {Await.result(v, Duration.Inf)} </li>
        case (k,v) => <li> {k}: {v} </li>
      }}</ul><p>{issue.changelog}</p>
    </div>

  def index = Action {
    Async {
      val renderFutures = api.issues.map(x => x.map(renderIssue))
      Future.sequence(renderFutures).map { renderedIssues =>
        Ok(views.html.index(renderedIssues.map(identity)(collection.breakOut)))
      }
    }
  }

}