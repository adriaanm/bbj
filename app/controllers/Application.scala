package controllers

import play.api._
import play.api.mvc._

object Application extends Controller {
  import play.api.libs.concurrent.Execution.Implicits._
  import util.jira.rest.api

  def index = Action {
    Async {
      api.getIssue("SI-6794"/*"SI-2457"*/).map { issue =>
        Ok(views.html.index(issue.toHtml))
      }
    }
  }

}