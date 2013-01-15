package controllers

import play.api._
import play.api.mvc._
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import bbj.JiraConnection
import bbj.Issues
import play.api.libs.json.JsValue
import play.api.libs.ws.WS

object Application extends Controller {

  def index = Action {
    Ok(views.html.index(null))
  }

}