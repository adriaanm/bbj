import java.io.{File, FileOutputStream, InputStream}
import play.api.libs.ws.{WS, Response}
import play.api.libs.concurrent.Execution.Implicits._


def tryAuthUrl(url: String) = {
  val getString = play.api.Play.current.configuration.getString(_: String)
  (for (user <- getString("jira.user");
       pass <- getString("jira.password"))
    yield WS.url(url).withAuth(user, pass, com.ning.http.client.Realm.AuthScheme.BASIC)).getOrElse(WS.url(url))
}
     
// write the inputStream to a FileOutputStream
def copyToFile(in: InputStream, f: File, bufferSize: Int = 1024) = {
  val bytes = new Array[Byte](bufferSize)
  val out = new FileOutputStream(f)

  @annotation.tailrec
  def write(): Unit =
    in.read(bytes) match {
      case -1 =>
      case count =>
        out.write(bytes, 0, count)
        write()
    }

  try write()
  finally { out.flush(); out.close() }
}

def jiraUrl(uri: String) = "https://issues.scala-lang.org/rest/api/latest" + uri

def getIssue(i: Int) = {
  tryAuthUrl(jiraUrl(s"/issue/SI-${i}?expand=changelog")).get()
}

def filename(i: Int) = s"/tmp/jira/${i}.json"


val app = new play.core.StaticApplication(new java.io.File("."))


// this needs to run until fixpoint... 
// TODO:
// - convert into proper monadic computation that tracks whether something was changed and repeat
// - do twice and make sure the contents don't change --> use git repo?
def go =
  (1 to 6856).foreach { i => 
    val f = new File(filename(i))
    if (!f.exists)
      getIssue(i).foreach { resp =>
        try {
          copyToFile(resp.ahcResponse.getResponseBodyAsStream, f)
          println(s"wrote $i")
        } catch {
          case e: Exception => println(s"$i failed: $e")
        }
      }
  }
