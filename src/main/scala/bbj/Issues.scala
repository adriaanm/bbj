package bbj

import java.util.Date

import scala.concurrent.Future

case class Project(projectId: String, name: String, description: String, lead: String, startingNumber: Int = 1)

object User {
  // top 100 submitters that also have known github username
  val toGithub = Map(
    ("alefas", "Alefas"),
    ("alexey_r", "alexeyr"),
    ("anli", "gaydenko"),
    ("antoras", "sschaef "),
    ("apm", "som-snytt"),
    ("arya", "refried"),
    ("atry", "Atry"),
    ("benhutchison", "benhutchison"),
    ("blair", "blair"),
    ("boia01", "aboisvert"),
    ("burmako", "xeno-by"),
    ("bvenners", "bvenners"),
    ("cunei", "cunei"),
    ("darkdimius", "DarkDimius"),
    ("darlenewallach", "wallachd"),
    ("dcaoyuan", "dcaoyuan"),
    ("dcsobral", "dcsobral"),
    ("den_sh", "densh"),
    ("dgruntz", "dgruntz"),
    ("dibblego", "tonymorris"),
    ("djb", "DavidBiesack"),
    ("djspiewak", "djspiewak"),
    ("dlwh", "dlwh"),
    ("dotta", "dotta"),
    ("dragos", "dragos"),
    ("drmaciver", "DRMacIver"),
    ("dubochet", "dubochet"),
    ("dwijnand", "dwijnand"),
    ("eengbrec", "eengbrec"),
    ("etorreborre", "etorreborre"),
    ("ewilligerstempore", "paulp"),
    ("gestalt", "michael-nischt"),
    ("grek", "gkossakowski"),
    ("harrah", "harrah"),
    ("heathermiller", "heathermiller"),
    ("heiko.seeberger", "hseeberger"),
    ("ichoran", "Ichoran"),
    ("ijuma", "ijuma"),
    ("imaier", "ingoem"),
    ("jamesiry", "JamesIry"),
    ("janekdb", "janekdb"),
    ("jeortiz", "jorgeortiz85"),
    ("jnordenberg", "jesnor"),
    ("jroper", "jroper"),
    ("jrudolph", "jrudolph"),
    ("jsalvata", "jsalvata"),
    ("lexn82", "lexn82"),
    ("magarcia", "magarciaEPFL"),
    ("mcdirmid", "mcdirmid"),
    ("michelou", "michelou"),
    ("milessabin", "milessabin"),
    ("moors", "adriaanm"),
    ("nilskp", "nilskp"),
    ("odersky", "odersky"),
    ("okomok", "okomok"),
    ("olegych", "OlegYch"),
    ("oschulz", "oschulz"),
    ("oxbow_lakes", "oxbowlakes"),
    ("paulbutcher", "paulbutcher"),
    ("pchiusano", "pchiusano "),
    ("pggiarrusso", "Blaisorblade"),
    ("phaller", "phaller"),
    ("plocinic", "hubertp"),
    ("Prokopec", "axel22"),
    ("propensive", "propensive"),
    ("retronym", "retronym"),
    ("rich dougherty", "richdougherty"),
    ("rickynils", "rickynils"),
    ("rklaehn", "rklaehn"),
    ("roland.kuhn", "rkuhn"),
    ("rompf", "TiarkRompf"),
    ("rytz", "lrytz"),
    ("sbocq", "sbocq"),
    ("sciss", "Sciss"),
    ("sethtisue", "SethTisue"),
    ("skyluc", "skyluc"),
    ("soc", "soc"),
    ("spoon", "lexspoon"),
    ("stepancheg", "stepancheg"),
    ("szeiger", "szeiger"),
    ("thebugslayer", "thebugslayer"),
    ("ureche", "VladUreche"),
    ("venechka1", "vigdorchik"),
    ("vogt", "cvogt"))
}

case class User(name: String, displayName: String, emailAddress: Option[String])(val groups: concurrent.Future[List[String]]) {
  def sanitize(s: String) = s.replaceAllLiterally("@","-at-")

  def toGithub: Option[String] = User.toGithub.get(name)

  override def toString = toGithub match {
    case Some(username) => s"@userName" // at-mention if user is known (no notification is sent on bulk import)
    case None => s"${sanitize(displayName)} (${sanitize(name)})"
  }
}

case class Version(name: String, description: Option[String], releaseDate: Option[Date], archived: Boolean, released: Boolean) {
  override def toString = (if (name.startsWith("Scala ")) name drop "Scala ".length else name).trim
}

case class Comment(author: User, body: String, updateAuthor: User, created: Date, updated: Date) {
  override def toString = s"$author said:\n$body"
}

case class Attachment(filename: String, author: User, created: Date, content: String, size: Int, mimeType: String, properties: Map[String, Any])

sealed class IssueLinkType(val name: String)

case object Relates extends IssueLinkType("See")
case object Duplicates extends IssueLinkType("Duplicates")
case object Blocks extends IssueLinkType("Blocks")
case object Clones extends IssueLinkType("Clones")
case object Mentions extends IssueLinkType("See")

case class IssueLink(kind: IssueLinkType, targetKey: String, reversed: Boolean = false) {
  def toGithub: Option[String] = if (reversed) None else Some(toString)
  override def toString = s"${kind.name} ${Issue.toGithubRef(targetKey).getOrElse(targetKey)}"
}

object Issue {
  def toGithubRef(key: String): Option[String] =
    if (key.startsWith("SI-")) Some("#"+key.drop("SI-".length))
    else None

}
case class Issue(key: String, fields: Map[String, Any]) {
  def summary          = fields get "summary"        collect { case x: String => x }
  def description      = fields get "description"    collect { case x: Option[String] => x }

  // pick a milestone
  def fixVersions      = fields get "fixVersions"    collect { case x: List[Version] => x }
  // use label?
  def affectedVersions = fields get "versions"       collect { case x: List[Version] => x }

  def assignee         = fields get "assignee"       collect { case x: Option[User] => x }
  def reporter         = fields get "reporter"       collect { case x: User => x }
  def creator          = fields get "creator"        collect { case x: User => x }
  def created          = fields get "created"        collect { case x: Date => x }
  def updated          = fields get "updated"        collect { case x: Date => x }
  def lastViewed       = fields get "lastViewed"     collect { case x: Option[Date] => x }
  def resolutionDate   = fields get "resolutiondate" collect { case x: Option[Date] => x }
  def dueDate          = fields get "duedate"        collect { case x: Option[Date] => x }

  def issueType        = fields get "issuetype"      collect { case x: String => x }
  def priority         = fields get "priority"       collect { case x: String => x }
  def resolution       = fields get "resolution"     collect { case x: Option[String] => x }
  def components       = fields get "components"     collect { case x: List[String] => x }
  def labels           = fields get "labels"         collect { case x: List[String] => x }
  def environment      = fields get "environment"    collect { case x: Option[String] => x }


  def issuelinks       = fields get "issuelinks"     collect { case x: List[IssueLink] => x }

  def comment          = fields get "comment"        collect { case x: List[Comment] => x }

  def attachment       = fields get "attachment"     collect { case x: List[Attachment] => x }

  def votes            = fields get "votes"          collect { case x: Future[List[User]] => x }
  def watches          = fields get "watches"        collect { case x: Future[List[User]] => x }
}
