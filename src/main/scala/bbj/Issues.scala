package bbj

import java.time.OffsetDateTime
import java.util.Date

case class Project(projectId: String, name: String, description: String, lead: String, startingNumber: Int = 1)

object User {
  val inContributorTeam =
    Set("Atry", "Blaisorblade", "DarkDimius", "DavidBiesack", "Ichoran", "JustinPihony", "Kornel", "OlegYch", "Philippus",
      "RomanHargrave", "Sciss", "SethTisue", "ShaneDelmore", "TiarkRompf", "ViniciusMiana", "VladUreche", "adriaanm",
      "andy1138", "axel22", "benhutchison", "bjornregnell", "blair", "burakemir", "bvenners", "cchantep", "chrisokasaki",
      "cunei", "cvogt", "dcaoyuan", "dcsobral", "demobox", "densh", "dgruntz", "dickwall", "djspiewak", "dlwh", "dotta",
      "dpogretskiy", "dragos", "dubochet", "dwijnand", "felixmulder", "fsalvi", "gaydenko", "gbasler", "ghik", "gkossakowski",
      "gourlaysama", "gzm0", "hamishdickson", "hamnis", "heathermiller", "hseeberger", "hubertp", "huitseeker", "ichaki5748",
      "ihji", "ijuma", "ilyasergey", "ingoem", "janekdb", "jasonxh", "jdevelop", "jedesah", "jesnor", "jinfu-leng", "jodersky",
      "jorgeortiz85", "jsuereth", "jvican", "kanielc", "khernyo", "kmizu", "kzys", "leifwickland", "lexn82", "lexspoon",
      "lgieron", "liff", "lindydonna", "lrytz", "mads-hartmann", "manojo", "martende", "martijnhoekstra", "melezov", "michael72",
      "michaelpigg", "michelou", "milessabin", "namin", "nilskp", "non", "odersky", "paplorinc", "paulbutcher", "paulp",
      "phaller", "propensive", "refried", "retronym", "richdougherty", "rjolly", "romanowski", "rtyley", "rubyu", "ruippeixotog",
      "samskivert", "sjrd", "slothspot", "soc", "som-snytt", "sriggin", "sschaef", "sstucki", "stepancheg", "szeiger", "tabdulradi",
      "teemulehtinen", "timcharper", "triggerNZ", "vglushak-vt", "vigdorchik", "viktorklang", "vjovanov", "xeno-by")

  // most assignees and top 100 submitters that also have known github username
  val toGithub = Map(
    "JanBessai"                      -> "JanBessai", // Jan Bessai
    "JustinPihony"                   -> "JustinPihony", // Justin Pihony
    "Kornel"                         -> "Kornel", // Kornel
    "Lyle"                           -> "lylek", // Lyle Kopnicky
    "MartijnHoekstra"                -> "martijnhoekstra", // Martijn Hoekstra
    "Philippus"                      -> "Philippus", // Philippus Baalman
    "VladimirNik"                    -> "VladimirNik", // Vladimir Nikolaev
    "acruise"                        -> "acruise", // Alex Cruise
    "alefas"                         -> "Alefas",
    "alexey_r"                       -> "alexeyr",
    "allisonhb"                      -> "allisonhb", // allisonhb
    "andy"                           -> "andy1138", // Andy Hicks
    "anli"                           -> "gaydenko",
    "antoras"                        -> "sschaef",
    "apm"                            -> "som-snytt",
    "apwashere"                      -> "demobox", // Andrew Phillips
    "arya"                           -> "refried",
    "atry"                           -> "Atry",
    "benhutchison"                   -> "benhutchison",
    "bjornregnell"                   -> "bjornregnell", // Bjorn Regnell
    "blair"                          -> "blair",
    "boia01"                         -> "aboisvert",
    "burmako"                        -> "xeno-by",
    "bvenners"                       -> "bvenners",
    "cbilgin"                        -> "cbilgin", // Cagatay Bilgin
    "cchantep"                       -> "cchantep", // C. Chantepie
    "chrisjames"                     -> "ChrisJamesC", // Christopher Chiche
    "chrisokasaki"                   -> "chrisokasaki", // Chris Okasaki
    "clhodapp"                       -> "clhodapp", // Chris Hodapp
    "cunei"                          -> "cunei",
    "d_m"                            -> "non", //  Osheim
    "darkdimius"                     -> "DarkDimius",
    "darlenewallach"                 -> "wallachd",
    "dcaoyuan"                       -> "dcaoyuan",
    "dchenbecker"                    -> "dchenbecker", // Derek Chen-Becker
    "dcsobral"                       -> "dcsobral",
    "den_sh"                         -> "densh",
    "dgruntz"                        -> "dgruntz",
    "dibblego"                       -> "tonymorris",
    "dickwall"                       -> "dickwall", // Dick Wall
    "djb"                            -> "DavidBiesack",
    "djspiewak"                      -> "djspiewak",
    "dlwh"                           -> "dlwh",
    "dmitry.m"                       -> "slothspot", // Dmitry Melnichenko
    "dotta"                          -> "dotta",
    "dpogretskiy"                    -> "dpogretskiy", // Dmitriy Pogretskiy
    "dragos"                         -> "dragos",
    "drmaciver"                      -> "DRMacIver",
    "dsbos"                          -> "dsbos", // Daniel Barclay
    "dturner-tw"                     -> "dturner-tw", // David Turner
    "dubochet"                       -> "dubochet",
    "dwijnand"                       -> "dwijnand", // Dale Wijnand
    "eengbrec"                       -> "eengbrec",
    "eje"                            -> "erikerlandson", //  Erlandson
    "emir"                           -> "burakemir", // Burak Emir
    "etorreborre"                    -> "etorreborre",
    "extempore"                      -> "paulp", // Paul Phillips
    "felixmulder"                    -> "felixmulder", // Felix Mulder
    "fsalvi"                         -> "fsalvi", // Fabien Salvi
    "gbasler"                        -> "gbasler", // Gerard Basler
    "gestalt"                        -> "michael-nischt",
    "ghik"                           -> "ghik", // Roman Janusz
    "gourlaysama"                    -> "gourlaysama", // Antoine Gourlay
    "grek"                           -> "gkossakowski",
    "hamish.dickson-at-gmail.com"    -> "hamishdickson", // Hamish Dickson
    "hamnis"                         -> "hamnis", // Erlend Hamnaberg
    "harrah"                         -> "harrah",
    "heathermiller"                  -> "heathermiller",
    "heiko.seeberger"                -> "hseeberger",
    "huitseeker"                     -> "huitseeker", // François Garillot
    "ichaki5748"                     -> "ichaki5748", // Evgeny Slutsky
    "ichoran"                        -> "Ichoran",
    "ihji"                           -> "ihji", // Heejong Lee
    "ijuma"                          -> "ijuma",
    "ilyas"                          -> "ilyasergey", // Ilya Sergey
    "imaier"                         -> "ingoem",
    "jamesiry"                       -> "JamesIry",
    "janekdb"                        -> "janekdb",
    "jasonxh"                        -> "jasonxh", // Hao Xia
    "jdevelop"                       -> "jdevelop", // Eugene Dzhurinsky
    "jedesah"                        -> "jedesah", // Jean-Remi Desjardins
    "jeortiz"                        -> "jorgeortiz85",
    "jinfu.leng"                     -> "jinfu-leng", // Jinfu Leng
    "jnordenberg"                    -> "jesnor",
    "jodersky"                       -> "jodersky", // jodersky
    "jpretty"                        -> "propensive",
    "jroper"                         -> "jroper",
    "jrudolph"                       -> "jrudolph",
    "jsalvata"                       -> "jsalvata",
    "jsuereth"                       -> "jsuereth", // Josh Suereth
    "jtvoorde"                       -> "jtvoorde", // Jeroen ter Voorde
    "jvican"                         -> "jvican", // Jorge Vicente Cantero
    "kanielc"                        -> "kanielc", // Denton Cockburn
    "kzys"                           -> "kzys", // Kazuyoshi Kato
    "leifwickland"                   -> "leifwickland", // Leif Wickland
    "lexn82"                         -> "lexn82",
    "lgieron"                        -> "lgieron", // Łukasz Gieroń
    "limo"                           -> "melezov", // Marko Elezovic
    "mads379"                        -> "mads-hartmann", // Mads Hartmann Jensen
    "magarcia"                       -> "magarciaEPFL",
    "malayeri"                       -> "lindydonna", // Donna Mithra Malayeri
    "manojo"                         -> "manojo", // Manohar Jonnalagedda
    "martende"                       -> "martende", // Oleg
    "matlik"                         -> "matlik", // James Matlik
    "maxcom"                         -> "maxcom", // Maxim Valyanskiy
    "mcdirmid"                       -> "mcdirmid",
    "melloc"                         -> "melloc", // Cody Mello
    "michael72"                      -> "michael72", // Michael Schulte
    "michaelpigg"                    -> "michaelpigg", // Michael Pigg
    "michelou"                       -> "michelou",
    "milessabin"                     -> "milessabin",
    "mirco"                          -> "dotta", // Mirco Dotta
    "mizushima"                      -> "kmizu", // Kota Mizushima
    "mkubala"                        -> "mkubala", // Marcin Kubala
    "moors"                          -> "adriaanm",
    "mpociecha"                      -> "mpociecha", // Michał Pociecha
    "ms-tg"                          -> "ms-tg", // Marc Siegel
    "mthorpe"                        -> "mt2309", // Michael Thorpe
    "nadezhin"                       -> "nadezhin", // Dmitry Nadezhin
    "namin"                          -> "namin", // Nada Amin
    "nilskp"                         -> "nilskp",
    "odersky"                        -> "odersky",
    "okomok"                         -> "okomok",
    "olegych"                        -> "OlegYch",
    "ollijh"                         -> "liff", // Olli Helenius
    "oschulz"                        -> "oschulz",
    "oxbow_lakes"                    -> "oxbowlakes",
    "paplorinc"                      -> "paplorinc", // Pap Lőrinc
    "paulbutcher"                    -> "paulbutcher",
    "pchiusano"                      -> "pchiusano",
    "pggiarrusso"                    -> "Blaisorblade",
    "phaller"                        -> "phaller",
    "plocinic"                       -> "hubertp",
    "ppavlov"                        -> "pavelpavlov", // Pavel Pavlov
    "prokopec"                       -> "axel22",
    "propensive"                     -> "propensive",
    "puffnfresh"                     -> "puffnfresh", // Brian McKenna
    "qerub"                          -> "qerub", // Christoffer Sawicki
    "retronym"                       -> "retronym",
    "rhargrave"                      -> "RomanHargrave", // Roman Hargrave
    "rich dougherty"                 -> "richdougherty",
    "rickynils"                      -> "rickynils",
    "rjfwhite"                       -> "rjfwhite", // Rob Whitehead
    "rjolly"                         -> "rjolly", // rjolly
    "rklaehn"                        -> "rklaehn",
    "roland.kuhn"                    -> "rkuhn",
    "romanowski"                     -> "romanowski", // Krzysztof Romanowski
    "rompf"                          -> "TiarkRompf",
    "rossjudson"                     -> "rossjudson", // Ross Judson
    "rtyley"                         -> "rtyley", // Roberto Tyley
    "rubyu"                          -> "rubyu", // rubyu
    "ruippeixotog"                   -> "ruippeixotog", // Rui Gonçalves
    "rytz"                           -> "lrytz",
    "samskivert"                     -> "samskivert", // Michael Bayne
    "sbocq"                          -> "sbocq",
    "sciss"                          -> "Sciss",
    "scottcarey"                     -> "scottcarey", // Scott Carey
    "sethtisue"                      -> "SethTisue",
    "shanedelmore"                   -> "ShaneDelmore", // Shane Delmore
    "sjrd"                           -> "sjrd", // Sébastien Doeraene
    "skyluc"                         -> "skyluc",
    "soc"                            -> "soc",
    "spoon"                          -> "lexspoon",
    "sriggin"                        -> "sriggin", // Sean Riggin
    "sstucki"                        -> "sstucki", // Sandro Stucki
    "stepancheg"                     -> "stepancheg",
    "szabi"                          -> "khernyo", // Szabolcs Berecz
    "szeiger"                        -> "szeiger",
    "tabdulradi"                     -> "tabdulradi", // Tamer Mohammed Abdulradi
    "teemulehtinen"                  -> "teemulehtinen", // Teemu Lehtinen
    "thebugslayer"                   -> "thebugslayer",
    "timcharper"                     -> "timcharper", // Tim Harper
    "tobias.schlatter"               -> "gzm0", // Tobias Schlatter
    "triggernz"                      -> "triggerNZ", // Tin Pavlinic
    "ureche"                         -> "VladUreche",
    "v.glushak-at-gmail.com"         -> "vglushak-vt", // Volodymyr Glushak
    "venechka1"                      -> "vigdorchik",
    "viktor.klang"                   -> "viktorklang", // Viktor Klang
    "viniciusmiana"                  -> "ViniciusMiana", // Vinicius Miana Bezerra
    "vjovanov"                       -> "vjovanov", // Vojin Jovanovic
    "vogt"                           -> "cvogt",
    "vuakko"                         -> "vuakko", // Niko Vuokko
    "wiejacha"                       -> "pawel-wiejacha", // Paweł Wiejacha
    "yllan"                          -> "yllan", // Yung-Luen Lan
    "ymasory"                        -> "LilyLambda" // Yuvi Masory
  )
}

case class User(name: String, displayName: String, emailAddress: Option[String])(val groups: concurrent.Future[List[String]]) {
  def sanitize(s: String) = s.replaceAllLiterally("@","-at-")

  def toGithub: Option[String] = User.toGithub.get(name)

  override def toString = toGithub match {
    case Some(userName) => s"@$userName" // at-mention if user is known (no notification is sent on bulk import)
    case None =>
      val disp = sanitize(displayName)
      val user = sanitize(name)
      if (disp == user) user
      else s"${disp} (${user})"
  }
}

case class Version(name: String, description: Option[String], releaseDate: Option[Date], archived: Boolean, released: Boolean) {
  override def toString = (if (name.startsWith("Scala ")) name drop "Scala ".length else name).trim
}

case class Comment(author: User, body: String, updateAuthor: User, created: OffsetDateTime, updated: OffsetDateTime)

case class Attachment(filename: String, author: User, created: OffsetDateTime, url: String, size: Int, mimeType: String, properties: Map[String, Any]) {
  override def toString = s"[$filename]($url) (created on ${format.dateTime(created)}, $size bytes)"
}

sealed class IssueLinkType(val name: String)

case object Relates extends IssueLinkType("See")
case object Duplicates extends IssueLinkType("Duplicates")
case object Blocks extends IssueLinkType("Blocks")
case object Clones extends IssueLinkType("Clones")
case object Mentions extends IssueLinkType("See")

case class IssueLink(kind: IssueLinkType, targetKey: String, reversed: Boolean = false) {
  override def toString = if (!reversed) s"${kind.name} ${Issue.toGithubRef(targetKey).getOrElse(targetKey)}" else ""
}

object Issue {
  def toGithubRef(key: String): Option[String] =
    if (key.startsWith("SI-")) Some("#"+key.drop("SI-".length))
    else None

}
case class Issue(key: String, fields: Map[String, Any]) {
  def summary          = fields get "summary"        collect { case x: String => x } getOrElse("(This issues was deleted.)")
  def description      = fields get "description"    collect { case x: Option[String] => x } getOrElse(Some(":wastebasket:"))

  // pick a milestone
  def fixVersions      = fields get "fixVersions"    collect { case x: List[Version] => x } getOrElse(Nil)
  // use label?
  def affectedVersions = fields get "versions"       collect { case x: List[Version] => x } getOrElse(Nil)

  def assignee         = fields get "assignee"       collect { case x: Option[User] => x } get
  def reporter         = fields get "reporter"       collect { case x: User => x } get
  def created          = fields get "created"        collect { case x: OffsetDateTime => x } get
  def updated          = fields get "updated"        collect { case x: OffsetDateTime => x } get
  def resolutionDate   = fields get "resolutiondate" collect { case x: Option[OffsetDateTime] => x } get

  def closed           = (fields get "status"         collect { case x: String => x } get) == "CLOSED"

  def issueType        = fields get "issuetype"      collect { case x: String => x } get
  def priority         = fields get "priority"       collect { case x: String => x } get
  def resolution       = fields get "resolution"     collect { case x: Option[String] => x } get
  def components       = fields get "components"     collect { case x: List[String] => x } get
  def labels           = fields get "labels"         collect { case x: List[String] => x } get
  def environment      = fields get "environment"    collect { case x: Option[String] => x } get

  def comments         = fields get "comment"        collect { case x: List[Comment] => x } getOrElse(Nil)
  def issuelinks       = fields get "issuelinks"     collect { case x: List[IssueLink] => x } get

  def attachments      = fields get "attachment"     collect { case x: List[Attachment] => x } getOrElse(Nil)

//
//  def lastViewed       = fields get "lastViewed"     collect { case x: Option[Date] => x }
//  def dueDate          = fields get "duedate"        collect { case x: Option[Date] => x }
//  def creator          = fields get "creator"        collect { case x: User => x }
//  def votes            = fields get "votes"          collect { case x: Future[List[User]] => x }
//  def watches          = fields get "watches"        collect { case x: Future[List[User]] => x }
}
