package bbj
import fastparse.all

object toMarkdown {
  private val repoRef = "scala/bug"

  import fastparse.all._
  type PS = Parser[String]

  private val join: ((String, String)) => String = { case (x,y) => x+y }
  private val join3: ((String, String, String)) => String = { case (x,y,z) => x+y+z }
  private def remark(open: String, close: String)(x: String) = open + x + close
  private def remark(open: String)(x: String) = open + x + open

  private val wsChar = CharPred(_.isWhitespace)
  private val lineEnd = P("\n")
  private val wsCharNotEOL = !lineEnd ~ wsChar
  val skipToNextLine: P0 = End | lineEnd | (wsCharNotEOL.rep ~ lineEnd).map(_ => ())


  // non-empty whitespace, including line-end, or the end of input (used to demarcate words)
  lazy val ws: PS = P(wsChar.rep(1).!)
  lazy val skipWs: P0 = P((!lineEnd ~ wsChar).rep)
  lazy val wsNotEOL: PS = P(wsCharNotEOL.rep(1).!) // do not allow End, as we rep this parser

  lazy val num = P(CharPred(_.isDigit)).rep(min=1)

  //
  def marked(delim: P0): PS =
    P(delim ~ phrase(delim) ~ delim).map(_.mkString(""))

  def words(p: PS): PS = P((p ~ (wordSep ~ p.?.map(_.getOrElse(""))).map(join).rep.map(_.mkString(""))).map(join))

  lazy val wordSep: PS = P(wsNotEOL | CharIn(",.;:!/&").!) // do not include "|" because it separates a link's description from the url
  private def phrase(delim: P0) = words(markedWord | unmarkedWord(delim))

  def unmarkedWord(delim: P0): PS =
    P((!(ws | delim) ~ AnyChar.!).rep(1).map(_.mkString("")))

  def op(s: String) = P(!(&("\\")) ~ s)

  // a word, no whitespace on either side
  lazy val markedWord: PS =
    P(verbatim | insert | superscript | subscript | del | strong | emphasis | link | issueRef)

  lazy val insert      : PS = marked(op("+")) map remark("<ins>", "</ins>")
  lazy val superscript : PS = marked(op("^")) map remark("<sup>", "</sup>")
  lazy val subscript   : PS = marked(op("~")) map remark("<sub>", "</sub>")
  lazy val del         : PS = marked(op("-")) map remark("~~")
  lazy val strong      : PS = marked(op("*")) map remark("**")
  lazy val emphasis    : PS = marked(op("_")) map remark("*")


  lazy val linkSep = op("|")
  lazy val url = issueUrl.map(i => s"https://github.com/$repoRef/issues/$i") | unmarkedWord("]")
  // don't parse [bla] as a link because it's often not one
  lazy val link: PS = P(
    (    "["  ~ skipWs ~ phrase(linkSep).! ~ skipWs ~ linkSep ~ skipWs ~ url.! ~ skipWs ~ "]").map{ case (desc, href) => s"[$desc]($href)" }
      | ("[~" ~ skipWs ~ unmarkedWord("]").map(User.toGithub.get).filter(_.nonEmpty) ~ skipWs ~ "]").map{ case user => s"@${user.get}" }
    )

  lazy val issueUrl = P("https://issues.scala-lang.org/browse/SI-" ~ num.!)
  lazy val issueRef: PS = P("SI-" ~ num.! | issueUrl).map(i => s"$repoRef#$i")

  // consume whole lines, assuming we're at the start of a line, consuming up to and including line end
  // defend against matching just End, since that would loop when we `rep` this parser
  // the line parsers don't actually include a final newline
  // TODO: this fails to parse the last line if it doesn't end in a newline, but adding | End will loop on empty lines because we rep
  lazy val anyLine: PS = P((!lineEnd ~ AnyChar).rep.! ~ lineEnd)
  // TODO: should parse:
  //  "Could we do better? Given that {{T <: Program}} on branch 1, and {{Program}} is final, could we infer that {{T == Program}}? This looks like a common case, and a worthwhile ehancement."
  lazy val wikiLine: PS =
    P(block | (header ~ wsNotEOL ~ wikiLineRest()).map(join3) | (bullets ~ wsNotEOL ~ wikiLineRest()).map(join3) | wikiLineRest())

  def codeLine(delim: String): PS = P(!blockEnd(delim) ~ anyLine)

  // consumes, but does not emit newline in output
  def wikiLineRest(delim: P0 = Fail): PS =
    P((phrase(delim) ~  (lineEnd | &(delim) | End).map(_ => "")).map(join))

  // can span lines
  lazy val verbatim =
    P(op("{{") ~ (!op("}}") ~ AnyChar.!).rep ~ op("}}") map (_.mkString("`", "", "`")))

  // at start of line
  lazy val header: PS = P("h" ~ CharIn("123456").! ~".") map (i => "#" * i.toInt)

  lazy val bullets: PS =
    (!"--- " ~ // don't parse diff as bullets
      P(("-" | "#" | "*").!.rep(1)) map (bs => " "*(bs.length-1)+"-"))

  lazy val block: PS = P(
    (blockOpen ~ skipToNextLine) flatMap { case (tag, langName) =>
      (codeLine(tag).rep.map(_.mkString("\n")) ~ blockEnd(tag) ~ skipToNextLine) map { c =>
        s"```$langName\n$c\n```"
      }
    })

  // helper for blocks
  lazy val lang = P(CharPred(_.isLetter).rep.!)
  lazy val codeLang = P("code".! ~ ((":" ~ lang.!) | Pass.map(_ => "scala")))

  // we lump all block formats together, as users often confused quote for code....
  // skip leading whitespace before tag to be nice
  lazy val blockOpen = P(skipWs ~ op("{")~ (codeLang | (("quote" | "noformat").! ~ Pass.map(_ => ""))) ~ op("}"))
  def blockEnd(tag: String): P0 = P(skipWs ~ op("{")~ tag ~op("}"))

  lazy val textile = P((wikiLine | anyLine).rep.map(_.mkString("\n")) ~ End) // all or nothing baby

  // {{ or }} by itself on a line is probably meant to be {code} (often used mixed... :-/)
  val shouldBeCode = """(?m)^\s*((\{\{)|(}}))\s*$""".r
  def sanitize(s: String) = {
    val lineEnding = if (s.endsWith("\n")) s else s+"\n" // to avoid issues with End.rep (see e.g. anyLine)
    shouldBeCode.replaceAllIn(lineEnding, "{code}") // fix up unbalanced usages of "{{\n ...\n{code}"
  }
  def apply(s: String): String =
    textile.parse(sanitize(s)).fold((_, _, _) => s, (res, _) => res)
}
