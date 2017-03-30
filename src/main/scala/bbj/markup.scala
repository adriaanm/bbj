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
  lazy val ws: PS = wsChar.rep(1).!
  lazy val wsNotEOL: PS = wsCharNotEOL.rep(1).! // do not allow End, as we rep this parser

  lazy val num = P(CharPred(_.isDigit)).rep(min=1)

  //
  def marked(delim: P0): PS =
    P(delim ~ phrase(delim) ~ delim).map(_.mkString(""))

  def words(p: PS): PS = (p ~ (wordSep ~ p.?.map(_.getOrElse(""))).map(join).rep.map(_.mkString(""))).map(join)

  val wordSep: PS = wsNotEOL | CharIn(",.;:!/&|").!
  private def phrase(delim: P0) = words(markedWord | unmarkedWord(delim))

  def unmarkedWord(delim: P0): PS = (!(ws | delim) ~ AnyChar.!).rep(1).map(_.mkString(""))

  def op(s: String) = P(!(&("\\")) ~ s)

  // a word, no whitespace on either side
  lazy val markedWord: PS =
    verbatim | insert | superscript | subscript | del | strong | emphasis | link | issueRef

  lazy val insert      : PS = marked(op("+")) map remark("<ins>", "</ins>")
  lazy val superscript : PS = marked(op("^")) map remark("<sup>", "</sup>")
  lazy val subscript   : PS = marked(op("~")) map remark("<sub>", "</sub>")
  lazy val del         : PS = marked(op("-")) map remark("~~")
  lazy val strong      : PS = marked(op("*")) map remark("**")
  lazy val emphasis    : PS = marked(op("_")) map remark("*")


  val linkSep = op("|")
  lazy val url = issueUrl.map(i => s"https://github.com/$repoRef/issues/$i") | unmarkedWord("]")
  lazy val linkDesc = P(wikiLineRest(linkSep).! ~ linkSep)
  lazy val link: PS = ("[" ~ linkDesc.? ~ url.! ~ "]").map{
    case (Some(desc), href) => s"[$desc]($href)"
    case (_, href) => href
  }

  val issueUrl = P("https://issues.scala-lang.org/browse/SI-" ~ num.!)
  lazy val issueRef: PS = ("SI-" ~ num.! | issueUrl).map(i => s"$repoRef#$i")

  // consume whole lines, assuming we're at the start of a line, consuming up to and including line end
  // defend against matching just End, since that would loop when we `rep` this parser
  // the line parsers don't actually include a final newline
  lazy val anyLine: PS = (!lineEnd ~ AnyChar).rep.! ~ lineEnd
  lazy val wikiLine: PS =
    block | (header ~ wsNotEOL ~ wikiLineRest()).map(join3) | (bullets ~ wsNotEOL ~ wikiLineRest()).map(join3) | wikiLineRest()

  def codeLine(delim: String): PS = !blockEnd(delim) ~ anyLine

  def wikiLineRest(delim: P0 = Fail): PS = (phrase(delim) ~  (lineEnd.! | (&(delim) | End).map(_ => ""))).map(join)

  // can span lines
  lazy val verbatim = op("{{") ~ (!op("}}") ~ AnyChar.!).rep ~ op("}}") map (_.mkString("`", "", "`"))

  // at start of line
  lazy val header: PS = P("h" ~ CharIn("123456").! ~".") map (i => "#" * i.toInt)
  lazy val bullets: PS = ("-" | "#" | "*").!.rep(1) map (bs => " "*(bs.length-1)+"-")
  lazy val block: PS =
    (blockOpen ~ skipToNextLine) flatMap { case (tag, langName) =>
      (codeLine(tag).rep.map(_.mkString("\n")) ~ blockEnd(tag) ~ skipToNextLine) map { c =>
        s"```$langName\n$c\n```"
      }
    }

  // helper for blocks
  lazy val lang = CharPred(_.isLetter).rep.!
  lazy val codeLang = P("code".! ~ ((":" ~ lang.!) | Pass.map(_ => "scala")))
  // we lump all block formats together, as users often confused quote for code....
  lazy val blockOpen = op("{")~ (codeLang | (("quote" | "noformat").! ~ Pass.map(_ => ""))) ~ op("}")
  def blockEnd(tag: String): P0 = op("{")~ tag ~op("}")

  lazy val textile = (wikiLine | anyLine).rep.map(_.mkString("\n")) ~ End // all or nothing baby

  def apply(s: String): String =
    textile.parse(s).fold((_, _, _) => s, (res, _) => res)
}

