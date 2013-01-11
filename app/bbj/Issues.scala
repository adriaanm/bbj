package bbj

import java.util.Date

trait Issues {
  case class Project(projectId: String, name: String, description: String, lead: User, startingNumber: Int = 1)

  case class User(name: String, displayName: String, emailAddress: Option[String])(val groups: concurrent.Future[List[String]]) {
    override def toString = displayName
  }

  case class Version(name: String, description: Option[String], releaseDate: Option[Date], archived: Boolean, released: Boolean) {
    override def toString = name
  }

  case class Comment(author: User, body: String, updateAuthor: User, created: Date, updated: Date) {
    override def toString = s"on $created, $author said '$body' ($updated, $updateAuthor)"
  }

  case class Attachment(filename: String, author: User, created: Date, content: String, size: Int, mimeType: String, properties: Map[String, Any])

  sealed class IssueLinkType(val name: String, val inward: String, val outward: String) {
    def directed = outward != inward
  }
  case object Relates extends IssueLinkType("Relates", "relates to", "relates to")
  case object Duplicates extends IssueLinkType("Duplicates", "is duplicated by", "duplicates")
  case object Blocks extends IssueLinkType("Blocks", "is blocked by", "blocks")
  case object Clones extends IssueLinkType("Clones", "is cloned by", "clones")

  case class IssueLink(kind: IssueLinkType, sourceKey: String, targetKey: String) {
    override def toString = s"$sourceKey ${kind.name} $targetKey"
  }

  case class Issue(key: String, fields: Map[String, Any])

  trait IssuesTransform {
    def apply(x: User): User
    def apply(x: Version): Version
    def apply(x: IssueLinkType): IssueLinkType

    def mapOver(x: Any): Any = {
      x match {
        case u: User                         => apply(u)
        case v: Version                      => apply(v)
        case Comment(a, b, ua, c, u)         => Comment(apply(a), b, apply(ua), c, u)
        case Attachment(f, a, c, d, s, m, p) => Attachment(f, apply(a), c, d, s, m, p)
        case ilt: IssueLinkType              => apply(ilt)
        case IssueLink(ilt, o, i)            => IssueLink(apply(ilt), o, i)
        case Issue(key, fields)              => Issue(key, fields map { case (k, v) => (k, mapOver(v)) })
        case xs: Iterable[Any]               => xs.map(mapOver)
        case x                               => x
      }
    }
  }
}