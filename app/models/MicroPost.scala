package models

import java.time.ZonedDateTime

import jp.t2v.lab.play2.pager.{ OrderType, Sortable }
import scalikejdbc._
import jsr310._
import skinny.orm._

case class MicroPost(id: Option[Long] = None,
                     userId: Long,
                     content: String,
                     createAt: ZonedDateTime,
                     updateAt: ZonedDateTime,
                     user: Option[User] = None)

object MicroPost extends SkinnyCRUDMapper[MicroPost] {

  override def tableName = "micro_posts"

  override def defaultAlias: Alias[MicroPost] = createAlias("m")

  belongsTo[User](User, (uf, u) => uf.copy(user = u)).byDefault

  override def extract(rs: WrappedResultSet, n: ResultName[MicroPost]): MicroPost =
    autoConstruct(rs, n, "user")

  def create(microPost: MicroPost)(implicit session: DBSession): Long =
    createWithAttributes(toNamedValues(microPost): _*)

  private def toNamedValues(record: MicroPost): Seq[(Symbol, Any)] = Seq(
    'userId   -> record.userId,
    'content  -> record.content,
    'createAt -> record.createAt,
    'updateAt -> record.updateAt
  )

  def update(microPost: MicroPost)(implicit session: DBSession): Int =
    updateById(microPost.id.get).withAttributes(toNamedValues(microPost): _*)

  implicit object sortable extends Sortable[MicroPost] {
    override val default: (String, OrderType) = ("id", OrderType.Descending)
    override val defaultPageSize: Int         = 10
    override val acceptableKeys: Set[String]  = Set("id")
  }

}