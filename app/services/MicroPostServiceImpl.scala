package services

import javax.inject.Singleton

import jp.t2v.lab.play2.pager.scalikejdbc._
import jp.t2v.lab.play2.pager.{ Pager, SearchResult }
import models.{ MicroPost, UserFollow }
import scalikejdbc._

import scala.util.Try

@Singleton
class MicroPostServiceImpl extends MicroPostService {

  override def create(microPost: MicroPost)(implicit dbSession: DBSession): Try[Long] = Try {
    MicroPost.create(microPost)
  }

  override def deleteById(microPostId: Long)(implicit dbSession: DBSession): Try[Int] = Try {
    MicroPost.deleteById(microPostId)
  }

  override def findByUserId(pager: Pager[MicroPost], userId: Long)(
      implicit dbSession: DBSession
  ): Try[SearchResult[MicroPost]] =
    countBy(userId).map { size =>
      SearchResult(pager, size)(findAllByWithLimitOffset(Seq(userId)))
    }

  override def countBy(userId: Long)(implicit dbSession: DBSession): Try[Long] = Try {
    MicroPost.countBy(sqls.eq(MicroPost.defaultAlias.userId, userId))
  }

  override def findAllByWithLimitOffset(pager: Pager[MicroPost], userId: Long)(
      implicit dbSession: DBSession
  ): Try[SearchResult[MicroPost]] = Try {
    val followingIds =
      UserFollow.findAllBy(sqls.eq(UserFollow.defaultAlias.userId, userId)).map(_.followId)
    val size = MicroPost.countBy(sqls.in(MicroPost.defaultAlias.userId, userId +: followingIds))
    SearchResult(pager, size)(findAllByWithLimitOffset(userId +: followingIds))
  }

  private def findAllByWithLimitOffset(userIds: Seq[Long])(pager: Pager[MicroPost])(
      implicit dbSession: DBSession
  ): Seq[MicroPost] = MicroPost.findAllByWithLimitOffset(
    sqls.in(MicroPost.defaultAlias.userId, userIds),
    pager.limit,
    pager.offset,
    pager.allSorters.map(_.toSQLSyntax(MicroPost.defaultAlias))
  )

}
