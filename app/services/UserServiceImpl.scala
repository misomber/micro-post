package services

import javax.inject.Singleton

import jp.t2v.lab.play2.pager.scalikejdbc._ // 明示的にインポートする
import jp.t2v.lab.play2.pager.{ Pager, SearchResult }
import models.User
import scalikejdbc.{ AutoSession, DBSession }

import scala.util.Try

@Singleton
class UserServiceImpl extends UserService {

  // ユーザーの作成に成功した場合は、Success(AUTO_INCREMENTによるID値を返します)
  def create(user: User)(implicit dbSession: DBSession = AutoSession): Try[Long] = Try {
    User.create(user)
  }

  def findByEmail(email: String)(implicit dbSession: DBSession = AutoSession): Try[Option[User]] =
    Try {
      User.where('email -> email).apply().headOption
    }

  override def findAll(pager: Pager[User])(implicit dbSession: DBSession): Try[SearchResult[User]] = Try {
    // 総件数を取得する
    val size = User.countAllModels()
    // SearchResultを生成する
    SearchResult(pager, size) { pager =>
      // Pagerに基づいて結果を返す
      User.findAllWithLimitOffset(
        pager.limit,
        pager.offset,
        pager.allSorters.map(_.toSQLSyntax(User.defaultAlias))
      )
    }
  }

  override def findById(id: Long)(implicit dbSession: DBSession): Try[Option[User]] = Try {
    User.findById(id)
  }

}
