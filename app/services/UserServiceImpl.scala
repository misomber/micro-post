package services

import javax.inject.Singleton

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

}
