package controllers

import javax.inject.{ Inject, Singleton }

import com.github.t3hnar.bcrypt._
import forms.Login
import jp.t2v.lab.play2.auth.LoginLogout
import play.api.Logger
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.{ I18nSupport, Messages, MessagesApi }
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc._
import services.UserService

import scala.concurrent.Future

@Singleton
class AuthController @Inject()(val userService: UserService, val messagesApi: MessagesApi)
    extends Controller
    with I18nSupport
    with AuthConfigSupport
    with LoginLogout {

  /**
    * verifyingメソッドでは、ユーザー認証処理(authenticateメソッド)を呼び出します。
    * 認証に失敗した際は、Messages("AuthFailed")がフォームのグローバルエラーとして返されます。
    */
  private val loginForm: Form[Login] = Form {
    mapping(
      "email"    -> email,
      "password" -> nonEmptyText
    )(Login.apply)(Login.unapply)
      .verifying(Messages("AuthFailed"), form => authenticate(form.email, form.password).isDefined)
  }

  /**
    * remembermeFormはログインセッションを記憶するためのフォームです。
    * 属性はチェックボックスを想定しているので、フォームの値形式はbooleanとなります。
    * このフォームはログインボタンを押した時にloginFormと同時に受け取ることができます。
    * loginアクション内で参照できます。
    */
  private val rememberMeForm: Form[Boolean] = Form {
    "rememberme" -> boolean
  }

  /**
    * ログイン画面を描画するためのアクションです。
    * loginFormの初期値は空ですが、remembermeFormには、ログインセッション上のremembermeがtrueであれば、
    * Boolean型のtrueを設定します。それ以外はfalseとなります。
    * @return
    */
  def index: Action[AnyContent] = Action { implicit request =>
    Ok(
      views.html.auth.login(loginForm, rememberMeForm.fill(request.session.get("rememberme").exists("true" ==)))
    )
  }

  def login: Action[AnyContent] = {
    Action.async { implicit request =>
      val rememberMe = rememberMeForm.bindFromRequest()
      loginForm.bindFromRequest.fold(
        formWithErrors => Future.successful(BadRequest(views.html.auth.login(formWithErrors, rememberMe))), { login =>
          val req = request.copy(tags = request.tags + ("rememberme" -> rememberMe.get.toString))
          gotoLoginSucceeded(login.email)(req, defaultContext)
            .map(_.withSession("rememberme" -> rememberMe.get.toString))
            .map(_.flashing("success" -> Messages("LoggedIn")))
        }
      )
    }
  }

  def logout: Action[AnyContent] = Action.async { implicit request =>
    gotoLogoutSucceeded.map(
      _.flashing("success" -> Messages("LoggedOut"))
        .removingFromSession("rememberme")
    )
  }

  private def authenticate(email: String, password: String): Option[User] = {
    userService
      .findByEmail(email)
      .map { user =>
        user.flatMap { u =>
          if (password.isBcrypted(u.password))
            user
          else
            None
        }
      }
      .get
  }

}
