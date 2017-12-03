package controllers

import java.time.ZonedDateTime
import javax.inject.{ Inject, Singleton }

import com.github.t3hnar.bcrypt._
import forms.SignUp
import models.User
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.{ I18nSupport, Messages, MessagesApi }
import play.api.mvc._
import play.api.{ Configuration, Logger }
import services.UserService

@Singleton
class SignUpController @Inject()(
    userService: UserService,
    val messagesApi: MessagesApi,
    config: Configuration
) extends Controller
    with I18nSupport {

  // Configuration#getStringメソッドを使って、conf/application.confからパスワード暗号化用のサルトを読み込みます。
  private val salt = config.getString("password.salt").get

  /**
    * ビューのサインアップフォームの内容を受け取れるように、次のとおりに定義しています。
    * verifyingメソッドは、フォームで入力された値の独自検証が行えます。
    * ここでは、入力された2つのパスワードの検証を行っています。
    * 検証失敗時のメッセージも指定できます。
    */
  private val signUpForm: Form[SignUp] = Form {
    mapping(
      "name"     -> nonEmptyText,
      "email"    -> email,
      "password" -> nonEmptyText,
      "confirm"  -> nonEmptyText
    )(SignUp.apply)(SignUp.unapply)
      .verifying(Messages("PasswordInvalid"), form => form.password == form.confirm)
  }

  /**
    * HomeControllerとほぼ同じロジックですが、初期化されたフォームをビューの引数に渡す必要があります。
    * @return
    */
  def index: Action[AnyContent] = Action { implicit request =>
    Ok(views.html.signup(signUpForm))
  }

  def register: Action[AnyContent] = Action { implicit request =>
    signUpForm
    // Requestオブジェクトからフォームに割り当てるためのメソッド
    // フォームのバリデーションや値の取り出しできます。フォームの成功ケースでは、
    // フォーム(SingUp)の内容をUserモデルに変換し、UserService#createを使って登録します。
      .bindFromRequest()
      .fold(
        formWithErrors => BadRequest(views.html.signup(formWithErrors)), { signUp =>
          val now            = ZonedDateTime.now()
          val hashedPassword = signUp.password.bcrypt(salt)
          val user           = User(None, signUp.name, signUp.email, hashedPassword, now, now)
          userService
            .create(user)
            .map { _ =>
              Redirect(routes.HomeController.index())
                .flashing("success" -> Messages("SignUpSucceeded"))
            }
            .recover {
              case e: Exception =>
                Logger.error(s"occurred error", e)
                Redirect(routes.HomeController.index())
                  .flashing("failure" -> Messages("InternalError"))
            }
            .getOrElse(InternalServerError(Messages("InternalError")))
        }
      )
  }

}
