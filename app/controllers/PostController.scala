package controllers

import java.time.ZonedDateTime
import javax.inject.{ Inject, Singleton }

import jp.t2v.lab.play2.auth.AuthenticationElement
import jp.t2v.lab.play2.pager.Pager
import models.MicroPost
import play.api.Logger
import play.api.data.Forms._
import play.api.data._
import play.api.i18n.{ I18nSupport, Messages, MessagesApi }
import play.api.mvc._
import services._

@Singleton
class PostController @Inject()(
    val userService: UserService,
    val microPostService: MicroPostService,
    val messagesApi: MessagesApi
) extends Controller
    with I18nSupport
    with AuthConfigSupport
    with AuthenticationElement {

  private val postForm = Form {
    "content" -> nonEmptyText
  }

  def post(pager: Pager[MicroPost]): Action[AnyContent] = StackAction { implicit request =>
    val user = loggedIn
    postForm
      .bindFromRequest()
      .fold(
        { formWithErrors =>
          handleError(pager, user, formWithErrors)
        }, { content =>
          createMicroPost(pager, user, content)
        }
      )
  }

  private def createMicroPost(pager: Pager[MicroPost], user: User, content: String) = {
    val now       = ZonedDateTime.now
    val microPost = MicroPost(None, user.id.get, content, now, now)
    microPostService
      .create(microPost)
      .map { _ =>
        Redirect(routes.HomeController.index(pager))
      }
      .recover {
        case e: Exception =>
          Logger.error("occurred error", e)
          Redirect(routes.HomeController.index(Pager.default))
            .flashing("failure" -> Messages("InternalError"))
      }
      .getOrElse(InternalServerError(Messages("InternalError")))
  }

  private def handleError(
      pager: Pager[MicroPost],
      user: User,
      formWithErrors: Form[String]
  )(implicit request: RequestHeader) = {
    microPostService
      .findAllByWithLimitOffset(pager, user.id.get)
      .map { searchResult =>
        BadRequest(views.html.index(Some(user), formWithErrors, searchResult))
      }
      .recover {
        case e: Exception =>
          Logger.error("occurred error", e)
          Redirect(routes.HomeController.index(Pager.default))
            .flashing("failure" -> Messages("InternalError"))
      }
      .getOrElse(InternalServerError(Messages("InternalError")))
  }

  def delete(microPostId: Long, pager: Pager[MicroPost]): Action[AnyContent] = StackAction { implicit request =>
    microPostService
      .deleteById(microPostId)
      .map { _ =>
        Redirect(routes.HomeController.index(pager))
      }
      .recover {
        case e: Exception =>
          Logger.error("occurred error", e)
          Redirect(routes.HomeController.index(Pager.default))
            .flashing("failure" -> Messages("InternalError"))
      }
      .getOrElse(InternalServerError(Messages("InternalError")))
  }

}
