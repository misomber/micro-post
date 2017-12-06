package controllers

import javax.inject._

import jp.t2v.lab.play2.auth.OptionalAuthElement
import jp.t2v.lab.play2.pager.{ Pager, SearchResult }
import models.MicroPost
import play.api.Logger
import play.api.data.Forms._
import play.api.data._
import play.api.i18n.{ I18nSupport, Messages, MessagesApi }
import play.api.mvc._
import services.{ MicroPostService, UserService }

@Singleton
class HomeController @Inject()(val userService: UserService,
                               val microPostService: MicroPostService,
                               val messagesApi: MessagesApi)
    extends Controller
    with I18nSupport
    with AuthConfigSupport
    with OptionalAuthElement {

  private val postForm = Form {
    "content" -> nonEmptyText
  }

  def index(pager: Pager[MicroPost]): Action[AnyContent] = StackAction { implicit request =>
    val userOpt = loggedIn
    userOpt
      .map { user =>
        microPostService
          .findAllByWithLimitOffset(pager, user.id.get)
          .map { searchResult =>
            Ok(views.html.index(userOpt, postForm, searchResult))
          }
          .recover {
            case e: Exception =>
              Logger.error(s"occurred error", e)
              Redirect(routes.HomeController.index(Pager.default))
                .flashing("failure" -> Messages("InternalError"))
          }
          .getOrElse(InternalServerError(Messages("InternalError")))
      }
      .getOrElse(
        Ok(views.html.index(userOpt, postForm, SearchResult(pager, 0)(_ => Seq.empty[MicroPost])))
      )
  }

}
