package fr.umrlastig.annotator.ui

import com.raquo.laminar.api.L.*
import fr.umrlastig.annotator.Model
import fr.umrlastig.annotator.ui.pages.{Annotate, AnnotatedMaps, Dashboard, Help, Home, Leaderboard, Login}
import org.scalajs.dom.console

object Layout {
  private def header()(implicit model: Model): Element =
    div(
      child.maybe <-- model.errorMessage.signal.map {
        case Some(msg) =>
          Some(
            div(
              cls("error-banner"),
              //style("background-color: #ffebee; color: #c62828; padding: 10px; margin-bottom: 10px; border-radius: 4px; text-align: center;"),
              msg,
              button(onClick --> { _ => model.errorMessage.update(_ => None) }, "×")
            )
          )
        case None => None
      },
      navTag(
        menuTag(
          li(
            button(onClick --> { _ => model.currentPage.update(_ => Page.Home) }, Page.Home.name)
          ),
          li(
            button(
              onClick --> { _ => model.currentPage.update(_ => Page.Dashboard) },
              disabled <-- model.datasetsVar.signal.map(_.isEmpty),
              Page.Dashboard.name
            )
          ),
          li(
            button(
              onClick --> { _ => model.currentPage.update(_ => Page.Leaderboard) },
              disabled <-- model.datasetsVar.signal.map(_.isEmpty),
              Page.Leaderboard.name
            )
          ),
          li(
            button(
              onClick --> { _ => model.currentPage.update(_ => Page.AnnotatedMaps) },
              disabled <-- model.datasetsVar.signal.map(_.isEmpty),
              Page.AnnotatedMaps.name
            )
          ),
          li(
            button(
              onClick --> { _ => model.currentPage.update(_ => Page.Annotate) },
              disabled <-- model.datasetsVar.signal.map(_.isEmpty).combineWithFn(model.annotationFinished.signal)((a, b) => a || b),
              Page.Annotate.name
            )
          ),
          li(
            button(
              onClick --> { _ => model.logInOut() },
              text <-- model.stateVar.signal.map(l => if l.validated then "Logout" else Page.Login.name)
            )
          ),
          li(
            button(
              onClick --> { _ => model.currentPage.update(_ => Page.Help) },
              Page.Help.name
            )
          )
        ),
/*
        div(img(width("60px"), height("60px"), src("Loading_2.gif")),
          display <-- model.stateVar.signal.map(_.validated).combineWithFn(model.datasetsVar.signal.map(_.isDefined))((a: Boolean, b: Boolean) => if !a || b then "none" else "initial")
        ),
*/
        div(
          progressTag(
            idAttr("progress-bar"),
            aria.label("loading")
          ),
          text <-- model.currentMessage.signal.map(m=>m.getOrElse("")),
          display <-- model.stateVar.signal.map(_.validated).combineWithFn(model.datasetsVar.signal.map(_.isDefined))((a: Boolean, b: Boolean) => if !a || b then "none" else "initial")
        )
      )
    )
  end header

  def appElement()(using model: Model): Element =
    div(
      header(),
      child <-- model.currentPage.signal.splitOne(x => x) { (id, initial, signal) =>
        console.info(s"Split ${id.name}")
        id match {
          case Page.Home => Home.renderHome()
          case Page.Dashboard => Dashboard.renderDashboard()
          case Page.Leaderboard => Leaderboard.renderLeaderboard()
          case Page.AnnotatedMaps => AnnotatedMaps.renderGlobalDashboard()
          case Page.Annotate => Annotate.renderAnnotate()
          case Page.Login => Login.renderLogin()
          case Page.Help => Help.renderHelp()
        }
      }
    )
  end appElement
}