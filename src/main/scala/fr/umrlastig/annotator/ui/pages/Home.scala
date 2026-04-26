package fr.umrlastig.annotator.ui.pages

import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import fr.umrlastig.annotator.ui.Page
import org.scalajs.dom.{HTMLDivElement, HTMLElement}

object Home {
  def renderHome(): Element =
    div(
      h1(Page.Home.name),
      // TODO load from generic description file (md, Html etc.)
      p("This is an annotation app for the SUBDENSE project"),
      div(
        h2("How does this work?"),
        p("Once you login, you will be able to:"),
        ul(
          listStyleType("none"),
          li(b("Annotate")," building data"),
          li("Check you progress in ",b("Dashboard")),
          li("Check the overall progress in ",b("Annotated Maps")),
        ),
        "The ",b("Help")," section will help you with the different cases of change to be annotated."
      )
    )
  end renderHome

}
