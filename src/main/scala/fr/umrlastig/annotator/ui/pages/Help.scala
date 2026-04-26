package fr.umrlastig.annotator.ui.pages

import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import fr.umrlastig.annotator.ui.Page
import org.scalajs.dom.HTMLDivElement

object Help {
  def renderHelp(): ReactiveHtmlElement[HTMLDivElement] =
    div(
      h1(Page.Help.name),
      p("Here are a few examples to help you annotate the changes."),
      h2("No change"),
      p("Buildings stay the same in aerial view 2011 and 2021."),
      img(src("noChange.png")),
      h2("Construction"),
      p("Building is constructed on land where in 2011 there was not building."),
      img(src("construction.png")),
      h2("Destruction"),
      p("Building that in 2011 existed is destructed in 2021 and no new building is constructed (yet)."),
      img(src("destruction.png")),
      h2("Reconstruction"),
      p("Building from 2011 is destructed and a new building in 2021 is constructed."),
      img(src("reconstruction.png")),
      h2("I don't know"),
      p("Any case which does not fit into the types above or if the aerial image is too bad to judge.")
    )
  end renderHelp

}
