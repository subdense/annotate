package fr.umrlastig.annotator.ui.pages

import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import fr.umrlastig.annotator.ui.Page
import fr.umrlastig.annotator.{Model, UserState}
import org.scalajs.dom.{HTMLDivElement, HTMLElement}

object Login {
  private def renderInputRow(error: UserState => Option[String])(mods: Modifier[HtmlElement]*)(implicit model: Model): HtmlElement = {
    val errorSignal = model.stateVar.signal.map(_.displayError(error))
    div(
      cls("-inputRow"),
      cls("x-hasError") <-- errorSignal.map(_.nonEmpty),
      mods,
      child.maybe <-- errorSignal.map(_.map(err => div(cls("-error"), err)))
    )
  }

  def renderLogin()(implicit model: Model): Element =
    div(
      h1(Page.Login.name),
      form(
        onSubmit.preventDefault.mapTo(model.stateVar.now()) --> model.submitter,
        renderInputRow(_.usernameError)(
          label("Username: "),
          input(
            placeholder("YOUR-USERNAME"),
            controlled(
              value <-- model.stateVar.signal.map(_.username),
              onInput.mapToValue --> model.usernameWriter
            )
          ),
          button(
            typ("button"), // "submit" is the default in HTML
            "Clear",
            onClick.mapTo("") --> model.usernameWriter
          )
        ),
        renderInputRow(_.tokenError)(
          label("Token: "),
          input(
            typ("password"),
            placeholder("YOUR-PERSONAL-ACCESS-TOKEN"),
            controlled(
              value <-- model.stateVar.signal.map(_.token),
              onInput.mapToValue --> model.tokenWriter
            )
          ),
          button(
            typ("button"), // default button type in HTML is "submit", we don't want it
            "Clear",
            onClick.mapTo("") --> model.tokenWriter
          )
        ),
        button(typ("submit"), "Submit")
      ),
      p("To create one, refer to ",a("Token creation",href("https://github.com/settings/tokens/new")),". Your account has to be linked to the ",a("SUBDENSE organisation",href("https://github.com/subdense"))," and the token need the ",b("repo")," rights.")
    )
  end renderLogin

}
