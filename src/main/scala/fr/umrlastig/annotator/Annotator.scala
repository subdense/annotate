package fr.umrlastig.annotator

import com.raquo.laminar.api.L.{*, given}
import fr.umrlastig.annotator.Config.{leftLeftStyle, rightRightStyle}
import fr.umrlastig.annotator.GitOps.{cloneData, config, read}
import fr.umrlastig.annotator.ui.{Layout, Page}
import org.scalajs.dom
import org.scalajs.dom.console
import org.scalajs.dom.window.alert
import typings.geojson.mod.*
import typings.leaflet.mod as L
import typings.leaflet.mod.*

import scala.annotation.tailrec
import scala.language.{implicitConversions, postfixOps}
import scala.scalajs.js
import scala.scalajs.js.{JSON, Promise}
import scala.util.Random

@main
def Annotator(): Unit =
  renderOnDomContentLoaded(
    dom.document.getElementById("app"),
    Main.appElement()
  )
end Annotator

object Main:
  private val model = new Model

  def appElement(): Element = Layout.appElement()(using model)

end Main

case class UserRank(username: String, count: Int, percentage: Double)

case class UserState(username: String = "", token: String = "", showErrors: Boolean = false, validated: Boolean = false) {
  def hasErrors: Boolean = usernameError.nonEmpty || tokenError.nonEmpty

  def usernameError: Option[String] = if (username.nonEmpty) None else Some("Username must not be empty.")

  def tokenError: Option[String] = if (token.length == 40) None else Some("Token must consist of 40 character long.")

  def displayError(error: UserState => Option[String]): Option[String] = error(this).filter(_ => showErrors)
}

final class Model {
  val currentPage: Var[Page] = Var(Page.Home)
  val geoJSON: Var[Option[(GeoJSON__[Geometry, GeoJsonProperties], GeoJSON__[Geometry, GeoJsonProperties])]] = Var(None)
  case class TaskState(date1: String="", date2: String="", wms1: String="", wms2: String="", sampleFile: String="", taskFile:String="", modalities: Seq[Seq[String]] = Seq.empty)
  val taskState: Var[TaskState] = Var(TaskState())
  val datasetsVar: Var[Option[js.Array[Task_]]] = Var(None)
  private var iterator: Option[Iterator[Task_]] = None
  // map variables
  val annotationMapVar: Var[Option[(Map_, Map_)]] = Var(None)
  var globalMapVar: Var[Option[(Map_, Map_)]] = Var(None)
  // no need for Var in Controls
  var globalMapLeftControl: Option[Control_.Layers] = None
  var globalMapRightControl: Option[Control_.Layers] = None
  val annotationFinished: Var[Boolean] = Var(false)
  //case class AnnotationState(linkType: String = "", changeType: String = "", quality: Boolean = false, comment: String = "", step: Int = 0)
  case class AnnotationState(types: Seq[String] = Seq.empty[String], quality: Boolean = false, comment: String = "", step: Int = 0)
  val annotationState: Var[AnnotationState] = Var(AnnotationState())
  val datasetSelected: Var[String] = Var("")
  val errorMessage: Var[Option[String]] = Var(None)
  val currentMessage: Var[Option[String]] = Var(None)

  val leaderboardData: Var[List[UserRank]] = Var(List.empty)

  private def localStorageGetOrElse(v: String): String = if dom.window.localStorage.hasOwnProperty(v) then dom.window.localStorage.getItem(v) else ""

  val stateVar: Var[UserState] = {
    val (username, token) = (localStorageGetOrElse("username"), localStorageGetOrElse("token"))
    Var(UserState(username,token))
  }

  private def geoJSONUpdate(left:GeoJSON__[Geometry,GeoJsonProperties], right:GeoJSON__[Geometry,GeoJsonProperties]): Unit =
    geoJSON.update(previous =>
      previous match {
        case Some((previousLeft, previousRight)) =>
          annotationMapVar.now().foreach((l,r)=>
            previousLeft.removeFrom(l)
            previousRight.removeFrom(r)
          )
        case None =>
          console.info(s"previous was None")
      }
      Some((left, right))
    )

  @tailrec
  def nextFeature(): Promise[Boolean] =
    def parseGeoJSONAndFilter(text:String, date: String) =
      GeoJSON__(JSON.parse(text).asInstanceOf[GeoJsonObject],
        GeoJSONOptions().setFilter((feature:Feature[Geometry, GeoJsonProperties])=>feature.properties("date") == date)).asInstanceOf[GeoJSON__[Geometry,GeoJsonProperties]]

    if iterator.isDefined && iterator.get.hasNext then
      val currentTask_ = iterator.get.next()
      if currentTask_.task.annotations.exists(_.username == stateVar.now().username) then {
        console.info(s"Already annotated by ${stateVar.now().username}")
        nextFeature()
      }
      else
        taskState.update(state => state.copy(
          date1 = currentTask_.dates(0), date2 = currentTask_.dates(1),
          wms1 = currentTask_.wmts(0), wms2 = currentTask_.wmts(1),
          sampleFile = currentTask_.sample, taskFile = currentTask_.task.task,
          modalities = currentTask_.modalities.map(_.toSeq).toSeq
        ))
        annotationState.update(_ => AnnotationState())
        read[String](s"${config.dir}/${currentTask_.task.task}", false)
          .`then`(content =>
            val newTask = content
            val left = parseGeoJSONAndFilter(newTask, currentTask_.dates(0)).setStyle(leftLeftStyle)
            val right = parseGeoJSONAndFilter(newTask, currentTask_.dates(1)).setStyle(rightRightStyle)
            geoJSONUpdate(left, right)
            true
          )
          .`catch`(err =>
            console.error(s"Reading failed for: ${config.dir}/${currentTask_.task.task}", err)
            false
          )
    else {
      console.info("No task left")
      Promise.resolve(false)
    }
  val tokenWriter: Observer[String] = stateVar.updater[String]((state, token) => state.copy(token = token))
  val usernameWriter: Observer[String] = stateVar.updater[String]((state, username) => state.copy(username = username))
  val submitter: Observer[UserState] = Observer[UserState] { state =>
    if (state.hasErrors) {
      stateVar.update(_.copy(showErrors = true))
    } else {
      dom.window.localStorage.setItem("token", state.token)
      dom.window.localStorage.setItem("username", state.username)
      val rng = new Random(state.username.hashCode)
      stateVar.update(s => s.copy(validated = true))
      cloneData(state.token, rng, currentMessage)
        .`then`(datasets =>
          datasetsVar.update(_ => Some(datasets))
          iterator = Some(datasets.iterator)
          currentPage.update(_ => Page.Home)
          nextFeature()
        )
        .`catch`(err => {
          console.error("Git clone failed!", err)
          errorMessage.update(_ => Some(s"Failed to clone data: ${err.toString}"))
          stateVar.update(_.copy(validated = false))
          // Show a notification to the user
          alert("Failed to clone data. Check console for details.")
          //currentPage.update(_ => Page.Login)
        })
    }
  }

  def logInOut(): Unit = {
    if (stateVar.now().validated) {
      dom.window.localStorage.removeItem("token")
      dom.window.localStorage.removeItem("username")
      currentPage.update(_ => Page.Home)
      stateVar.update(_.copy(validated = false))
    } else {
      currentPage.update(_ => Page.Login)
    }
  }
}
