package fr.umrlastig.annotator.ui.pages

import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import fr.umrlastig.annotator.GitOps.*
import fr.umrlastig.annotator.ui.Page
import fr.umrlastig.annotator.{Model, Sample, Types, Utils}
import org.scalajs.dom
import org.scalajs.dom.window.alert
import org.scalajs.dom.{HTMLDivElement, console}

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.JSON

object Annotate {

  private def annotationType(index: Int)(implicit model: Model): Signal[String] =
    model.annotationState.signal.map { s => if (s.types.isEmpty) "" else s.types(index) }

  private def typeElement(name: String, index: Int, n: Int)(implicit model: Model): Element =
    // TODO add special cases for label, boolean input and text input (quality issue, comment)
    button(name, typ("button"),
      backgroundColor <-- annotationType(index).map(t => if t == name then "Silver" else "WhiteSmoke"),
      onClick --> { _ =>
        model.annotationState.update(_.copy(
          types = {
            val prevTypes = model.annotationState.now().types
            if (prevTypes.isEmpty) Seq.fill(n)("").updated(index, name)
            else prevTypes.updated(index, name)
          },
          step = index))
        console.info(s"types: ${model.annotationState.now().types}")
      }
    )

  // TODO index is always < max size by use but not secure
  private def nextButton(index: Int)(implicit model: Model): Element =
    button(">", backgroundColor := "Orchid",
      disabled <-- annotationType(index).map(_.isEmpty),
      onClick --> { _ => model.annotationState.update(_.copy(step = index + 1)) }
    )

  // TODO index is always >= 1 by use of the function but this is not secure without a check
  private def backButton(index: Int)(implicit model: Model): Element =
    button("<", backgroundColor := "Orchid", onClick --> { _ => model.annotationState.update(_.copy(step = index - 1)) })

  private def saveButton(implicit model: Model): Element = button("save",
    backgroundColor := "Crimson",
    disabled <-- model.annotationState.signal.map(_.types.isEmpty),
    onClick --> saveAnnotation
  )

  private def saveAnnotation(event: dom.MouseEvent)(implicit model: Model): Unit = {
    val currentAnnotationState = model.annotationState.now()
    val currentTaskState = model.taskState.now()
    val currentUserState = model.stateVar.now()
    console.info(s"save: annotationState: $currentAnnotationState")
    val sampleFile = currentTaskState.sampleFile
    val taskFile = currentTaskState.taskFile
    gitPull(currentUserState.username, currentUserState.token)
      .`then`(_ => {
        read[Sample](s"${config.dir}/$sampleFile").`then`(content => {
          val task = content.tasks.find(_.task == taskFile).getOrElse(
            throw new NoSuchElementException(s"Task $taskFile not found in $sampleFile")
          )
          val annotation = Types.newAnnotation()
          annotation.username = currentUserState.username
          //annotation.link = currentAnnotationState.linkType
          //annotation.change = currentAnnotationState.changeType
          annotation.types = currentAnnotationState.types.toJSArray
          annotation.quality = currentAnnotationState.quality
          annotation.comment = currentAnnotationState.comment
          task.annotations.push(annotation)
          write(s"${config.dir}/$sampleFile", JSON.stringify(content, space = 2))
            .`then`(_ => gitPush(currentUserState.username, currentUserState.token, sampleFile, s"Update $taskFile for ${annotation.username}"))
            .`then`(_ => {
              model.datasetsVar.update(datasets => datasets.map(_.map {
                t =>
                  if (t.task.task == taskFile) {
                    val newTask = Types.newTask_()
                    newTask.dataset = t.dataset
                    newTask.dates = t.dates
                    newTask.wmts = t.wmts
                    newTask.sample = t.sample
                    newTask.modalities = t.modalities
                    newTask.task = Types.newTask()
                    newTask.task.task = taskFile
                    newTask.task.annotations = task.annotations
                    newTask
                  } else t
              }))

              model.nextFeature().`then`(updated => {
                if updated then
                  model.annotationMapVar.now().foreach((l, r) => Utils.updateMaps(l, r, model.geoJSON.now().get._1, model.geoJSON.now().get._2))
                  model.currentPage.update(_ => Page.Annotate)
                else
                  model.annotationFinished.update(_ => true)
                  model.currentPage.update(_ => Page.Dashboard)
              })
            })
            .`catch`(err => {
              console.error("Save operation failed!", err)
              // Show a notification to the user
              model.errorMessage.update(_ => Some(s"Error: ${err.toString}"))
              alert("Failed to save annotation. Check console for details.")
            })
          })
          .`catch`(err => {
            console.error("Failed to read sample file", err)
            // Show a notification to the user
            model.errorMessage.update(_ => Some(s"Error: ${err.toString}"))
            alert("Could not load sample data.")
          })
      })
      .`catch`(err => {
        console.error("Pull or read failed:", err)
        model.errorMessage.update(_ => Some(s"Error: ${err.toString}"))
        alert("Failed to sync with server. Please refresh and try again.")
      })
  }

  def renderAnnotate()(implicit model: Model): Element =
    // generic scheme : children( prev button if not first, [... typeButtons], next button if not last, save button if last) <-- annotationState.signal.map(_.step== INDEX )
    val modalities = model.taskState.now().modalities
    console.info(s"Modalities for task : $modalities")
    val toolbar = modalities.zipWithIndex.map{ case (s,i) =>
      val n = modalities.length
      val elements = if (n==1) {s.map(typeElement(_, i, n))++Seq(saveButton)} else {
        (if (i>0) Seq(backButton(i)) else Seq.empty)++s.map(typeElement(_, i, n))++(if (i<(n-1)) Seq(nextButton(i)) else Seq.empty)++(if (i==(n-1)) Seq(saveButton) else Seq.empty)
      }
      children(elements)  <-- model.annotationState.signal.map(_.step==i)
    }
    div(
      // Wait for the component to be mounted before adding the leaflet and syncs
      onMountCallback(ctx =>
        val (l, r) = Utils.syncMaps("mapLeft", "mapRight", model.annotationMapVar)
        Utils.updateMaps(l,r,model.geoJSON.now().get._1,model.geoJSON.now().get._2,Some(model.taskState.now().wms1),Some(model.taskState.now().wms2))
      ),
      div(idAttr("my-container"), cls("my-container"),
        div(idAttr("mapLeft"), cls("mapLeft"), cls("map"),
        ),
        div(idAttr("mapRight"), cls("mapRight"), cls("map"),
        )
      ),
      div(cls("toolbar")).amend(toolbar: _*)
    )
  end renderAnnotate

}
