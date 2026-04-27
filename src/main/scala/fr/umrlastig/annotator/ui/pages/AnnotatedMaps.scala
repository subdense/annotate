package fr.umrlastig.annotator.ui.pages

import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import fr.umrlastig.annotator.GitOps.{config, read}
import fr.umrlastig.annotator.Utils.*
import fr.umrlastig.annotator.ui.Page
import fr.umrlastig.annotator.{Model, Task_, Utils, rnd}
import org.scalajs.dom.{HTMLDivElement, HTMLElement}
import typings.geojson.mod.*
import typings.leaflet.mod as L
import typings.leaflet.mod.*
import typings.leaflet.mod.PathOptions.MutableBuilder
import typings.turfCentroid.mod.*

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.{JSON, Promise}

case class DatasetGeoJSON(name: String, wms1: String, wms2: String, features1: GeoJSON__[Geometry,GeoJsonProperties], features2: GeoJSON__[Geometry,GeoJsonProperties])

object AnnotatedMaps {
  private val globalMapVar: Var[Option[(Map_, Map_)]] = Var(None)
  // no need for Var in Controls
  private val datasetSelected: Var[String] = Var("")

  private enum Style:
    case None, Sample, Task, Types
  private val availableStyles = List(Style.None, Style.Sample, Style.Task, Style.Types)
  private val selectedStyleVar = Var(Style.None)
  private var currentLeftGroup: Option[LayerGroup_[GeoJsonProperties]] = None
  private var currentRightGroup: Option[LayerGroup_[GeoJsonProperties]] = None

  private def asFeatureCollection(text: String): FeatureCollection[Geometry, GeoJsonProperties] =
    JSON.parse(text).asInstanceOf[FeatureCollection[Geometry, GeoJsonProperties]]

  private def makeIcon(text: String, values: Array[(Double, String)], size: Double = 20) =
    val sum = values.map(_._1).sum
    divIcon(DivIconOptions().setHtml(div(
      b(text),
      if values.length > 1 then
        background("conic-gradient(" + values.foldLeft(
          (List[String](), 0.0))
          ((acc, tuple) => (acc._1 ++ List(s"${tuple._2} ${acc._2}deg", s"${tuple._2} ${acc._2 + tuple._1 * 360 / sum}deg"), acc._2 + tuple._1 * 360 / sum))._1.mkString(",") + ")")
      else
        backgroundColor(values.head._2),
      width(s"${size}px"),
      height(s"${size}px"),
      borderRadius(s"${size/2}px"),
      alignContent("center")
    ).ref).setIconSize(Point_(size, size)))

  private def makeSampleMarkers(fc: FeatureCollection[Geometry, GeoJsonProperties]): js.Array[Layer] = fc.features.groupBy(_.properties("sample")).map((sample, features) =>
    val coords = features.flatMap(_.geometry.asInstanceOf[MultiPolygon].coordinates(0)(0)).map(p => (p(1), p(0)))
    val (px, py) = ((coords.map(_._1).max + coords.map(_._1).min) / 2, (coords.map(_._2).max + coords.map(_._2).min) / 2)
    val annotators = features.map(_.properties("annotators").asInstanceOf[js.Array[String]])
    val (noAnnotation, oneAnnotation, twoAnnotations, moreAnnotations) = (annotators.count(_.isEmpty), annotators.count(_.length == 1), annotators.count(_.length == 2), annotators.count(_.length > 2))
    Marker_[GeoJsonProperties](js.Tuple3(px, py, js.undefined).asInstanceOf[LatLngTuple],
      MarkerOptions().setIcon(
        makeIcon(
          features.length.toString,
          Array(
            (noAnnotation.toDouble, "rgb(237,248,233)"),
            (oneAnnotation.toDouble, "rgb(186,228,179)"),
            (twoAnnotations.toDouble, "rgb(116,196,118)"),
            (moreAnnotations.toDouble, "rgb(35,139,69)"),
          ),
          size = 100
        )
      )
    )
      .bindPopup(layer => div(
        h4(sample.asInstanceOf[String].split("/").take(2).mkString("-")),
        p(b("no annotation: "), noAnnotation), p(b("one annotation: "), oneAnnotation),
        p(b("two annotations: "), twoAnnotations), p(b("more annotations: "), moreAnnotations)
      ).ref).asInstanceOf[Layer]
  ).toJSArray

  private def makeTaskMarkers(fc: FeatureCollection[Geometry, GeoJsonProperties]): js.Array[Layer] = fc.features.map(feature =>
    val annotators = feature.properties("annotators").asInstanceOf[js.Array[String]]
    val point = centroid[GeoJsonProperties](feature).geometry
    def getColor(nbAnnotators: Int): String =
      nbAnnotators match {
        case 0 => "rgb(215,48,39)"
        case 1 => "rgb(252,141,89)"
        case 2 => "rgb(254,224,139)"
        case 3 => "rgb(217,239,139)"
        case 4 => "rgb(145,207,96)"
        case _ => "rgb(26,152,80)"
      }
    Marker_[GeoJsonProperties](js.Tuple3(point.coordinates(1), point.coordinates(0), js.undefined).asInstanceOf[LatLngTuple],
      MarkerOptions().setIcon(makeIcon(annotators.length.toString, Array((0.toDouble, getColor(annotators.length)))))
    ).bindPopup(layer => div(
      h4("Annotations: " + annotators.length)
    ).ref)
  ).toJSArray

  private def makeTypesMarkers(colorMap: mutable.Map[String, String])(fc: FeatureCollection[Geometry, GeoJsonProperties]): js.Array[Layer] =
    fc.features.flatMap(feature =>
      val types = feature.properties("types").asInstanceOf[js.Array[String]]
      if types.isEmpty then
        Seq()
      else
        val typeMap = types.groupBy(identity).view.mapValues(_.size).toMap
        val maxOccurrences = typeMap.values.max
        //val count = typeMap.count(_._2==maxOccurrences)
        val mainTypeEntry = typeMap.find(_._2 == maxOccurrences)
        if mainTypeEntry.isEmpty then
          Seq()
        else
          val mainType = mainTypeEntry.get._1
          val point = centroid[GeoJsonProperties](feature).geometry
          val colorString = colorMap.getOrElseUpdate(mainType, getRGBColor)
          Seq(
            Marker_[GeoJsonProperties](js.Tuple3(point.coordinates(1), point.coordinates(0), js.undefined).asInstanceOf[LatLngTuple],
              MarkerOptions().setIcon(makeIcon(mainType, Array((0.toDouble, colorString)), size = 50))
            ).bindPopup(layer => div(List(h4("Main type: " + mainType)).appendedAll(typeMap.map(k=>p(b(k._1 + " :"), k._2.toString)))).ref)
          )
    ).toJSArray

  def renderGlobalDashboard()(implicit model: Model): Element =

    def taskFeatures(dateLeft: String, dateRight: String)(task: Task_): Promise[(js.Array[Feature[Geometry,GeoJsonProperties]],js.Array[Feature[Geometry,GeoJsonProperties]])] =
      //console.debug("taskFile: "+task.task.task)
      read[String](s"${config.dir}/${task.task.task}", false).`then`(content =>
        val newTask = content
        val features = asFeatureCollection(newTask).features
        features.foreach(f=>
          f.properties("task")=task.task.task
          f.properties("sample")=task.sample
          f.properties("annotators")=task.task.annotations.map(_.username)
          // TODO adapt with generic types
          f.properties("types")=task.task.annotations.map(_.types.head)
          //f.properties("linkTypes")=task.task.annotations.map(_.link)
          //f.properties("changeTypes")=task.task.annotations.map(_.change)
          // TODO use these again?
          //f.properties("quality")=task.task.annotations.map(_.quality)
          //f.properties("comments")=task.task.annotations.map(_.comment)
        )
        val groupedFeatures = features.groupBy(_.properties("date"))
        (groupedFeatures.getOrElse(dateLeft,js.Array()),groupedFeatures.getOrElse(dateRight,js.Array()))
      )
    //def username = model.stateVar.now().username
    val datasetGeoJSONVar: Var[js.Array[DatasetGeoJSON]] = Var(js.Array())

    // transforms the datasets into geojson datasets
    val datasetsProcessor = Observer[Option[js.Array[Task_]]] {
      case None =>
        datasetGeoJSONVar.set(js.Array())
      case Some(datasets) =>
        Promise.all[DatasetGeoJSON](
          datasets.toArray
            .groupBy(_.dataset)
            .map { case (d, arr) =>
              Promise.all[(js.Array[Feature[Geometry, GeoJsonProperties]], js.Array[Feature[Geometry, GeoJsonProperties]])](
                arr.map(taskFeatures(model.taskState.now().date1, model.taskState.now().date2)).toJSArray
              ).`then` { a =>
                val b = a.unzip
                DatasetGeoJSON(name = d.split("/").head, wms1 = arr.head.wmts(0), wms2 = arr.head.wmts(1),
                  features1 = L.GeoJSON__[Geometry, GeoJsonProperties](FeatureCollection(b._1.flatten.toJSArray)),
                  features2 = L.GeoJSON__[Geometry, GeoJsonProperties](FeatureCollection(b._2.flatten.toJSArray))
                )
              }
            }.toJSArray
        ).`then` { results =>
          datasetGeoJSONVar.set(results)
        }
    }
    model.datasetsVar.signal --> datasetsProcessor

    def updateGlobalMap(dataset: DatasetGeoJSON): Unit = {
      //console.debug(s"update global Maps for dataset ${dataset._1}")
      val colorMap = mutable.Map[String, String]()
      def styled(geojson: GeoJSON__[Geometry,GeoJsonProperties]):GeoJSON__[Geometry,GeoJsonProperties] = geojson.setStyle(f =>
        val feature = f.asInstanceOf[Feature[Geometry, GeoJsonProperties]]
        val c = colorMap.getOrElseUpdate(feature.properties("task").asInstanceOf[String], getColor)
        PathOptions().setColor(c)
      ).bindPopup(layer =>
        val feature = layer.feature.asInstanceOf[Feature[Geometry, GeoJsonProperties]]
        div(
          h4(feature.properties("task").asInstanceOf[String]),
          p(b("annotators: "), feature.properties("annotators").asInstanceOf[js.Array[String]].mkString(","))
        ).ref
      )
      val lGeoJSON = styled(dataset._4)
      val rGeoJSON = styled(dataset._5)
      globalMapVar.now().foreach((left, right) =>
        Utils.updateMaps(left, right, lGeoJSON, rGeoJSON, Some(dataset._2), Some(dataset._3))
      )
    }
    def chooseDataset(datasets: js.Array[DatasetGeoJSON], selectedName: String): Option[DatasetGeoJSON] =
        if (datasets.nonEmpty) {
          if (selectedName.isEmpty) datasets.headOption else datasets.find(_._1 == selectedName)
        } else None

    val mapUpdater = Observer[(js.Array[DatasetGeoJSON], String)] {
      case (datasets, selectedName) => chooseDataset(datasets, selectedName).foreach(updateGlobalMap)
    }

    def updateGroupStyle(dataset: DatasetGeoJSON, style: Style): Unit = {
      def removeGroups(l: Map_, r: Map_): Unit =
        currentLeftGroup.foreach(lg => l.removeLayer(lg))
        currentRightGroup.foreach(rg => r.removeLayer(rg))

      if style == Style.None then {
        globalMapVar.now().foreach { (l, r) => removeGroups(l, r) }
        currentLeftGroup = None
        currentRightGroup = None
        return
      }
      val leftFC:FeatureCollection[Geometry, GeoJsonProperties] = dataset._4.toGeoJSON().asInstanceOf[FeatureCollection[Geometry, GeoJsonProperties]]
      val rightFC:FeatureCollection[Geometry, GeoJsonProperties] = dataset._5.toGeoJSON().asInstanceOf[FeatureCollection[Geometry, GeoJsonProperties]]
      def setMarkers(markerFunction: FeatureCollection[Geometry, GeoJsonProperties] => js.Array[Layer]): Unit = {
        val left: LayerGroup_[GeoJsonProperties] = layerGroup(markerFunction(leftFC))
        val right: LayerGroup_[GeoJsonProperties] = layerGroup(markerFunction(rightFC))
        globalMapVar.now().foreach { (l, r) =>
          removeGroups(l, r)
          left.addTo(l)
          right.addTo(r)
          currentLeftGroup = Some(left)
          currentRightGroup = Some(right)
        }
      }
      style match {
        case Style.Sample => setMarkers(makeSampleMarkers)
        case Style.Task => setMarkers(makeTaskMarkers)
        case Style.Types =>
          val colorMap = mutable.Map[String, String]()
          setMarkers(makeTypesMarkers(colorMap))
        case Style.None =>
      }
    }
    end updateGroupStyle

    val mapStyleUpdater = Observer[(js.Array[DatasetGeoJSON], String, Style)] {
      case (datasets, selectedName, selectedStyle) =>
        chooseDataset(datasets, selectedName).foreach(d => updateGroupStyle(d, selectedStyle))
    }

    datasetGeoJSONVar.signal.combineWith(datasetSelected.signal) --> mapUpdater
    datasetGeoJSONVar.signal.combineWith(datasetSelected.signal).combineWith(selectedStyleVar.signal) --> mapStyleUpdater

    div(
      h1(Page.AnnotatedMaps.name),
      label("Dataset: ",forId("datasetSelect")),
      select(idAttr("datasetSelect"),
        children <-- datasetGeoJSONVar.signal.map(l=>l.map(p=>option(p._1).amend(selected(p._1 == datasetSelected.now())))),
        onChange.mapToValue --> datasetSelected,
        datasetGeoJSONVar.signal.combineWith(datasetSelected.signal) --> mapUpdater,
      ),
      label("Display: ",forId("styleSelect")),
      select(idAttr("styleSelect"),
        value <-- selectedStyleVar.signal.map(_.toString),
        onChange.mapToValue.map(v => Style.valueOf(v)) --> selectedStyleVar,
        availableStyles.map { step => option(value := step.toString, step.toString) },
        datasetGeoJSONVar.signal.combineWith(datasetSelected.signal).combineWith(selectedStyleVar.signal) --> mapStyleUpdater
      ),
      div(
        datasetGeoJSONVar.signal.combineWith(datasetSelected.signal) --> mapUpdater,
        // Wait for the component to be mounted before adding the leaflet and syncs
        onMountCallback(ctx =>
          Utils.syncMaps("globalMapLeft", "globalMapRight", globalMapVar)
        ),
        div(idAttr("global-container"), cls("my-container"),
          div(idAttr("globalMapLeft"), cls("mapLeft"), cls("map"),
          ),
          div(idAttr("globalMapRight"), cls("mapRight"), cls("map"),
          )
        ),
      ),
      model.datasetsVar.signal --> datasetsProcessor,
    )
  end renderGlobalDashboard
}
