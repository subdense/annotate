package fr.umrlastig.annotator.ui.pages

import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import fr.umrlastig.annotator.GitOps.{config, read}
import fr.umrlastig.annotator.Utils.*
import fr.umrlastig.annotator.ui.Page
import fr.umrlastig.annotator.{Model, Task_, Utils}
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


object AnnotatedMaps {
  private def asFeatureCollection(text: String): FeatureCollection[Geometry, GeoJsonProperties] =
    JSON.parse(text).asInstanceOf[FeatureCollection[Geometry, GeoJsonProperties]]

  private def makeIcon(text: String, values: Array[(Double, String)]) =
    val sum = values.map(_._1).sum
    divIcon(DivIconOptions().setHtml(div(
      b(text),
      background("conic-gradient(" + values.foldLeft((List[String](), 0.0))((acc, tuple) => (acc._1 ++ List(s"${tuple._2} ${acc._2}deg", s"${tuple._2} ${acc._2 + tuple._1 * 360 / sum}deg"), acc._2 + tuple._1 * 360 / sum))._1.mkString(",") + ")"),
      width(s"${sum * 2}px"),
      height(s"${sum * 2}px"),
      borderRadius(s"${sum}px"),
      alignContent("center")
    ).ref).setIconSize(Point_(sum * 2, sum * 2)))

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
          //f.properties("linkTypes")=task.task.annotations.map(_.link)
          //f.properties("changeTypes")=task.task.annotations.map(_.change)
          f.properties("quality")=task.task.annotations.map(_.quality)
          f.properties("comments")=task.task.annotations.map(_.comment)
        )
        val groupedFeatures = features.groupBy(_.properties("date"))
        (groupedFeatures.getOrElse(dateLeft,js.Array()),groupedFeatures.getOrElse(dateRight,js.Array()))
      )
    def username = model.stateVar.now().username
    val datasetVar: Var[js.Array[(String,String,String,GeoJSON__[Geometry,GeoJsonProperties],GeoJSON__[Geometry,GeoJsonProperties])]] = Var(js.Array())

    val datasetsProcessor = Observer[Option[js.Array[Task_]]] {
      case None =>
        datasetVar.update(_ => js.Array())
      case Some(datasets) =>
        Promise.all[(String, String, String, GeoJSON__[Geometry,GeoJsonProperties], GeoJSON__[Geometry,GeoJsonProperties])](
          datasets.toArray
            .groupBy(_.dataset)
            .map { case (d, arr) =>
              Promise.all[(js.Array[Feature[Geometry, GeoJsonProperties]], js.Array[Feature[Geometry, GeoJsonProperties]])](
                arr.map(taskFeatures(model.taskState.now().date1, model.taskState.now().date2)).toJSArray
              ).`then` { a =>
                val b = a.unzip
                (
                  d.split("/").head,
                  arr.head.wmts(0),
                  arr.head.wmts(1),
                  L.GeoJSON__[Geometry, GeoJsonProperties](FeatureCollection(b._1.flatten.toJSArray)),
                  L.GeoJSON__[Geometry, GeoJsonProperties](FeatureCollection(b._2.flatten.toJSArray))
                )
              }
            }.toJSArray
        ).`then` { results =>
          datasetVar.update(_ => results)
        }
    }
    model.datasetsVar.signal --> datasetsProcessor

    def updateGlobalMap(dataset: (String,String,String,GeoJSON__[Geometry,GeoJsonProperties],GeoJSON__[Geometry,GeoJsonProperties])): Unit = {
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
      def sampleMarkers(fc:FeatureCollection[Geometry, GeoJsonProperties]): js.Array[Layer] = fc.features.groupBy(_.properties("sample")).map((sample, features) =>
        val coords = features.flatMap(_.geometry.asInstanceOf[MultiPolygon].coordinates(0)(0)).map(p => (p(1), p(0)))
        val (px, py) = ((coords.map(_._1).max + coords.map(_._1).min) / 2, (coords.map(_._2).max + coords.map(_._2).min) / 2)
        val annotators = features.map(_.properties("annotators").asInstanceOf[js.Array[String]])
        val (noAnnotation, oneAnnotation, twoAnnotations, moreAnnotations) = (annotators.count(_.isEmpty), annotators.count(_.length == 1), annotators.count(_.length == 2), annotators.count(_.length > 2))
        Marker_[GeoJsonProperties](js.Tuple3(px, py, js.undefined).asInstanceOf[LatLngTuple],
          MarkerOptions().setIcon(makeIcon(features.length.toString, Array(
            (noAnnotation.toDouble, "rgb(237,248,233)"),
            (oneAnnotation.toDouble, "rgb(186,228,179)"),
            (twoAnnotations.toDouble, "rgb(116,196,118)"),
            (moreAnnotations.toDouble, "rgb(35,139,69)")
          ))))
          .bindPopup(layer=>div(
            h4(sample.asInstanceOf[String].split("/").take(2).mkString("-")),
            p(b("no annotation: "),noAnnotation),p(b("one annotation: "),oneAnnotation),
            p(b("two annotations: "),twoAnnotations),p(b("more annotations: "),moreAnnotations)
          ).ref).asInstanceOf[Layer]
      ).toJSArray
      def taskMarkers(fc:FeatureCollection[Geometry, GeoJsonProperties]): js.Array[Layer] = fc.features.map(feature=>
        val annotators = feature.properties("annotators").asInstanceOf[js.Array[String]]
        val point = centroid[GeoJsonProperties](feature).geometry
        Marker_[GeoJsonProperties](js.Tuple3(point.coordinates(1), point.coordinates(0), js.undefined).asInstanceOf[LatLngTuple],
          MarkerOptions().setIcon(makeIcon(annotators.length.toString, Array(
            (annotators.length.toDouble+10, "rgb(237,248,233)")
          )))
        ).bindPopup(layer=>div(
          h4("Annotations: " + annotators.length)
        ).ref)
      ).toJSArray
      val leftFC:FeatureCollection[Geometry, GeoJsonProperties] = dataset._4.toGeoJSON().asInstanceOf[FeatureCollection[Geometry, GeoJsonProperties]]
      val leftSampleMarkers: js.Array[Layer] = sampleMarkers(leftFC)
      val leftTaskMarkers: js.Array[Layer] = taskMarkers(leftFC)
      val rightFC:FeatureCollection[Geometry, GeoJsonProperties] = dataset._5.toGeoJSON().asInstanceOf[FeatureCollection[Geometry, GeoJsonProperties]]
      val rightSampleMarkers: js.Array[Layer] = sampleMarkers(rightFC)
      val rightTaskMarkers: js.Array[Layer] = taskMarkers(rightFC)
      model.globalMapVar.now().foreach((left,right) =>
        model.globalMapLeftControl.foreach(left.removeControl)
        model.globalMapRightControl.foreach(right.removeControl)
        Utils.updateMaps(left, right, lGeoJSON, rGeoJSON, Some(dataset._2), Some(dataset._3))
        def updateGroups(map: Map_, sampleMarkers: js.Array[Layer], taskMarkers: js.Array[Layer]):Control_.Layers =
          val sampleGroup = layerGroup(sampleMarkers).addTo(map)
          val taskGroup = layerGroup(taskMarkers)
          val controls = control.layers().addTo(map).addOverlay(sampleGroup, "Annotation progress at the sample level").addOverlay(taskGroup, "Annotations at the task level")
          map.on("zoomend", (e: LeafletEvent) => {
            if map.getZoom() < 16 then
              map.addLayer(sampleGroup)
              map.removeLayer(taskGroup)
            else
              map.removeLayer(sampleGroup)
              map.addLayer(taskGroup)
          }:Unit)
          controls
        val leftControls = updateGroups(left,leftSampleMarkers,leftTaskMarkers)
        model.globalMapLeftControl = Some(leftControls)
        val rightControls = updateGroups(right,rightSampleMarkers,rightTaskMarkers)
        model.globalMapRightControl = Some(rightControls)
      )
    }

    val mapUpdater = Observer[(js.Array[(String, String, String, GeoJSON__[Geometry, GeoJsonProperties], GeoJSON__[Geometry, GeoJsonProperties])], String)] {
      case (datasets, selectedName) =>
        if (datasets.nonEmpty) {
          val selectedDataset = if (selectedName.isEmpty) datasets.headOption else datasets.find(_._1 == selectedName)
          selectedDataset.foreach(updateGlobalMap)
        }
    }

    datasetVar.signal.combineWith(model.datasetSelected.signal) --> mapUpdater

    div(
      h1(Page.AnnotatedMaps.name),
      label("Dataset: ",forId("datasetSelect")),
      select(idAttr("datasetSelect"),
        children <-- datasetVar.signal.map(l=>l.map(p=>option(p._1).amend(selected(p._1 == model.datasetSelected.now())))),
        onChange.mapToValue --> model.datasetSelected,
        datasetVar.signal.combineWith(model.datasetSelected.signal) --> mapUpdater,
      ),
      div(
        datasetVar.signal.combineWith(model.datasetSelected.signal) --> mapUpdater,
        // Wait for the component to be mounted before adding the leaflet and syncs
        onMountCallback(ctx =>
          Utils.syncMaps("globalMapLeft", "globalMapRight", model.globalMapVar)
          /*
                    val (l,r) = (map("globalMapLeft",true),map("globalMapRight",false))
                    l.sync(r, SyncMapOptions())
                    r.sync(l, SyncMapOptions())
                    globalMapLeftVar.update(_ => Some(l))
                    globalMapRightVar.update(_ => Some(r))
          */
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
