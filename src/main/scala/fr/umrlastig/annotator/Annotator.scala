package fr.umrlastig.annotator

import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import fr.umrlastig.annotator.GitOps.{cloneData, config, gitPull, gitPush, read, write}
import fr.umrlastig.annotator.Utils.getColor
import fr.umrlastig.annotator.Config.{leftLeftStyle, rightRightStyle}
import org.scalajs.dom
import org.scalajs.dom.window.alert
import org.scalajs.dom.{HTMLDivElement, HTMLElement, console}
import typings.geojson.mod.*
import typings.gitEssentials.distTypesApiPushMod.PushResult
import typings.leaflet.mod as L
import typings.leaflet.mod.*
import typings.leaflet.mod.PathOptions.MutableBuilder
import typings.leafletSync.*
import typings.leafletSync.leafletMod.{Map as SMap, *}
import typings.turfCentroid.mod.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.{implicitConversions, postfixOps}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
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
  import model.*
  private def updateMaps(leftMap: Map_, rightMap: Map_,
                         leftGeoJSON: GeoJSON__[Geometry,GeoJsonProperties], rightGeoJSON: GeoJSON__[Geometry,GeoJsonProperties],
                         leftWms: Option[String] = None, rightWms: Option[String] = None) =
    leftWms.foreach(addTileLayer(leftMap))
    rightWms.foreach(addTileLayer(rightMap))
    leftGeoJSON.addTo(leftMap)
    rightGeoJSON.addTo(rightMap)
    leftMap.fitBounds(
      if rightGeoJSON.getBounds().isValid()
      then rightGeoJSON.getBounds()
      else leftGeoJSON.getBounds())

  private def asFeatureCollection(text: String): FeatureCollection[Geometry, GeoJsonProperties] =
    JSON.parse(text).asInstanceOf[FeatureCollection[Geometry, GeoJsonProperties]]

  private def header(): Element =
    div(
      child.maybe <-- errorMessage.signal.map {
        case Some(msg) =>
          Some(
            div(
              cls("error-banner"),
              //style("background-color: #ffebee; color: #c62828; padding: 10px; margin-bottom: 10px; border-radius: 4px; text-align: center;"),
              msg,
              button(onClick --> { _ => errorMessage.update(_ => None) }, "×")
            )
          )
        case None => None
      },
      navTag(
        menuTag(
          li(
            button(onClick --> { _ => currentPage.update(_ => Page.Home) }, Page.Home.name)
          ),
          li(
            button(
              onClick --> { _ => currentPage.update(_ => Page.Dashboard) },
              disabled <-- datasetsVar.signal.map(_.isEmpty),
              Page.Dashboard.name
            )
          ),
          li(
            button(
              onClick --> { _ => currentPage.update(_ => Page.Leaderboard) },
              disabled <-- datasetsVar.signal.map(_.isEmpty),
              Page.Leaderboard.name
            )
          ),
          li(
            button(
              onClick --> { _ => currentPage.update(_ => Page.AnnotatedMaps) },
              disabled <-- datasetsVar.signal.map(_.isEmpty),
              Page.AnnotatedMaps.name
            )
          ),
          li(
            button(
              onClick --> { _ => currentPage.update(_ => Page.Annotate) },
              disabled <-- datasetsVar.signal.map(_.isEmpty).combineWithFn(annotationFinished.signal)((a, b)=>a||b),
              Page.Annotate.name
            )
          ),
          li(
            button(
              onClick --> { _ => logInOut() },
              text <-- stateVar.signal.map(l=>if l.validated then "Logout" else Page.Login.name)
            )
          ),
          li(
            button(
              onClick --> { _ => currentPage.update(_ => Page.Help) },
              Page.Help.name
            )
          )
        ),
        div(img(width("60px"),height("60px"),src("Loading_2.gif")),
          display <-- stateVar.signal.map(_.validated).combineWithFn(datasetsVar.signal.map(_.isDefined))((a:Boolean,b:Boolean)=>if !a||b then "none" else "initial"))
      )
    )
  end header

  def appElement(): Element =
    div(
      header(),
      child <-- currentPage.signal.splitOne(x => x) { (id, initial, signal) =>
        console.info(s"Split ${id.name}")
        id match {
          case Page.Home          => renderHome()
          case Page.Dashboard     => renderDashboard()
          case Page.Leaderboard   => renderLeaderboard()
          case Page.AnnotatedMaps => renderGlobalDashboard()
          case Page.Annotate      => renderAnnotate()
          case Page.Login         => renderLogin()
          case Page.Help          => renderHelp()
        }
      }
    )
  end appElement

  private def renderHome(): Element =
    div(
      h1(Page.Home.name),
      // TODO load from generic description file (md, html etc.)
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

  private def renderDashboard(): Element =
    case class SampleR(name: String, total: Int, progress:Int)
    case class DatasetR(name:String, samples: List[SampleR])
    def newRenderDataset(dataset: String, init: DatasetR, signal: Signal[DatasetR]): HtmlElement = {
      div(
        label(h3(dataset),forId(dataset)),
        progressTag(
          idAttr(dataset),
          maxAttr <-- signal.map(_.samples.map(_.total).sum.toString),
          value <-- signal.map(_.samples.map(_.progress).sum.toString)
        ),
        children <-- signal.map(_.samples).split(_.name)(newRenderSample)
      )
    }
    def newRenderSample(sample: String, initial: SampleR, sampleSignal: Signal[SampleR]): HtmlElement = {
      div(
        label(sample, forId(sample)),
        progressTag(
          idAttr(sample),
          maxAttr <-- sampleSignal.map(_.total.toString),
          value <-- sampleSignal.map(_.progress.toString)
        )
      )
    }
    def username = stateVar.now().username
    val signal = datasetsVar.signal.map(datasetsOption=>datasetsOption.map(datasets=>datasets.toArray
      .groupBy(_.dataset)
      .map((d,array)=>
        DatasetR(d.split("/").head,array.groupBy(_.sample).map((s,array)=>
          SampleR(s.split("/").take(2).mkString("-"),array.length,array.count(_.task.annotations.exists(_.username == username)))
        ).toList.sortBy(_.name))
      ).toList
    ).get)
    div(
      h1(Page.Dashboard.name),
      h2(s"User: $username"),
      children <-- signal.split(_.name)(newRenderDataset)

    )
  end renderDashboard

  private def makeIcon(text: String, values: Array[(Double,String)]) =
    val sum = values.map(_._1).sum
    divIcon(DivIconOptions().setHtml(div(
      b(text),
      background("conic-gradient(" + values.foldLeft((List[String](), 0.0))((acc, tuple) => (acc._1 ++ List(s"${tuple._2} ${acc._2}deg", s"${tuple._2} ${acc._2 + tuple._1 * 360 / sum}deg"), acc._2 + tuple._1 * 360 / sum))._1.mkString(",") + ")"),
      width(s"${sum*2}px"),
      height(s"${sum*2}px"),
      borderRadius(s"${sum}px"),
      alignContent("center")
    ).ref).setIconSize(Point_(sum*2,sum*2)))

  private def renderGlobalDashboard(): Element =
    def taskFeatures(dateLeft: String, dateRight: String)(task: Task_): Promise[(js.Array[Feature[Geometry,GeoJsonProperties]],js.Array[Feature[Geometry,GeoJsonProperties]])] =
      console.debug("taskFile: "+task.task.task)
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
    def username = stateVar.now().username
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
                arr.map(taskFeatures(taskState.now().date1, taskState.now().date2)).toJSArray
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
    datasetsVar.signal --> datasetsProcessor

    def updateGlobalMap(dataset: (String,String,String,GeoJSON__[Geometry,GeoJsonProperties],GeoJSON__[Geometry,GeoJsonProperties])): Unit = {
      console.debug(s"update global Maps for dataset ${dataset._1}")
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
      globalMapLeftVar.now().zip(globalMapRightVar.now()).foreach((left,right) =>
        globalMapLeftControl.foreach(left.removeControl)
        globalMapRightControl.foreach(right.removeControl)
        updateMaps(left, right, lGeoJSON, rGeoJSON, Some(dataset._2), Some(dataset._3))
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
        globalMapLeftControl = Some(leftControls)
        val rightControls = updateGroups(right,rightSampleMarkers,rightTaskMarkers)
        globalMapRightControl = Some(rightControls)
      )
    }

    val mapUpdater = Observer[(js.Array[(String, String, String, GeoJSON__[Geometry, GeoJsonProperties], GeoJSON__[Geometry, GeoJsonProperties])], String)] {
      case (datasets, selectedName) =>
        if (datasets.nonEmpty) {
          val selectedDataset = if (selectedName.isEmpty) datasets.headOption else datasets.find(_._1 == selectedName)
          selectedDataset.foreach(updateGlobalMap)
        }
    }

    datasetVar.signal.combineWith(datasetSelected.signal) --> mapUpdater

    div(
      h1(Page.AnnotatedMaps.name),
      label("Dataset: ",forId("datasetSelect")),
      select(idAttr("datasetSelect"),
        children <-- datasetVar.signal.map(l=>l.map(p=>option(p._1).amend(selected(p._1 == datasetSelected.now())))),
        onChange.mapToValue --> datasetSelected,
        datasetVar.signal.combineWith(datasetSelected.signal) --> mapUpdater,
      ),
      div(
        datasetVar.signal.combineWith(datasetSelected.signal) --> mapUpdater,
        // Wait for the component to be mounted before adding the leaflet and syncs
        onMountCallback(ctx =>
          val (l,r) = (map("globalMapLeft",true),map("globalMapRight",false))
          l.sync(r, SyncMapOptions())
          r.sync(l, SyncMapOptions())
          globalMapLeftVar.update(_ => Some(l))
          globalMapRightVar.update(_ => Some(r))
        ),
        div(idAttr("global-container"), cls("my-container"),
          div(idAttr("globalMapLeft"), cls("mapLeft"), cls("map"),
          ),
          div(idAttr("globalMapRight"), cls("mapRight"), cls("map"),
          )
        ),
      ),
      datasetsVar.signal --> datasetsProcessor,
    )
  end renderGlobalDashboard

  private def renderLeaderboard(): Element =
    val currentUser = stateVar.now().username
    val leaderboardProcessor = Observer[Option[js.Array[Task_]]] {
      case None =>
        console.info("leaderboardProcessor with no task")
        leaderboardData.update(_ => List.empty)
      case Some(datasets) =>
        console.info(s"leaderboardProcessor with ${datasets.size} datasets")
        // Get all annotations from all tasks
        val allAnnotations = datasets.flatMap(_.task.annotations)
        // Count by username
        val counts = mutable.Map[String, Int]()
        allAnnotations.foreach { ann =>
          val user = ann.username
          counts(user) = counts.getOrElse(user, 0) + 1
        }
        // Calculate total for percentage
        val total = counts.values.sum
        val totalDouble = if (total == 0) 1.0 else total.toDouble
        // Convert to list and sort descending
        val ranked = counts.toList
          .map { case (user, count) => UserRank(user, count, (count / totalDouble) * 100) }
          .sortBy(-_.count) // Sort descending by count
        leaderboardData.update(_ => ranked)
    }

    div(
      h1(Page.Leaderboard.name),

      // Empty state message
      child.maybe <-- leaderboardData.signal.map { ranks =>
        if (ranks.isEmpty) Some(div("No annotations yet.")) else None
      },

      // Leaderboard table (always renders, but empty if no data)
      div(
        div(cls("leaderboard-header"), span("Rank"), span("User"), span("Count"), span("%")),

        // This is the key fix: children takes a Signal[List[Element]]
        children <-- leaderboardData.signal.map { ranks =>
          ranks.zipWithIndex.map { case (r, i) =>
            val medal = i match {
              case 0 => "🥇"
              case 1 => "🥈"
              case 2 => "🥉"
              case _ => s"#${i + 1}"
            }
            val isMe = r.username == currentUser

            div(
              cls("leaderboard-row"),
              if (isMe) cls("highlight") else "",
              span(medal),
              span(r.username),
              span(r.count.toString),
              span(f"${r.percentage}%.1f%%")
            )
          }
        }
      ),
      datasetsVar.signal --> leaderboardProcessor
    )
  end renderLeaderboard

  // implicit conversion to leaflet.sync monkey patched version of Map
  implicit def map2sync(jq: Map_): SMap = jq.asInstanceOf[SMap]

  def map(name: String, left: Boolean): Map_ =
    console.info("create map")
    L.map(name, MapOptions().setAttributionControl(!left).setZoomControl(left)).setView(LatLngLiteral(48.8, 2.3), zoom = 18)

  private def addTileLayer(map: Map_)(wmts:String): Unit =
    val defaultWMTS = Map("REQUEST" -> "GetTile", "SERVICE" -> "WMTS", "VERSION" -> "1.0.0", "STYLE" -> "normal", "TILEMATRIXSET" -> "PM", "FORMAT" -> "image/jpeg", "TILEMATRIX" -> "{z}", "TILEROW" -> "{y}", "TILECOL" -> "{x}")

    def makeWMTS(url: String, map: Map[String, String]) = s"$url?${map.map((k, v) => k + "=" + v).mkString("&")}"

    val tmpLayers = mutable.ArrayBuffer.empty[Layer]
    map.eachLayer((layer:Layer) => tmpLayers+=layer)
    tmpLayers.foreach(map.removeLayer)
    val split = wmts.split('?')
    val baseUrl = split.head
    // can not always use WMTS default param : need to parse additional parameters provided in the wmts url
    // TODO for now use convention URL/wmts?layer1,layer2,...&PARAM1=...&... which is not the correct call with "LAYER="
    val layers = split(1).split('&')(0).split(',')
    val params = split(1).split('&').tail.map(s => {val kv = s.split('='); (kv(0),kv(1))}).toMap
    val wmtsParams: Map[String,String] = defaultWMTS.keys.map(k => if(params.contains(k)) (k,params(k)) else (k,defaultWMTS(k))).toMap
    console.debug(s"addTileLayer with url $baseUrl, layers = ${layers.mkString(",")} and parameters ${params.mkString(",")}")
    if baseUrl.contains("wmts") then
      for (layer <- layers) {
        tileLayer(makeWMTS(baseUrl,Map("LAYER"->layer)++wmtsParams),
          // TODO add attribution somewhere in params ? .setAttribution("IGN-F/Geoportail")
          TileLayerOptions().setMinZoom(0).setMaxZoom(20).setMaxNativeZoom(18).setAttribution("").setTileSize(256)
        ).addTo(map)
      }
    else
      tileLayer.wms(s"$baseUrl?",
        WMSOptions().setMinZoom(0).setMaxZoom(20).setMaxNativeZoom(18).setAttribution(baseUrl).setLayers(layers.mkString(","))
      ).addTo(map)

  // TODO add option to have switch between multiple WMS
  /*
  tileLayer(
    "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
    TileLayerOptions().setMaxZoom(18).setAttribution("""&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>""")
  ).addTo(m)
  */

  private def typeElement(name: String, index: Int, n: Int): Element =
  // TODO add special cases for label, boolean input and text input (quality issue, comment)
    button(name,typ("button"),
      backgroundColor <-- annotationState.signal.map{s => if (s.types.isEmpty) "" else s.types(index)}.map(t=>if t == name then "Silver" else "WhiteSmoke"),
      onClick --> { _ => annotationState.update(_.copy(
        types = {val prevTypes = annotationState.now().types
          if (prevTypes.isEmpty) Seq.fill(n)("").updated(index, name)
          else prevTypes.updated(index, name)},
        step=index))
        console.info(s"types: ${annotationState.now().types}")
      }
    )

  // TODO index is always < max size by use but not secure
  private def nextButton(index: Int): Element =
    button(">",backgroundColor := "Orchid",
      disabled <-- annotationState.signal.map(_.types(index)).map(_.isEmpty),
      onClick --> { _ => annotationState.update(_.copy(step=index+1)) }
    )

  // TODO index is always >= 1 by use of the function but this is not secure without a check
  private def backButton(index: Int): Element = button("<",backgroundColor := "Orchid",onClick --> { _ => annotationState.update(_.copy(step=index-1)) })

  private def saveButton: Element = button("save",
    backgroundColor := "Crimson",
    disabled <-- annotationState.signal.map(_.types.isEmpty),
    onClick --> saveAnnotation
  )

  private def renderAnnotate(): Element =
    // generic scheme : children( prev button if not first, [... typeButtons], next button if not last, save button if last) <-- annotationState.signal.map(_.step== INDEX )
    val modalities = taskState.now().modalities
    console.info(s"Modalities for task : $modalities")
    val toolbar = modalities.zipWithIndex.map{ case (s,i) =>
        val n = modalities.length
        val elements = if (n==1) {s.map(typeElement(_, i, n))++Seq(saveButton)} else {
          (if (i>0) Seq(backButton(i)) else Seq.empty)++s.map(typeElement(_, i, n))++(if (i<(n-1)) Seq(nextButton(i)) else Seq.empty)++(if (i==(n-1)) Seq(saveButton) else Seq.empty)
        }
        children(elements)  <-- annotationState.signal.map(_.step==i)
      }

    div(
      // Wait for the component to be mounted before adding the leaflet and syncs
      onMountCallback(ctx =>
        val (l, r) = (map("mapLeft", true), map("mapRight", false))
        l.sync(r, SyncMapOptions())
        r.sync(l, SyncMapOptions())
        mapLeftVar.update(_ => Some(l))
        mapRightVar.update(_ => Some(r))

        updateMaps(l,r,geoJSON.now().get._1,geoJSON.now().get._2,Some(taskState.now().wms1),Some(taskState.now().wms2))
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

  private def saveAnnotation(event: dom.MouseEvent): Unit = {
    val currentAnnotationState = annotationState.now()
    val currentTaskState = taskState.now()
    val currentUserState = stateVar.now()
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
              datasetsVar.update(datasets => datasets.map(_.map {
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

              nextFeature().`then`(updated => {
                if updated then
                  mapLeftVar.now().zip(mapRightVar.now()).foreach((l, r) => updateMaps(l, r, geoJSON.now().get._1, geoJSON.now().get._2))
                  currentPage.update(_ => Page.Annotate)
                else
                  annotationFinished.update(_ => true)
                  currentPage.update(_ => Page.Dashboard)
              })
            })
            .`catch`(err => {
              console.error("Save operation failed!", err)
              // Show a notification to the user
              errorMessage.update(_=>Some(s"Error: ${err.toString}"))
              alert("Failed to save annotation. Check console for details.")
            })
        })
        .`catch`(err => {
          console.error("Failed to read sample file", err)
          // Show a notification to the user
          errorMessage.update(_=>Some(s"Error: ${err.toString}"))
          alert("Could not load sample data.")
        })
      })
      .`catch`(err => {
        console.error("Pull or read failed:", err)
        errorMessage.update(_ => Some(s"Error: ${err.toString}"))
        alert("Failed to sync with server. Please refresh and try again.")
      })
  }

  private def renderInputRow(error: UserState => Option[String])(mods: Modifier[HtmlElement]*): HtmlElement = {
    val errorSignal = stateVar.signal.map(_.displayError(error))
    div(
      cls("-inputRow"),
      cls("x-hasError") <-- errorSignal.map(_.nonEmpty),
      mods,
      child.maybe <-- errorSignal.map(_.map(err => div(cls("-error"), err)))
    )
  }
  private def renderLogin(): Element =
    div(
      h1(Page.Login.name),
      form(
        onSubmit.preventDefault.mapTo(stateVar.now()) --> submitter,
        renderInputRow(_.usernameError)(
          label("Username: "),
          input(
            placeholder("YOUR-USERNAME"),
            controlled(
              value <-- stateVar.signal.map(_.username),
              onInput.mapToValue --> usernameWriter
            )
          ),
          button(
            typ("button"), // "submit" is the default in HTML
            "Clear",
            onClick.mapTo("") --> usernameWriter
          )
        ),
        renderInputRow(_.tokenError)(
          label("Token: "),
          input(
            typ("password"),
            placeholder("YOUR-PERSONAL-ACCESS-TOKEN"),
            controlled(
              value <-- stateVar.signal.map(_.token),
              onInput.mapToValue --> tokenWriter
            )
          ),
          button(
            typ("button"), // default button type in HTML is "submit", we don't want it
            "Clear",
            onClick.mapTo("") --> tokenWriter
          )
        ),
        button(typ("submit"), "Submit")
      ),
      p("To create one, refer to ",a("Token creation",href("https://github.com/settings/tokens/new")),". Your account has to be linked to the ",a("SUBDENSE organisation",href("https://github.com/subdense"))," and the token need the ",b("repo")," rights.")
    )
  end renderLogin

  private def renderHelp(): ReactiveHtmlElement[HTMLDivElement] =
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

end Main

enum Page(val name: String):
  case Home extends Page("Home")
  case Dashboard extends Page("Dashboard")
  case Leaderboard extends Page("Leaderboard")
  case AnnotatedMaps extends Page("Annotated Maps")
  case Annotate extends Page("Annotate")
  case Login extends Page("Login")
  case Help extends Page("Help")

final class Model {
  val currentPage: Var[Page] = Var(Page.Home)
  val geoJSON: Var[Option[(GeoJSON__[Geometry, GeoJsonProperties], GeoJSON__[Geometry, GeoJsonProperties])]] = Var(None)
  case class TaskState(date1: String="", date2: String="", wms1: String="", wms2: String="", sampleFile: String="", taskFile:String="", modalities: Seq[Seq[String]] = Seq.empty)
  val taskState: Var[TaskState] = Var(TaskState())
  val datasetsVar: Var[Option[js.Array[Task_]]] = Var(None)
  private var iterator: Option[Iterator[Task_]] = None
  val mapLeftVar: Var[Option[Map_]] = Var(None)
  val mapRightVar: Var[Option[Map_]] = Var(None)
  var globalMapLeftVar: Var[Option[Map_]] = Var(None)
  var globalMapRightVar: Var[Option[Map_]] = Var(None)
  // no need for Var in Controls
  var globalMapLeftControl: Option[Control_.Layers] = None
  var globalMapRightControl: Option[Control_.Layers] = None
  val annotationFinished: Var[Boolean] = Var(false)
  //case class AnnotationState(linkType: String = "", changeType: String = "", quality: Boolean = false, comment: String = "", step: Int = 0)
  case class AnnotationState(types: Seq[String] = Seq.empty[String], quality: Boolean = false, comment: String = "", step: Int = 0)
  val annotationState: Var[AnnotationState] = Var(AnnotationState())
  val datasetSelected: Var[String] = Var("")
  val errorMessage: Var[Option[String]] = Var(None)

  case class UserRank(username: String, count: Int, percentage: Double)
  val leaderboardData: Var[List[UserRank]] = Var(List.empty)

  case class UserState(username: String = "",token: String = "",showErrors: Boolean = false,validated: Boolean = false) {
    def hasErrors: Boolean = usernameError.nonEmpty || tokenError.nonEmpty
    def usernameError: Option[String] = if (username.nonEmpty) None else Some("Username must not be empty.")
    def tokenError: Option[String] = if (token.length == 40) None else Some("Token must consist of 40 character long.")
    def displayError(error: UserState => Option[String]): Option[String] = error(this).filter(_ => showErrors)
  }
  private def localStorageGetOrElse(v: String): String = if dom.window.localStorage.hasOwnProperty(v) then dom.window.localStorage.getItem(v) else ""

  val stateVar: Var[UserState] = {
    val (username, token) = (localStorageGetOrElse("username"), localStorageGetOrElse("token"))
    Var(UserState(username,token))
  }

  private def geoJSONUpdate(left:GeoJSON__[Geometry,GeoJsonProperties], right:GeoJSON__[Geometry,GeoJsonProperties]): Unit =
    geoJSON.update(previous =>
      previous match {
        case Some((previousLeft, previousRight)) =>
          mapLeftVar.now().zip(mapRightVar.now()).foreach((l,r)=>
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
      cloneData(state.token, rng)
        .`then`(datasets =>
          datasetsVar.update(_ => Some(datasets))
          iterator = Some(datasets.iterator)
          stateVar.update(_.copy(validated = true))
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
