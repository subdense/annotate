package subdense

import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom
import org.scalajs.dom.{HTMLDivElement, HTMLElement}
import typings.geojson.mod.*
import typings.gitEssentials.clientsFsIndexedDbFsClientMod.IndexedDbFsClient
import typings.gitEssentials.clientsHttpWebHttpClientMod.makeWebHttpClient
import typings.gitEssentials.distTypesApiAddMod.AddParams
import typings.gitEssentials.distTypesApiCommitMod.CommitParams
import typings.gitEssentials.distTypesApiPushMod.{PushParams, PushResult}
import typings.gitEssentials.distTypesModelsAuthMod.Auth
import typings.gitEssentials.distTypesModelsAuthorMod.Author
import typings.gitEssentials.distTypesModelsFsClientMod.{EncodingOptions, RmOptions}
import typings.gitEssentials.gitEssentialsStrings.utf8
import typings.gitEssentials.{distTypesApiCloneMod as clone, distTypesClientsHttpWebHttpClientMod as whco, mod as essentials}
import typings.leaflet.mod as L
import typings.leaflet.mod.*
import typings.leaflet.mod.PathOptions.MutableBuilder
import typings.leafletSync.*
import typings.leafletSync.leafletMod.{Map as SMap, *}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.implicitConversions
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.URIUtils.encodeURIComponent
import scala.scalajs.js.{JSON, Promise}

val leftRightStyle = PathOptions().setColor("#ff7800").setWeight(5).setOpacity(0.25).setDashArray("20 20").setDashOffset("10").setFill(false)
val leftLeftStyle = PathOptions().setColor("#78ff00").setWeight(2).setOpacity(0.8).setFill(false)
val rightRightStyle = PathOptions().setColor("#ff7800").setWeight(2).setOpacity(0.8).setFill(false)
val rightLeftStyle = PathOptions().setColor("#78ff00").setWeight(5).setOpacity(0.25).setDashArray("20 20").setDashOffset("10").setFill(false)

val rnd = new scala.util.Random(42)
def toHexString(color:(Double,Double,Double)): String = {
  val (r,g,b) = ((color._1*255).floor.intValue,(color._2*255).floor.intValue,(color._3*255).floor.intValue)
  f"#$r%02x$g%02x$b%02x"
}
// input: h in [0,360] and s,v in [0,1] - output: r,g,b in [0,1]
def hsl2rgb (h: Double, s: Double, l:Double) =
  val a = s * Math.min(l, 1 - l)
  def f(n:Int) =
    val k = (n + h / 30) % 12
    l - a * Math.max(Seq(k - 3, 9 - k, 1).min, -1)
  (f(0),f(8),f(4))

def getColor = toHexString(hsl2rgb(rnd.nextDouble()*360,0.8,0.5))

class DatasetList extends js.Object {
  var datasets: js.Array[String] = _
}

class Dataset extends js.Object {
  var dates: js.Array[String] = _
  var wmts: js.Array[String] = _
  var samples: js.Array[String] = _
}
class Annotation extends js.Object {
  var username: String = _
  var link: String = _
  var quality: Boolean = _
  var comment: String = _
  var change: String = _
}

class Task extends js.Object {
  var task: String = _
  var annotations: js.Array[Annotation] = _
}
class Sample extends js.Object {
  var tasks: js.Array[Task] = _
}

class Dataset_ extends js.Object {
  var dates: js.Array[String] = _
  var wmts: js.Array[String] = _
  var tasks: js.Array[String] = _
}

//case class Task(dataset:String,dates: (String,String), wmts: (String, String), task: String)

class Task_ extends js.Object {
  var dataset: String = _
  var dates: js.Array[String] = _
  var wmts: js.Array[String] = _
  var sample: String = _
  var task: Task = _
}

//case class Dataset_ (dates: (String,String), wmts: (String, String), samples: Array[String]);

val client = IndexedDbFsClient("my-repos")
val dir = "/datasets"

def read[T](file: String, parse: Boolean = true): Promise[T] = client.readFile(file, EncodingOptions().setEncoding(utf8))
  .`then`(content => (if parse then JSON.parse(content.asInstanceOf[String]) else content).asInstanceOf[T])

def write(file: String, content: String): Promise[Unit] = client.writeFile(file, content, EncodingOptions().setEncoding(utf8))

def cloneData(token: String): Promise[js.Array[Task_]] =
  val useIsomorphicProxy = true
  val proxy = if useIsomorphicProxy then "https://cors.isomorphic-git.org" else "https://gitcorsproxy.vercel.app/api/cors"
  val url = "https://github.com/subdense/private_datasets.git"
  val http_ = """^https?:\/\/"""
  def transform(url:String,b:js.UndefOr[Boolean]) = if useIsomorphicProxy then s"$proxy/${url.replaceAll(http_, "")}" else s"$proxy?url=${encodeURIComponent(url)}"
  println(s"start cleanup")
  client.rm(dir, RmOptions().setRecursive(true).setForce(true))
    .`then`[Unit](_=>println(s"done with cleanup"))
    .`then`[Unit](_=>
      essentials.clone_(clone.CloneParams(
        dir = dir,
        fs = client,
        http = makeWebHttpClient(whco.WebHttpClientOptions().setTransformRequestUrl(transform)),
        url = url)
      .setOnAuth((url, auth) =>
        println(s"Authentication for $url")
        auth.setUsername(token))
      ))
    .`then`[DatasetList](_=>
      println(s"done cloning $url")
      read[DatasetList](s"$dir/datasets.json"))
    .`then`(content=>
      println(s"done reading $dir/datasets.json")
      val promises = content.asInstanceOf[DatasetList].datasets.map(datasetName => read[Dataset](s"$dir/$datasetName").`then`(d => (datasetName, d)))
      Promise.all(promises))
    .`then`(datasets=>
        Promise.all(datasets.asInstanceOf[js.Array[(String, Dataset)]].flatMap((datasetName, dataset) =>
          dataset.samples.map(sample => read[Sample](s"$dir/$sample").`then`(s => js.Dynamic.literal(name = datasetName, dates = dataset.dates, wmts = dataset.wmts, sampleFile = sample, sample = s))))))
    .`then`(samples =>
        println(s"all sample promises")
        val tasks = samples.asInstanceOf[js.Array[js.Dynamic]].flatMap(s =>
          s.sample.asInstanceOf[Sample].tasks.map(t =>
            val task = Task_()
            task.dataset = s.name.asInstanceOf[String]
            task.dates = s.dates.asInstanceOf[js.Array[String]]
            task.wmts = s.wmts.asInstanceOf[js.Array[String]]
            task.sample = s.sampleFile.asInstanceOf[String]
            task.task = t
            task
          )
        )
        println(s"${tasks.length} tasks")
        tasks
      )

def gitPush(username: String, token: String, file: String, message: String): Promise[PushResult] =
  val useIsomorphicProxy = true
  val proxy = if useIsomorphicProxy then "https://cors.isomorphic-git.org" else "https://gitcorsproxy.vercel.app/api/cors"
  val url = "https://github.com/subdense/private_datasets.git"
  val http_ = """^https?:\/\/"""
  def transform(url: String, b: js.UndefOr[Boolean]) = if useIsomorphicProxy then s"$proxy/${url.replaceAll(http_, "")}" else s"$proxy?url=${encodeURIComponent(url)}"
  println(s"start add $file with ${transform(url, js.undefined)}")
  essentials.add(AddParams(
    dir=dir,
    filepath = file,
    fs = client
  )).`then`(_=>
    println(s"start commit $message with ${transform(url, js.undefined)}")
    essentials.commit(CommitParams(dir = dir, fs = client, message = message).setAuthor(Author(username)))
      .`then`(c=>
        println(s"start push")
        essentials.push(
          PushParams(dir = dir, fs = client, http = makeWebHttpClient(whco.WebHttpClientOptions().setTransformRequestUrl(transform)))
            .setUrl(url)
            .setOnAuth((_,_) => Auth().setUsername(token))
        )
      )
  )

@main
def Annotator(): Unit =
  renderOnDomContentLoaded(
    dom.document.getElementById("app"),
    Main.appElement()
  )
end Annotator

object Main:
  val model = new Model
  import model.*

  //val currentPage: Var[Page] = Var(Page.Home)
  //private val geoJSON: Var[Option[(GeoJSON__[Geometry,GeoJsonProperties],GeoJSON__[Geometry,GeoJsonProperties])]] = Var(None)

  private def updateMaps(leftMap: Map_, rightMap: Map_, leftGeoJSON: GeoJSON__[Geometry,GeoJsonProperties], rightGeoJSON: GeoJSON__[Geometry,GeoJsonProperties]) =
    leftGeoJSON.addTo(leftMap)
    rightGeoJSON.addTo(rightMap)
    leftMap.setView(
      if rightGeoJSON.getBounds().isValid()
      then rightGeoJSON.getBounds().getCenter()
      else leftGeoJSON.getBounds().getCenter())

  private def asFeatureCollection(text: String): FeatureCollection[Geometry, GeoJsonProperties] =
    JSON.parse(text).asInstanceOf[FeatureCollection[Geometry, GeoJsonProperties]]

  def header(): Element =
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
      )
    )
  def appElement(): Element =
    div(
      header(),
      child <-- currentPage.signal.splitOne(x => x) { (id, initial, signal) =>
        println(s"Split ${id.name}")
        id match {
          case Page.Home          => renderHome()
          case Page.Dashboard     => renderDashboard()
          case Page.AnnotatedMaps => renderGlobalDashboard()
          case Page.Annotate      => renderAnnotate()
          case Page.Login         => renderLogin()
          case Page.Help          => renderHelp()
        }
      }
    )
  end appElement

  def renderHome(): Element =
    div(
      h1(Page.Home.name),
      p("This is an annotation app for the SUBDENSE project")
    )
  end renderHome

  def renderDashboard(): Element =
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
    divIcon(DivIconOptions().setHtml(b(
    text,
    background("conic-gradient("+values.foldLeft((List[String](),0.0))((acc,tuple)=>(acc._1++List(s"${tuple._2} ${acc._2}deg",s"${tuple._2} ${acc._2 + tuple._1*360/sum}deg"),acc._2 + tuple._1*360/sum))._1.mkString(",")+")")
  ).ref))

  def renderGlobalDashboard(): Element =
    def taskFeatures(dateLeft: String, dateRight: String)(task: Task_): Promise[(js.Array[Feature[Geometry,GeoJsonProperties]],js.Array[Feature[Geometry,GeoJsonProperties]])] =
      println("taskFile: "+task.task.task)
      read[String](s"$dir/${task.task.task}", false).`then`(content =>
        val newTask = content.asInstanceOf[String]
        val features = asFeatureCollection(newTask).features
        features.foreach(f=>
          f.properties("task")=task.task.task
          f.properties("sample")=task.sample
          f.properties("annotators")=task.task.annotations.map(_.username)
          f.properties("linkTypes")=task.task.annotations.map(_.link)
          f.properties("changeTypes")=task.task.annotations.map(_.change)
          f.properties("quality")=task.task.annotations.map(_.quality)
          f.properties("comments")=task.task.annotations.map(_.comment)
        )
        val groupedFeatures = features.groupBy(_.properties("date"))
        (groupedFeatures.getOrElse(dateLeft,js.Array()),groupedFeatures.getOrElse(dateRight,js.Array()))
      )
    def username = stateVar.now().username
    val datasetVar: Var[js.Array[(String,GeoJSON__[Geometry,GeoJsonProperties],GeoJSON__[Geometry,GeoJsonProperties])]] = Var(js.Array())
    datasetsVar.now().map(datasets=>
      Promise.all[(String,GeoJSON__[Geometry,GeoJsonProperties],GeoJSON__[Geometry,GeoJsonProperties])](
        datasets.toArray
          .groupBy(_.dataset)
          .map((d,array)=>
            Promise.all[(js.Array[Feature[Geometry,GeoJsonProperties]],js.Array[Feature[Geometry,GeoJsonProperties]])](
              array.map(taskFeatures(taskState.now().date1,taskState.now().date2)).toJSArray
            ).`then`(a=>
              val b = a.asInstanceOf[js.Array[(js.Array[Feature[Geometry,GeoJsonProperties]],js.Array[Feature[Geometry,GeoJsonProperties]])]].unzip
              println(s"Left: ${b._1.flatten.toJSArray.size} - Right: ${b._2.flatten.toJSArray.size}")
              (d.split("/").head,L.GeoJSON__[Geometry,GeoJsonProperties](FeatureCollection(b._1.flatten.toJSArray)),L.GeoJSON__[Geometry,GeoJsonProperties](FeatureCollection(b._2.flatten.toJSArray)))
            )
          ).toJSIterable
      ).`then`(result=>datasetVar.update(_=>result))
    )
    val dsObserver = Observer[js.Array[(String,GeoJSON__[Geometry,GeoJsonProperties],GeoJSON__[Geometry,GeoJsonProperties])]](
      onNext = nextValue =>
        println("update global Maps")
        if nextValue.nonEmpty then
          val colorMap = mutable.Map[String,String]()
          val lGeoJSON = nextValue.head._2.setStyle(f =>
            val feature = f.asInstanceOf[Feature[Geometry,GeoJsonProperties]]
            val c = colorMap.getOrElseUpdate(feature.properties("task").asInstanceOf[String],getColor)
            PathOptions().setColor(c)
          ).bindPopup(layer=>
            val feature = layer.feature.asInstanceOf[Feature[Geometry,GeoJsonProperties]]
            div(
              h4(feature.properties("task").asInstanceOf[String]),
              p(b("annotators: "),feature.properties("annotators").asInstanceOf[js.Array[String]].mkString(","))
            ).ref
          )
          val rGeoJSON = nextValue.head._3.setStyle(f =>
            val feature = f.asInstanceOf[Feature[Geometry,GeoJsonProperties]]
            val c = colorMap.getOrElseUpdate(feature.properties("task").asInstanceOf[String],getColor)
            PathOptions().setColor(c)
          ).bindPopup(layer=>
            val feature = layer.feature.asInstanceOf[Feature[Geometry,GeoJsonProperties]]
            div(
              h4(feature.properties("task").asInstanceOf[String]),
              p(b("annotators: "),feature.properties("annotators").asInstanceOf[js.Array[String]].mkString(","))
            ).ref
          )
          val lMarkers = nextValue.head._2.toGeoJSON().asInstanceOf[FeatureCollection[Geometry,GeoJsonProperties]].features.groupBy(_.properties("sample")).map((sample,features)=>
            println(s"sample = $sample")
            //println(s"features = ${features.length}")
            val coords = features.flatMap(_.geometry.asInstanceOf[MultiPolygon].coordinates(0)(0)).map(p=>(p(1),p(0)))
            //println(s"coords = ${coords.length}")
            //coords.foreach((a,b)=>println(s"p = $a, $b"))
            val (px,py) = ((coords.map(_._1).max + coords.map(_._1).min)/2,(coords.map(_._2).max + coords.map(_._2).min)/2)
            val annotators = features.map(_.properties("annotators").asInstanceOf[js.Array[String]])
            val (noAnnotation,oneAnnotation,twoAnnotations,moreAnnotations) = (annotators.count(_.isEmpty),annotators.count(_.length == 1),annotators.count(_.length == 2),annotators.count(_.length > 2))
            //println(s"bounds = $px - $py")
            Marker_[GeoJsonProperties](js.Tuple3(px,py,js.undefined).asInstanceOf[LatLngTuple],
              MarkerOptions().setIcon(makeIcon(features.length.toString, Array(
                (noAnnotation.toDouble,"rgb(237,248,233)"),
                (oneAnnotation.toDouble,"rgb(186,228,179)"),
                (twoAnnotations.toDouble,"rgb(116,196,118)"),
                (moreAnnotations.toDouble,"rgb(35,139,69)")
              ))))
              .bindPopup(sample.asInstanceOf[String].split("/").take(2).mkString("-"))
              .asInstanceOf[Layer]
          ).toJSArray
          if (globalMapLeft.isDefined && globalMapRight.isDefined)
            updateMaps(globalMapLeft.get, globalMapRight.get, lGeoJSON, rGeoJSON)
            val group = layerGroup(lMarkers).addTo(globalMapLeft.get)
            control.layers().addTo(globalMapLeft.get).addOverlay(group,"Annotations")
            globalMapLeft.get.on("zoomend",(e:LeafletEvent) =>
              if globalMapLeft.get.getZoom() < 16 then
                globalMapLeft.get.addLayer(group)
              else
                globalMapLeft.get.removeLayer(group)
              println("Event " + globalMapLeft.get.getZoom())
            )
    )
    div(
      h1(Page.AnnotatedMaps.name),
      label("Dataset: ",forId("datasetSelect")),
      select(idAttr("datasetSelect"),
        value <-- datasetSelected.signal,
        children <-- datasetVar.signal.map(l=>l.map(p=>option(p._1))),
        onChange.mapToValue --> datasetSelected
      ),
      div(
        datasetVar.signal --> dsObserver,
        // Wait for the component to be mounted before adding the leaflet and syncs
        onMountCallback(ctx =>
          val (l,r) = (map("globalMapLeft",true),map("globalMapRight",false))
          globalMapLeft = Some(l)
          println(s"wmts left = ${taskState.now().wms1}")
          addTileLayer(l,taskState.now().wms1)
          println(s"wmts right = ${taskState.now().wms2}")
          globalMapRight = Some(r)
          addTileLayer(r,taskState.now().wms2)
          l.sync(r, SyncMapOptions())
          r.sync(l, SyncMapOptions())
        ),
        div(idAttr("global-container"), cls("my-container"),
          div(idAttr("globalMapLeft"), cls("mapLeft"), cls("map"),
          ),
          div(idAttr("globalMapRight"), cls("mapRight"), cls("map"),
          )
        ),
      )
    )
  end renderGlobalDashboard

  // implicit conversion to leaflet.sync monkey patched version of Map
  implicit def map2sync(jq: Map_): SMap = jq.asInstanceOf[SMap]

  private def linkButton(name: String): Element =
    button(name,typ("button"),
      backgroundColor <-- annotationState.signal.map(_.linkType).map(link=>if link == name then "Silver" else "WhiteSmoke"),
      onClick --> { _ => annotationState.update(state=>state.copy(linkType = name, step=1)) }
    )
  private def changeButton(name: String): Element =
    button(name,typ("button"),
      backgroundColor <-- annotationState.signal.map(_.changeType).map(change=>if change == name then "Silver" else "WhiteSmoke"),
      onClick --> { _ => annotationState.update(state=>state.copy(changeType = name)) }
    )

  def map(name: String, left: Boolean): Map_ =
    println("create map")
    L.map(name, MapOptions().setAttributionControl(!left).setZoomControl(left)).setView(LatLngLiteral(48.8, 2.3), zoom = 18)

  private def addTileLayer(map: Map_, wmts:String): Unit =
    val split = wmts.split('?')
    val baseUrl = split.head
    val layers = split(1).split(',')
    for (layer <- layers) {
      if baseUrl.contains("wmts") then
        tileLayer(
          s"$baseUrl?" +
            "&REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0" +
            "&STYLE=normal" +
            "&TILEMATRIXSET=PM" +
            "&FORMAT=image/jpeg" +
            s"&LAYER=$layer" +
            "&TILEMATRIX={z}" +
            "&TILEROW={y}" +
            "&TILECOL={x}",
          TileLayerOptions().setMinZoom(0).setMaxZoom(20).setMaxNativeZoom(18).setAttribution("IGN-F/Geoportail").setTileSize(256)
        ).addTo(map)
      else
        tileLayer.wms(s"$baseUrl?",
          WMSOptions().setMinZoom(0).setMaxZoom(20).setMaxNativeZoom(18).setAttribution(baseUrl)
        ).addTo(map)
    }

  /*
  tileLayer(
    "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
    TileLayerOptions().setMaxZoom(18).setAttribution("""&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>""")
  ).addTo(m)
  */

  def renderAnnotate(): Element =
    div(
      // Wait for the component to be mounted before adding the leaflet and syncs
      onMountCallback(ctx =>
        val (l, r) = (map("mapLeft", true), map("mapRight", false))
        mapLeft = Some(l)
        println(s"wmts left = ${taskState.now().wms1}")
        addTileLayer(l,taskState.now().wms1)
        println(s"wmts right = ${taskState.now().wms2}")
        mapRight = Some(r)
        addTileLayer(r,taskState.now().wms2)
        l.sync(r, SyncMapOptions())
        r.sync(l, SyncMapOptions())
        updateMaps(l,r,geoJSON.now().get._1,geoJSON.now().get._2)
      ),
      div(idAttr("my-container"), cls("my-container"),
        div(idAttr("mapLeft"), cls("mapLeft"), cls("map"),
        ),
        div(idAttr("mapRight"), cls("mapRight"), cls("map"),
        )
      ),
      div(cls("toolbar"),
        children(
          linkButton("1-1"),
          linkButton("0-1"),
          linkButton("1-0"),
          linkButton("1-m"),
          linkButton("n-1"),
          linkButton("n-m"),
          linkButton("other"),
          button(">",
            backgroundColor := "Orchid",
            disabled <-- annotationState.signal.map(_.linkType).map(link=> link == ""),
            onClick --> { _ => annotationState.update(state=>state.copy(step=1)) }
          )
        ) <-- annotationState.signal.map(_.step==0),
        children(
          button("<",
            backgroundColor := "Orchid",
            onClick --> { _ => annotationState.update(state=>state.copy(step=0)) }
          ),
          changeButton("no change"),
          changeButton("construction"),
          changeButton("destruction"),
          //changeButton("extension"),
          //changeButton("reduction"),
          changeButton("reconstruction"),
          changeButton("IDK"),
          label("Quality issue: "),input("Quality issue",`type`:="checkbox",
            checked <-- annotationState.signal.map(_.quality),
            inContext( thisNode => onClick --> { _=> annotationState.update(state=>state.copy(quality = thisNode.ref.checked)) } )
          ),
          input("Comment",placeholder := "Any comment?",value <-- annotationState.signal.map(_.comment),
            onInput.mapToValue --> annotationState.updater[String]((state,commentValue)=>state.copy(comment=commentValue))),
          button("save",
            backgroundColor := "Crimson",
            disabled <-- annotationState.signal.map(_.changeType == ""),
            onClick --> { _ =>
              //annotationStep.update(_=>0)
              println("save")
              val sampleFile = taskState.now().sampleFile
              val taskFile = taskState.now().taskFile
              read[Sample](s"$dir/$sampleFile").`then`(content =>
                println(s"read $sampleFile:\n${JSON.stringify(content, space=2)}")
                val task = content.tasks.find(_.task == taskFile).get
                val annotation = Annotation()
                annotation.username = stateVar.now().username
                annotation.link = annotationState.now().linkType
                annotation.change = annotationState.now().changeType
                annotation.quality = annotationState.now().quality
                annotation.comment = annotationState.now().comment
                task.annotations.push(annotation)
                println(s"now\n${JSON.stringify(content, space=2)}")
                write(s"$dir/$sampleFile",JSON.stringify(content, space=2))
                gitPush(stateVar.now().username, stateVar.now().token, sampleFile, s"Update $taskFile for ${annotation.username}")
              )
              nextFeature().`then`(updated=>
                if updated then
                  println("updated")
                  mapLeft.zip(mapRight).foreach((l,r)=>updateMaps(l,r,geoJSON.now().get._1,geoJSON.now().get._2))
                  currentPage.update(_ => Page.Annotate)
                else
                  annotationFinished.update(_=>true)
                  currentPage.update(_ => Page.Dashboard)
              )
            }
          )
        ) <-- annotationState.signal.map(_.step==1)
      )
    )
  end renderAnnotate

  private def renderInputRow(
    error: UserState => Option[String]
  )(
    mods: Modifier[HtmlElement]*
  ): HtmlElement = {
    val errorSignal = stateVar.signal.map(_.displayError(error))
    div(
      cls("-inputRow"),
      cls("x-hasError") <-- errorSignal.map(_.nonEmpty),
      mods,
      child.maybe <-- errorSignal.map(_.map(err => div(cls("-error"), err)))
    )
  }
  def renderLogin(): Element =
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

end Main

enum Page(val name: String):
  case Home extends Page("Home")
  case Dashboard extends Page("Dashboard")
  case AnnotatedMaps extends Page("Annotated Maps")
  case Annotate extends Page("Annotate")
  case Login extends Page("Login")
  case Help extends Page("Help")

final class Model {
  val currentPage: Var[Page] = Var(Page.Home)
  val geoJSON: Var[Option[(GeoJSON__[Geometry, GeoJsonProperties], GeoJSON__[Geometry, GeoJsonProperties])]] = Var(None)
  case class TaskState(date1: String="", date2: String="", wms1: String="", wms2: String="", sampleFile: String="", taskFile:String="")
  val taskState = Var(TaskState())
  val datasetsVar: Var[Option[js.Array[Task_]]] = Var(None)
  var iterator: Option[Iterator[Task_]] = None
  var mapLeft: Option[Map_] = None
  var mapRight: Option[Map_] = None
  var globalMapLeft: Option[Map_] = None
  var globalMapRight: Option[Map_] = None
  val annotationFinished: Var[Boolean] = Var(false)
  case class AnnotationState(linkType: String = "", changeType: String = "", quality: Boolean = false, comment: String = "", step: Int = 0)
  val annotationState = Var(AnnotationState())
  val datasetSelected: Var[String] = Var("")

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
  /*{
    val (username, token) = (localStorageGetOrElse("username"), localStorageGetOrElse("token"))
    val validated = username.nonEmpty && token.nonEmpty
    println(s"logged $username $validated")
    if validated then cloneData(token).map(datasets => {
        datasetsVar.update(_ => Some(datasets.asInstanceOf[js.Array[Task_]]))
        iterator = datasets.asInstanceOf[js.Array[Task_]].iterator
        nextFeature()
      })
    Var(UserState(username,token,validated))
  }*/

  private def geoJSONUpdate(left:GeoJSON__[Geometry,GeoJsonProperties], right:GeoJSON__[Geometry,GeoJsonProperties]): Unit =
    geoJSON.update(previous =>
      previous match {
        case Some((previousLeft, previousRight)) =>
          mapLeft.zip(mapRight).foreach((l,r)=>
            previousLeft.removeFrom(l)
            previousRight.removeFrom(r)
          )
        case None =>
          println(s"previous was None")
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
      println(s"new task ${currentTask_.task.task}")
      if currentTask_.task.annotations.exists(_.username == stateVar.now().username) then nextFeature()
      else
        taskState.update(state => state.copy(
          date1 = currentTask_.dates(0),date2=currentTask_.dates(1),
          wms1=currentTask_.wmts(0), wms2=currentTask_.wmts(1),
          sampleFile = currentTask_.sample, taskFile=currentTask_.task.task
        ))
        annotationState.update(_=>AnnotationState())
        read[String](s"$dir/${currentTask_.task.task}", false).`then`(content =>
          val newTask = content.asInstanceOf[String]
          val left = parseGeoJSONAndFilter(newTask, currentTask_.dates(0)).setStyle(leftLeftStyle)
          val right = parseGeoJSONAndFilter(newTask, currentTask_.dates(1)).setStyle(rightRightStyle)
          geoJSONUpdate(left, right)
          true
        )
    else Promise.resolve(false)
  val tokenWriter: Observer[String] = stateVar.updater[String]((state, token) => state.copy(token = token))
  val usernameWriter: Observer[String] = stateVar.updater[String]((state, username) => state.copy(username = username))
  val submitter: Observer[UserState] = Observer[UserState] { state =>
    if (state.hasErrors) {
      stateVar.update(_.copy(showErrors = true))
    } else {
      //dom.window.alert(s"Username: ${state.username}; Token: ${state.token}")
      dom.window.localStorage.setItem("token", state.token)
      dom.window.localStorage.setItem("username", state.username)
      stateVar.update(_.copy(validated = true))
      currentPage.update(_ => Page.Home)
      println("Cloning?")
      cloneData(state.token).`then`(datasets =>
        println("clone data finished!")
        datasetsVar.update(_ => Some(datasets.asInstanceOf[js.Array[Task_]]))
        iterator = Some(datasets.asInstanceOf[js.Array[Task_]].iterator)
        nextFeature()
      )
      println("Cloned?")
    }
  }

  def logInOut(): Unit = {
    if (stateVar.now().validated) {
      println("logging out")
      dom.window.localStorage.removeItem("token")
      dom.window.localStorage.removeItem("username")
      currentPage.update(_ => Page.Home)
      stateVar.update(_.copy(validated = false))
    } else {
      println("logging in")
      currentPage.update(_ => Page.Login)
    }
  }
}
