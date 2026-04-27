package fr.umrlastig.annotator

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom.console
import typings.geojson.mod.*
import typings.leaflet.mod as L
import typings.leaflet.mod.*
import typings.leafletSync.*
import typings.leafletSync.leafletMod.{Map as SMap, *}

import scala.collection.mutable
import scala.language.implicitConversions

val rnd = new scala.util.Random(42)
object Utils {
  private def toHexString(color: (Double, Double, Double)): String = {
    val (r, g, b) = ((color._1 * 255).floor.intValue, (color._2 * 255).floor.intValue, (color._3 * 255).floor.intValue)
    f"#$r%02x$g%02x$b%02x"
  }
  private def toRGBString(color: (Double, Double, Double)): String = {
    val (r, g, b) = ((color._1 * 255).floor.intValue, (color._2 * 255).floor.intValue, (color._3 * 255).floor.intValue)
    f"rgb($r,$g,$b)"
  }

  // input: h in [0,360] and s,v in [0,1] - output: r,g,b in [0,1]
  private def hsl2rgb(h: Double, s: Double, l: Double) =
    val a = s * Math.min(l, 1 - l)

    def f(n: Int) =
      val k = (n + h / 30) % 12
      l - a * Math.max(Seq(k - 3, 9 - k, 1).min, -1)

    (f(0), f(8), f(4))

  def getColor: String = toHexString(hsl2rgb(rnd.nextDouble() * 360, 0.8, 0.5))

  def getRGBColor: String = toRGBString(hsl2rgb(rnd.nextDouble() * 360, 0.8, 0.5))
  
  private def addTileLayer(map: Map_)(wmts: String): Unit =
    val defaultWMTS = Map("REQUEST" -> "GetTile", "SERVICE" -> "WMTS", "VERSION" -> "1.0.0", "STYLE" -> "normal", "TILEMATRIXSET" -> "PM", "FORMAT" -> "image/jpeg", "TILEMATRIX" -> "{z}", "TILEROW" -> "{y}", "TILECOL" -> "{x}")

    def makeWMTS(url: String, map: Map[String, String]) = s"$url?${map.map((k, v) => k + "=" + v).mkString("&")}"

    val tmpLayers = mutable.ArrayBuffer.empty[Layer]
    map.eachLayer((layer: Layer) => tmpLayers += layer)
    tmpLayers.foreach(map.removeLayer)
    val split = wmts.split('?')
    val baseUrl = split.head
    // can not always use WMTS default param : need to parse additional parameters provided in the wmts url
    // TODO for now use convention URL/wmts?layer1,layer2,...&PARAM1=...&... which is not the correct call with "LAYER="
    val layers = split(1).split('&')(0).split(',')
    val params = split(1).split('&').tail.map(s => {
      val kv = s.split('=');
      (kv(0), kv(1))
    }).toMap
    val wmtsParams: Map[String, String] = defaultWMTS.keys.map(k => if (params.contains(k)) (k, params(k)) else (k, defaultWMTS(k))).toMap
    if baseUrl.contains("wmts") then
      for (layer <- layers) {
        tileLayer(makeWMTS(baseUrl, Map("LAYER" -> layer) ++ wmtsParams),
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

  def updateMaps(leftMap: Map_, rightMap: Map_,
                 leftGeoJSON: GeoJSON__[Geometry, GeoJsonProperties], rightGeoJSON: GeoJSON__[Geometry, GeoJsonProperties],
                 leftWms: Option[String] = None, rightWms: Option[String] = None): Unit =
    leftWms.foreach(addTileLayer(leftMap))
    rightWms.foreach(addTileLayer(rightMap))
    leftGeoJSON.addTo(leftMap)
    rightGeoJSON.addTo(rightMap)
    leftMap.fitBounds(
      if rightGeoJSON.getBounds().isValid()
      then rightGeoJSON.getBounds()
      else leftGeoJSON.getBounds())

  // implicit conversion to leaflet.sync monkey patched version of Map
  implicit def map2sync(jq: Map_): SMap = jq.asInstanceOf[SMap]

  def getMap(name: String, left: Boolean): Map_ =
    console.info("create map")
    L.map(name, MapOptions().setAttributionControl(!left).setZoomControl(left)).setView(LatLngLiteral(48.8, 2.3), zoom = 18)

  def syncMaps(leftName: String, rightName: String, mapVar: Var[Option[(Map_, Map_)]]): (Map_, Map_) =
    val (l, r) = (getMap(leftName, true), getMap(rightName, false))
    l.sync(r, SyncMapOptions())
    r.sync(l, SyncMapOptions())
    mapVar.update(_ => Some((l, r)))
    (l, r)
}