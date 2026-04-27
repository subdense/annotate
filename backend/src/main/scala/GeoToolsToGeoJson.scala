import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import org.geotools.api.feature.simple.SimpleFeature
import org.geotools.data.simple.SimpleFeatureCollection
import org.locationtech.jts.geom.{Geometry, Polygon}

import java.io.{File, PrintWriter}
import scala.jdk.CollectionConverters.*

// GeoJSON Case Classes
case class GeoJsonGeometry(`type`: String, coordinates: Json)
case class GeoJsonFeature(`type`: String, properties: Map[String, String], geometry: GeoJsonGeometry)
case class GeoJsonFeatureCollection(`type`: String, features: List[GeoJsonFeature])

object GeoToolsToGeoJson {
  /**
   * Recursive helper to build the coordinate structure based on geometry type.
   * Returns a Json value representing the coordinates array.
   */
  private def buildCoordinates(geometry: Geometry): Json = {
    def coordinateArray(g: Geometry): Json =
      val points = g.getCoordinates.map(c => Json.arr(Json.fromDouble(c.x).get, Json.fromDouble(c.y).get))
      Json.arr(points: _*)
    def polygonArray(polygon: Polygon): Json =
      // Exterior
      val extRing = coordinateArray(polygon.getExteriorRing)
      // Holes
      val holes = (0 until polygon.getNumInteriorRing).map { j =>
        coordinateArray(polygon.getInteriorRingN(j))
      }
      Json.arr(extRing +: holes: _*)

    geometry.getGeometryType match {

      // 1. Point -> [x, y]
      case "Point" =>
        val c = geometry.getCoordinate
        Json.arr(Json.fromDouble(c.x).get, Json.fromDouble(c.y).get)

      // 2. LineString -> [[x, y], [x, y], ...]
      case "LineString" =>
        coordinateArray(geometry)

      // 3. Polygon -> [[[x, y], ...], [[x, y], ...]] (Exterior + Holes)
      case "Polygon" =>
        val polygon = geometry.asInstanceOf[Polygon]
        polygonArray(polygon)

      // 4. MultiPoint -> [[x, y], [x, y], ...] (Same structure as LineString)
      case "MultiPoint" =>
        coordinateArray(geometry)

      // 5. MultiLineString -> [[[x, y], ...], [[x, y], ...]] (Same structure as Polygon rings)
      case "MultiLineString" =>
        val lines = (0 until geometry.getNumGeometries).map { i =>
          coordinateArray(geometry.getGeometryN(i))
        }
        Json.arr(lines: _*)

      // 6. MultiPolygon -> [[[[x, y], ...], ...], ...]
      case "MultiPolygon" =>
        val polys = (0 until geometry.getNumGeometries).map { i =>
          val poly = geometry.getGeometryN(i).asInstanceOf[Polygon]
          polygonArray(poly)
        }
        Json.arr(polys: _*)
      case _ =>
        throw new IllegalArgumentException(s"Unsupported geometry type: ${geometry.getGeometryType}")
    }
  }

  /** Extract properties from a Feature */
  private def extractProperties(feature: SimpleFeature): Map[String, String] = {
    val schema = feature.getFeatureType
    val geom = schema.getGeometryDescriptor.getLocalName
    schema.getAttributeDescriptors.asScala.filter {att => att.getLocalName != geom}.map { desc =>
      val key = desc.getLocalName
      val value = feature.getAttribute(key)
      key -> (if (value != null) value.toString else null)
    }.toMap
  }

  /** Convert a single Feature to GeoJsonFeature */
  private def featureToGeoJson(feature: SimpleFeature): GeoJsonFeature = {
    val geometry = feature.getDefaultGeometry.asInstanceOf[Geometry]
    val coords = buildCoordinates(geometry)

    GeoJsonFeature(
      `type` = "Feature",
      geometry = GeoJsonGeometry(
        `type` = geometry.getGeometryType,
        coordinates = coords
      ),
      properties = extractProperties(feature)
    )
  }

  /** Convert entire FeatureCollection to GeoJsonFeatureCollection */
  private def collectionToGeoJson(collection: SimpleFeatureCollection): GeoJsonFeatureCollection = {
    val features = scala.collection.mutable.ArrayBuffer[GeoJsonFeature]()
    val iterator = collection.features()
    while (iterator.hasNext)
      features += featureToGeoJson(iterator.next())
    GeoJsonFeatureCollection(
      `type` = "FeatureCollection",
      features = features.toList
    )
  }

  /** Write to file with proper formatting */
  def writeGeoJson(collection: SimpleFeatureCollection, file: File): Unit = {
    val geoJson = collectionToGeoJson(collection)
    implicit val geoJsonGeometryEncoder: Encoder[GeoJsonGeometry] = deriveEncoder
    implicit val geojsonEncoder: Encoder[GeoJsonFeature] = deriveEncoder
    implicit val fooEncoder: Encoder[GeoJsonFeatureCollection] = deriveEncoder

    // Custom printer matching JavaScript's JSON.stringify
    val jsLikePrinter = Printer(
      dropNullValues = false,
      indent = "  ",
      lbraceRight = "\n",
      rbraceLeft = "\n",
      lbracketRight = "\n",
      rbracketLeft = "\n",
      lrbracketsEmpty = "\n",
      arrayCommaRight = "\n",
      objectCommaRight = "\n",
      colonLeft = "",
      colonRight = " ",
    )
    val jsonString = geoJson.asJson.printWith(jsLikePrinter)

    val writer = new PrintWriter(file)
    try {
      writer.write(jsonString)
    } finally {
      writer.close()
    }
  }
}