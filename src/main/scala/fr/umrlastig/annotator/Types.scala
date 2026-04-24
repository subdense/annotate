package fr.umrlastig.annotator

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

@js.native
trait DatasetList extends js.Object {
  var datasets: js.Array[String] = js.native
}
@js.native
trait Dataset extends js.Object {
  var dates: js.Array[String] = js.native
  var wmts: js.Array[String] = js.native
  var samples: js.Array[String] = js.native
}
@js.native
trait Annotation extends js.Object {
  var username: String = js.native
  var types: js.Array[String] = js.native
  var quality: Boolean = js.native
  var comment: String = js.native
}
@js.native
trait Task extends js.Object {
  var task: String = js.native
  var annotations: js.Array[Annotation] = js.native
}
@js.native
trait Sample extends js.Object {
  var tasks: js.Array[Task] = js.native
}
/*@js.native
trait Dataset_ extends js.Object {
  var dates: js.Array[String] = js.native
  var wmts: js.Array[String] = js.native
  var tasks: js.Array[String] = js.native
}*/
@js.native
trait Task_ extends js.Object {
  var dataset: String = js.native
  var dates: js.Array[String] = js.native
  var wmts: js.Array[String] = js.native
  var sample: String = js.native
  var task: Task = js.native
  var modalities: js.Array[js.Array[String]] = js.native
}

object Types {
  // Helper to create a new JS object matching the trait
  // TODO find a better alternative? it is kind of ugly
  def newAnnotation(): Annotation =
    js.Dynamic.literal(
      username = "",
      types = js.Array[String](),
      quality = false,
      comment = ""
    ).asInstanceOf[Annotation]
  def newTask(): Task =
    js.Dynamic.literal(
      task = "",
      annotations = js.Array[Annotation]()
    ).asInstanceOf[Task]
  def newSample(): Sample =
    js.Dynamic.literal(
      tasks = js.Array[Task]()
    ).asInstanceOf[Sample]
  def newDataset(): Dataset =
    js.Dynamic.literal(
      dates = js.Array[String](),
      wmts = js.Array[String](),
      samples = js.Array[String]()
    ).asInstanceOf[Dataset]
  def newDatasetList(): DatasetList =
    js.Dynamic.literal(
      datasets = js.Array[String]()
    ).asInstanceOf[DatasetList]
  def newTask_(): Task_ =
    js.Dynamic.literal(
      dataset = "",
      dates = js.Array[String](),
      wmts = js.Array[String](),
      sample = "",
      task = newTask(),
      modalities = js.Array[js.Array[String]]()
    ).asInstanceOf[Task_]
  def newConfig(): Config =
    js.Dynamic.literal(
      url = "",
      dir = "",
      fsName = "",
      useIsomorphicProxy = "",
      corsProxyIsomorphic = "",
      corsProxyDefault = "",
      annotationSetup = js.Dynamic.literal()
    ).asInstanceOf[Config]
}