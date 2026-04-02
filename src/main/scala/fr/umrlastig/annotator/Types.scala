package fr.umrlastig.annotator

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

object Types {
  // Helper to create a new JS object matching the trait
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