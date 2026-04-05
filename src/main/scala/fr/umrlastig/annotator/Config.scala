package fr.umrlastig.annotator

import typings.leaflet.mod.PathOptions

import scala.scalajs.js

@js.native
trait Config extends js.Object {
  var url: String = js.native
  var dir: String = js.native
  var fsname: String = js.native
  var useIsomorphicProxy: String = js.native
  var corsProxyIsomorphic: String = js.native
  var corsProxyDefault: String = js.native
  var annotationSetup: js.Object = js.native
}

object Config {
  val leftRightStyle: PathOptions = PathOptions().setColor("#ff7800").setWeight(5).setOpacity(0.25).setDashArray("20 20").setDashOffset("10").setFill(false)
  val leftLeftStyle: PathOptions = PathOptions().setColor("#78ff00").setWeight(2).setOpacity(0.8).setFill(false)
  val rightRightStyle: PathOptions = PathOptions().setColor("#ff7800").setWeight(2).setOpacity(0.8).setFill(false)
  val rightLeftStyle: PathOptions = PathOptions().setColor("#78ff00").setWeight(5).setOpacity(0.25).setDashArray("20 20").setDashOffset("10").setFill(false)
}
