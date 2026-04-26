package fr.umrlastig.annotator.ui.pages

import com.raquo.laminar.api.L.{*, given}
import fr.umrlastig.annotator.Model
import fr.umrlastig.annotator.ui.Page
import org.scalajs.dom.HTMLDivElement

object Dashboard {
  def renderDashboard()(implicit model: Model): Element =
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
    def username = model.stateVar.now().username
    val userSignal = model.datasetsVar.signal.map(datasetsOption=>datasetsOption.map(datasets=>datasets.toArray
      .groupBy(_.dataset)
      .map((d,array)=>
        DatasetR(d.split("/").head,array.groupBy(_.sample).map((s,array)=>
          SampleR(s.split("/").take(2).mkString("-"),array.length,array.count(_.task.annotations.exists(_.username == username)))
        ).toList.sortBy(_.name))
      ).toList
    ).get)
    val allUsersSignal = model.datasetsVar.signal.map(datasetsOption=>datasetsOption.map(datasets=>datasets.toArray
      .groupBy(_.dataset)
      .map((d,array)=>
        DatasetR(d.split("/").head,array.groupBy(_.sample).map((s,array)=>
          SampleR(s.split("/").take(2).mkString("-"),array.length,array.count(_.task.annotations.length > 0))
        ).toList.sortBy(_.name))
      ).toList
    ).get)
    div(
      h1(Page.Dashboard.name),
      h2(s"User: $username"),
      children <-- userSignal.split(_.name)(newRenderDataset),
      h2(s"All users"),
      children <-- allUsersSignal.split(_.name)(newRenderDataset)
    )
  end renderDashboard
}
