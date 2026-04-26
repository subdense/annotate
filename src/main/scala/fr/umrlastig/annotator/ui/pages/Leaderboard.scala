package fr.umrlastig.annotator.ui.pages

import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import fr.umrlastig.annotator.ui.Page
import fr.umrlastig.annotator.{Model, Task_, UserRank}
import org.scalajs.dom.{HTMLDivElement, console}

import scala.collection.mutable
import scala.scalajs.js

object Leaderboard {
  def renderLeaderboard()(implicit model: Model): Element =
    val currentUser = model.stateVar.now().username
    val leaderboardProcessor = Observer[Option[js.Array[Task_]]] {
      case None =>
        console.info("leaderboardProcessor with no task")
        model.leaderboardData.update(_ => List.empty)
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
        model.leaderboardData.update(_ => ranked)
    }

    div(
      h1(Page.Leaderboard.name),

      // Empty state message
      child.maybe <-- model.leaderboardData.signal.map { ranks =>
        if (ranks.isEmpty) Some(div("No annotations yet.")) else None
      },

      // Leaderboard table (always renders, but empty if no data)
      div(
        div(cls("leaderboard-header"), span("Rank"), span("User"), span("Count"), span("%")),

        // This is the key fix: children takes a Signal[List[Element]]
        children <-- model.leaderboardData.signal.map { ranks =>
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
      model.datasetsVar.signal --> leaderboardProcessor
    )
  end renderLeaderboard
}
