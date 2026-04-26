package fr.umrlastig.annotator.ui

enum Page(val name: String):
  case Home extends Page("Home")
  case Dashboard extends Page("Dashboard")
  case Leaderboard extends Page("Leaderboard")
  case AnnotatedMaps extends Page("Annotated Maps")
  case Annotate extends Page("Annotate")
  case Login extends Page("Login")
  case Help extends Page("Help")

object State {
}