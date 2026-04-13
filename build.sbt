import org.scalajs.linker.interface.ModuleSplitStyle

val upickleVersion = "4.4.3"

lazy val annotator = project.in(file("."))
  .enablePlugins(ScalaJSPlugin) // Enable the Scala.js plugin in this project
  .enablePlugins(ScalablyTypedConverterExternalNpmPlugin)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    scalaVersion := "3.3.3",

    // Tell Scala.js that this is an application with a main method
    scalaJSUseMainModuleInitializer := true,

    /* Configure Scala.js to emit modules in the optimal way to
     * connect to Vite's incremental reload.
     * - emit ECMAScript modules
     * - emit as many small modules as possible for classes in the "annotator" package
     * - emit as few (large) modules as possible for all other classes
     *   (in particular, for the standard library)
     */
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(
          ModuleSplitStyle.SmallModulesFor(List("subdense")))
    },

    /* Depend on the scalajs-dom library.
     * It provides static types for the browser DOM APIs.
     */
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.8.0",
    libraryDependencies += "com.lihaoyi" %%% "upickle" % upickleVersion,
    libraryDependencies += "com.raquo" %%% "laminar" % "17.0.0",
    // Depends on Airstream 17.0.0 & URL DSL 0.6.2
    // Tell ScalablyTyped that we manage `npm install` ourselves
    externalNpm := baseDirectory.value,

    buildInfoKeys := Seq[BuildInfoKey](
       name,
       version,
       scalaVersion,
       BuildInfoKey.action("configJson") {
         val source = scala.io.Source.fromFile("src/main/resources/config.json")
         val res = source.getLines().mkString
         source.close()
         res
       }
      ),
      buildInfoPackage := "subdense"
  )

val jgitVersion = "7.6.0.202603022253-r"
val gtVersion = "34.3"
val circeVersion = "0.14.15"
// backend scripts
lazy val backend = (project in file("backend"))
  .settings(
    name := "annotator-backend",
    scalaVersion := "3.3.1", // Match your main project version
    resolvers ++= Seq(
      "osgeo" at "https://repo.osgeo.org/repository/release"
    ),
    libraryDependencies ++= Seq(
      // JSON parsing
      "com.lihaoyi" %% "upickle" % upickleVersion,
      // Git operations
      "org.eclipse.jgit" % "org.eclipse.jgit" % jgitVersion,
      // SSH support for JGit (optional, but good for HTTPS with tokens)
      "org.eclipse.jgit" % "org.eclipse.jgit.ssh.apache" % jgitVersion,
      // jts io for geojson parsing
      "org.locationtech.jts.io" % "jts-io-common" % "1.20.0",
      "org.geotools" % "gt-main" % gtVersion,
      "org.geotools" % "gt-geojson-store" % gtVersion,
      // Circe for json writing
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "io.circe" %% "circe-jawn" % circeVersion,
    )
  )