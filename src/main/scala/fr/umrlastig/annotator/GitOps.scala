package fr.umrlastig.annotator

import com.raquo.airstream.state.Var
import org.scalajs.dom.console
import subdense.BuildInfo
import typings.gitEssentials.distTypesModelsFsClientMod.{EncodingOptions, RmOptions}
import typings.gitEssentials.clientsFsIndexedDbFsClientMod.IndexedDbFsClient
import typings.gitEssentials.gitEssentialsStrings.utf8
import typings.gitEssentials.clientsHttpWebHttpClientMod.makeWebHttpClient
import typings.gitEssentials.distTypesApiAddMod.AddParams
import typings.gitEssentials.{distTypesApiCloneMod as cloneMod, distTypesClientsHttpWebHttpClientMod as whco, mod as essentials}
import typings.gitEssentials.distTypesApiCommitMod.CommitParams
import typings.gitEssentials.distTypesApiPullMod.PullParams
import typings.gitEssentials.distTypesApiPushMod.{PushParams, PushResult}
import typings.gitEssentials.distTypesModelsAuthMod.Auth
import typings.gitEssentials.distTypesModelsAuthorMod.Author

import scala.scalajs.js
import scala.scalajs.js.URIUtils.encodeURIComponent
import scala.scalajs.js.{JSON, Promise}
import scala.util.Random

object GitOps {
  // read the config from config.json file, added from CI of the instantiation repo
  // TODO default config from resources to deploy even if not instantiated?
  val config: Config = JSON.parse(BuildInfo.configJson).asInstanceOf[Config]

  private def useIsomorphicProxy = config.useIsomorphicProxy.toBoolean
  private def corsProxyIsomorphic = config.corsProxyIsomorphic
  private def corsProxyDefault = config.corsProxyDefault
  private def annotationSetup = config.annotationSetup

  private val client = IndexedDbFsClient(config.fsname)
  private val proxy = if useIsomorphicProxy then corsProxyIsomorphic else corsProxyDefault
  private val http_ = """^https?:\/\/"""
  private def transform(url: String, b: js.UndefOr[Boolean]) = if useIsomorphicProxy then s"$proxy/${url.replaceAll(http_, "")}" else s"$proxy?url=${encodeURIComponent(url)}"

  def read[T](file: String, parse: Boolean = true): Promise[T] = client.readFile(file, EncodingOptions().setEncoding(utf8))
    .`then`(content => (if parse then JSON.parse(content.asInstanceOf[String]) else content).asInstanceOf[T])

  def write(file: String, content: String): Promise[Unit] = client.writeFile(file, content, EncodingOptions().setEncoding(utf8))

  // TODO add branch option? (always work on default branch when cloning, an other config may be useful?)
  def cloneData(token: String, rng: Random, message: Var[Option[String]]): Promise[js.Array[Task_]] =
    message.set(Some("Cleaning up data"))
    client.rm(config.dir, RmOptions().setRecursive(true).setForce(true))
      .`catch`(err => {
        console.error("Cleanup failed, continuing anyway?", err)
        js.Promise.reject(err)
      })
      .`then`[Unit](_ => {
        console.info(s"Cleanup done for ${config.dir}. Cloning...")
        message.set(Some("Cloning data"))
        essentials.clone_(cloneMod.CloneParams(
            dir = config.dir,
            fs = client,
            http = makeWebHttpClient(whco.WebHttpClientOptions().setTransformRequestUrl(transform)),
            url = config.url)
          .setOnAuth((url, auth) => auth.setUsername(token))
          .setDepth(1)
          .setSingleBranch(true)
        )
      })
      .`catch`(err => {
        console.error("Git clone failed", err)
        js.Promise.reject(err)
      })
      .`then`[DatasetList](_ => {
        message.set(Some("Parsing dataset list"))
        read[DatasetList](s"${config.dir}/datasets.json")
      })
      .`catch`(err => {
        console.error("Failed to read datasets.json", err)
        js.Promise.reject(err)
      })
      .`then`(content => {
        message.set(Some("Parsing datasets"))
        Promise.all(content.datasets.map(datasetName => read[Dataset](s"${config.dir}/$datasetName")
          .`catch`(err => {
            console.error(s"Missing dataset: $datasetName", err)
            js.Promise.reject(err)
          })
          .`then`(d => (datasetName, d))))
      })
      .`then`(datasets => {
        message.set(Some("Parsing samples"))
        Promise.all(datasets.asInstanceOf[js.Array[(String, Dataset)]].flatMap((datasetName, dataset) =>
          dataset.samples.map(sample => read[Sample](s"${config.dir}/$sample")
            .`catch`(err => {
              console.error(s"Error reading sample: $sample", err)
              js.Promise.reject(err)
            })
            .`then`(s =>
              //console.debug(s"Annotation setup from static config : ${js.Object.keys(annotationSetup).toSeq}")
              //console.debug(s"Config for dataset $datasetName : ${annotationSetup.asInstanceOf[js.Dynamic].selectDynamic(datasetName).asInstanceOf[js.Array[js.Array[String]]]}")
              js.Dynamic.literal(name = datasetName, dates = dataset.dates, wmts = dataset.wmts, sampleFile = sample, sample = s,
                modalities = annotationSetup.asInstanceOf[js.Dynamic].selectDynamic(datasetName))
            )
          )
        ))
      })
      .`then`(samples =>
        message.set(Some("Parsing tasks"))
        samples.asInstanceOf[js.Array[js.Dynamic]].flatMap(s =>
          val modalities = s.modalities.asInstanceOf[js.Array[js.Array[String]]]
          //console.debug(s"Setting up sample with modalities : $modalities ; dims : ${modalities.toSeq.size} x ${modalities(0).toSeq.size}")
          val tasks = rng.shuffle(s.sample.asInstanceOf[Sample].tasks)
          tasks.map(t =>
            val task = Types.newTask_()
            task.dataset = s.name.asInstanceOf[String]
            task.dates = s.dates.asInstanceOf[js.Array[String]]
            task.wmts = s.wmts.asInstanceOf[js.Array[String]]
            task.sample = s.sampleFile.asInstanceOf[String]
            task.task = t
            task.modalities = modalities
            //console.debug(s"Task: $task")
            task
          )
        )
      )

  def gitPush(username: String, token: String, file: String, message: String): Promise[PushResult] =
    console.info(s"start add $file with ${transform(config.url, js.undefined)}")

    essentials.add(AddParams(dir = config.dir, filepath = file, fs = client))
      .`catch`(err => {
        console.error(s"Failed to add $file", err)
        js.Promise.reject(err)
      })
      .`then`[String](_ => {
        console.info(s"start commit $message with ${transform(config.url, js.undefined)}")
        essentials.commit(CommitParams(dir = config.dir, fs = client, message = message).setAuthor(Author(username)))
      })
      .`catch`(err => {
        console.error("Commit failed", err)
        js.Promise.reject(err)
      })
      .`then`[PushResult](c => {
        console.info(s"start push")
        essentials.push(
          PushParams(dir = config.dir, fs = client, http = makeWebHttpClient(whco.WebHttpClientOptions().setTransformRequestUrl(transform)))
            .setUrl(config.url)
            .setOnAuth((_, _) => Auth().setUsername(token))
        )
      })
      .`catch`(err => {
        console.error("Push failed! Data might be lost.", err)
        js.Promise.reject(err)
      })

  def gitPull(username: String, token: String): Promise[Unit] =
    essentials.pull(
        PullParams(dir = config.dir, fs = client, http = makeWebHttpClient(whco.WebHttpClientOptions().setTransformRequestUrl(transform)))
          .setUrl(config.url)
          .setOnAuth((_, auth) => auth.setUsername(token))
          .setAuthor(Author(username))
          .setSingleBranch(true)
      )
      .`catch`(err => {
        console.error("Pull failed:", err)
        js.Promise.reject(err)
      })
}