import org.eclipse.jgit.api.Git
import org.eclipse.jgit.transport.{CredentialItem, CredentialsProvider, URIish}
import upickle.default.*

import java.io.File
import java.nio.file.Files

object CleanupAnnotations {
  private case class Annotation(username: String, types: List[String], quality: Boolean, comment: String) derives ReadWriter
  private case class Task(task: String, annotations: List[Annotation]) derives ReadWriter
  private case class Tasks(tasks: List[Task]) derives ReadWriter

  private case class Stats(filesScanned: Int = 0, duplicatesFound: Int = 0)

  // Configuration from Environment Variables
  private val REPO_URL = sys.env.getOrElse("REPO_URL", sys.error("REPO_URL env var not set"))
  private val TOKEN = sys.env.getOrElse("GITHUB_TOKEN", sys.error("GITHUB_TOKEN env var not set"))
  private val TEMP_DIR = Files.createTempDirectory("annotator-cleanup-").toFile
  private val TARGET_BRANCH = "main"

  def main(args: Array[String]): Unit = {
    val dryRun = args.contains("--dry-run")
    println(s"=== Annotator Cleanup Tool ===")
    println(s"Target Repo: $REPO_URL")
    println(s"Mode: ${if (dryRun) "DRY RUN (No changes)" else "LIVE (Will commit & push)"}")
    println(s"Temp Dir: $TEMP_DIR")
    println()

    try {
      println("1. Cloning repository...")
      cloneRepo()
      println("2. Scanning for duplicates...")
      val stats = processSamples()
      println("\n=== SUMMARY ===")
      println(s"Files scanned: ${stats.filesScanned}")
      println(s"Duplicates found: ${stats.duplicatesFound}")
      if (stats.duplicatesFound == 0) {
        println("No duplicates found. Nothing to do.")
        return
      }
      if (dryRun) {
        println("\n[DRY RUN] Changes simulated. Add no flag to apply.")
      } else {
        val confirm = scala.io.StdIn.readLine("\nCommit and push changes? (y/n): ")
        if (confirm.toLowerCase.startsWith("y")) {
          commitAndPush()
          println("Changes pushed successfully!")
        } else {
          println("Changes discarded.")
        }
      }
    } catch {
      case e: Exception =>
        println(s"ERROR: ${e.getMessage}")
        e.printStackTrace()
    } finally {
      // Cleanup temp dir
      deleteRecursively(TEMP_DIR)
      println("Temp directory cleaned up.")
    }
  }
  private def cloneRepo(): Unit = {
    val git = Git.cloneRepository()
      .setURI(REPO_URL)
      .setDirectory(TEMP_DIR)
      .setBranch(TARGET_BRANCH)
      .setCredentialsProvider(
        new CredentialsProvider {
          override def isInteractive: Boolean = false
          override def supports(items: CredentialItem*): Boolean = true
          override def get(uri: URIish, items: CredentialItem*): Boolean = {
            items.foreach {
              case p: CredentialItem.Username => p.setValue("git")
              case p: CredentialItem.Password => p.setValue(TOKEN.toCharArray)
            }
            true
          }
        }
      ).call()
    println(s"Cloned to ${TEMP_DIR.getAbsolutePath}")
  }

  private def processSamples(): Stats = {
    var stats = Stats()
    val sampleFiles = getAllSampleFiles(TEMP_DIR)

    sampleFiles.foreach { file =>
      stats = stats.copy(filesScanned = stats.filesScanned + 1)
      val content = Files.readString(file.toPath)
      // Parse JSON
      val json = read[Tasks](content)
      val tasks = json.tasks
      var modified = false
      val cleanedTasks = tasks.map { taskObj =>
        val annotations = taskObj.annotations
        val seen = scala.collection.mutable.Set[String]()
        val uniqueAnnotations = annotations.filter { ann =>
          val username = ann.username
          // we ignore the content and focus on finding multiple annotations with the same user
          if (seen.contains(username)) {
            println(s"Found duplicate for ${taskObj.task} with user $username")
            false // Duplicate
          } else {
            seen.add(username)
            true
          }
        }

        if (uniqueAnnotations.length != annotations.length) {
          modified = true
          val removedCount = annotations.length - uniqueAnnotations.length
          stats = stats.copy(
            duplicatesFound = stats.duplicatesFound + removedCount,
          )
          // Update the task object with clean list
          taskObj.copy(annotations = uniqueAnnotations)
        } else
          taskObj
      }
      if (modified) {
        // Write back
        val newJson = write(json.copy(tasks = cleanedTasks), indent = 2)
        Files.writeString(file.toPath, newJson)
        println(s"Fixed: ${file.getName} (removed ${stats.duplicatesFound} duplicates)")
      }
    }

    stats
  }

  private def getAllSampleFiles(dir: File): List[File] = {
    dir.listFiles().toList.flatMap { f =>
      if (f.isDirectory) getAllSampleFiles(f)
      else if (f.getName.equals("tasks.json")) List(f)
      else Nil
    }
  }

  private def commitAndPush(): Unit = {
    val git = Git.open(TEMP_DIR)
    // Add all changes
    git.add().addFilepattern(".").call()
    // Commit
    val commit = git.commit()
      .setMessage("fix: cleanup duplicate annotations")
      .setAuthor("Cleanup Bot", "bot@subdense.local")
      .call()
    println(s"Committed: ${commit.getId}")
    // Push
    git.push()
      .setRemote("origin")
      .setCredentialsProvider(
        new CredentialsProvider {
          override def isInteractive: Boolean = false
          override def supports(items: CredentialItem*): Boolean = true
          override def get(uri: URIish, items: CredentialItem*): Boolean = {
            items.foreach {
              case p: CredentialItem.Username => p.setValue("git")
              case p: CredentialItem.Password => p.setValue(TOKEN.toCharArray)
            }
            true
          }
        }
      )
      .call()
    println("Pushed to remote.")
  }

  private def deleteRecursively(file: File): Unit = {
    if (file.isDirectory) file.listFiles().foreach(deleteRecursively)
    file.delete()
  }
}