Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / tlBaseVersion := "0.0"

ThisBuild / organization     := "io.github.pierrenodet"
ThisBuild / organizationName := "Pierre Nodet"
ThisBuild / startYear        := Some(2023)
ThisBuild / licenses         := Seq(License.Apache2)
ThisBuild / developers       := List(tlGitHubDev("pierrenodet", "Pierre Nodet"))

ThisBuild / tlSonatypeUseLegacyHost := false

val Scala3 = "3.3.0"

ThisBuild / scalaVersion := Scala3

val PrimaryJava = JavaSpec.temurin("8")
val LTSJava     = JavaSpec.temurin("17")
val GraalVM     = JavaSpec.graalvm("17")

ThisBuild / githubWorkflowJavaVersions := Seq(PrimaryJava, LTSJava, GraalVM)

lazy val root = tlCrossRootProject
  .aggregate(core, benchmarks, examples)

ThisBuild / tlCiReleaseBranches := Seq.empty

ThisBuild / githubWorkflowAddedJobs +=
  WorkflowJob(
    "coverage",
    "Generate Coverage Report",
    githubWorkflowJobSetup.value.toList ++ List(
      WorkflowStep.Sbt(
        List("coverage", s"${root.jvm.id}/test", "coverageAggregate"),
        name = Some("Generate Coverage Report")
      ),
      WorkflowStep.Use(
        UseRef.Public(
          "codecov",
          "codecov-action",
          "v2"
        )
      )
    ),
    javas = List(PrimaryJava),
    scalas = List(Scala3)
  )

ThisBuild / githubWorkflowAddedJobs +=
  WorkflowJob(
    "site",
    "Generate Site",
    githubWorkflowJobSetup.value.toList ++ List(
      WorkflowStep.Sbt(
        List(s"${core.jvm.id}/doc"),
        name = Some("Generate site")
      ),
      WorkflowStep.Use(
        UseRef.Public("peaceiris", "actions-gh-pages", "v3.8.0"),
        Map(
          "github_token" -> s"$${{ secrets.GITHUB_TOKEN }}",
          "publish_dir"  -> {
            val path = (ThisBuild / baseDirectory).value.toPath.toAbsolutePath
              .relativize((core.jvm / Compile / doc / target).value.toPath)
            (0 until path.getNameCount).map(path.getName).mkString("/")
          },
          "keep_files"   -> "false"
        ),
        name = Some("Publish site"),
        cond = Some(
          s"github.event_name != 'pull_request' && github.ref == 'refs/heads/main'"
        )
      )
    ),
    scalas = List(Scala3),
    javas = List(PrimaryJava)
  )

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    name := "matou",
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit"            % "1.0.0-M8" % Test,
      "org.scalameta" %%% "munit-scalacheck" % "1.0.0-M8" % Test
    )
  )
  .settings(docs.settings)

lazy val benchmarks: Project = project
  .in(file("benchmarks"))
  .settings(
    moduleName := "matou-benchmarks",
    libraryDependencies ++= Seq(
      "dev.ludovic.netlib" % "blas" % "3.0.3"
    )
  )
  .settings(
    Jmh / compile := (Jmh / compile).dependsOn(Test / compile).value,
    Jmh / run     := (Jmh / run).dependsOn(Jmh / compile).evaluated
  )
  .enablePlugins(JmhPlugin, NoPublishPlugin)
  .dependsOn(core.jvm)

ThisBuild / resolvers += "jitpack" at "https://jitpack.io"
ThisBuild / fork / run := true

lazy val examples: Project = project
  .in(file("examples"))
  .settings(
    moduleName := "matou-examples",
    libraryDependencies ++= Seq(
      "com.github.micycle1" % "processing-core-4" % "4.1.1"
    )
  )
  .enablePlugins(NoPublishPlugin)
  .dependsOn(core.jvm)
