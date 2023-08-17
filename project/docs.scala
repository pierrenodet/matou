import sbt._
import sbt.Keys._

object docs{
  lazy val settings = Seq(
    Compile / doc / scalacOptions ++=
      Seq(
        "-project",
        "matou",
        "-siteroot",
        "docs",
        "-project-version",
        version.value,
        "-social-links:" +
          "github::https://github.com/pierrenodet/matou," +
          "twitter::https://twitter.com/pierrenodet",
        "-project-footer",
        s"Copyright (c) 2023, ${organizationName.value}",
        "-source-links:github://pierrenodet/matou",
        "-revision",
        "main"
      ),
    Compile / doc / scalacOptions ++= Seq("-snippet-compiler:compile"),
  )
}
