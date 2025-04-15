import sbt.Keys.*

inThisBuild(
  List(
    scalaVersion := "3.6.4",
  )
)

val project_name = "scalajs-vfs"
val project_version = "1.0.0-SNAPSHOT"

scalacOptions ++= Seq("-feature", "-deprecation")

lazy val root = (project in file("."))
  .settings(
    organization := "org.enricobn",
    name := project_name,
    version := project_version,
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % Test,
    libraryDependencies += "org.scalamock" %%% "scalamock" % "7.3.0" % Test

  )
  .enablePlugins(ScalaJSPlugin)

scalacOptions ++= Seq(
  "-deprecation",
)
    
