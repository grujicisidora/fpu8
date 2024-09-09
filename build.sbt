ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

val chiselVersion = "3.6.0"
val chiselTestVersion = "0.6.2"
addCompilerPlugin("edu.berkeley.cs" %% "chisel3-plugin" % chiselVersion cross CrossVersion.full)
lazy val root = (project in file("."))
  .settings(
    name := "fpu8",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % chiselVersion,  // Update to the latest Chisel version
      "edu.berkeley.cs" %% "chiseltest" % chiselTestVersion // For testing
    )
  )