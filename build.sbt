lazy val ScalaTestVersion  = "3.0.5"
lazy val AntlrVersion  = "4.7.1"

lazy val root = (project in file("."))
  .enablePlugins(Antlr4Plugin)
  .settings(
    inThisBuild(List(
      organization := "ca.valencik",
      scalaVersion := "2.12.6",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Antlr4 Scala Example",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % ScalaTestVersion % Test,
      "org.antlr" % "antlr4" % AntlrVersion,
    ),
    antlr4GenListener in Antlr4 := false,
    antlr4GenVisitor in Antlr4 := true,
    antlr4PackageName in Antlr4 := Some("ca.valencik.antlr4fun")
  )
