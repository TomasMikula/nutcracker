name := "nutcracker"

version := "0.2-SNAPSHOT"

organization := "com.github.tomasmikula"

scalaVersion := "2.12.1"


resolvers += "bintray/non" at "http://dl.bintray.com/non/maven"
resolvers += Resolver.sonatypeRepo("releases")

autoCompilerPlugins := true
addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.3" cross CrossVersion.binary)
addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.17")

scalastyleFailOnError := true

scalacOptions ++= Seq(
  "-language:higherKinds",
  "-Xlint",
  "-unchecked",
  "-deprecation",
  "-feature",
  //"-Xfatal-warnings",
  "-Yno-adapted-args",
  "-Ypartial-unification",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard",
  "-Ypatmat-exhaust-depth", "40",
  "-Xfuture")

javacOptions ++= Seq(
  "-source", "1.8",
  "-target", "1.8",
  "-Xlint:unchecked",
  "-Xlint:deprecation")

lazy val root = (project in file(".")).
  dependsOn(RootProject(uri("https://github.com/TomasMikula/Principled.git#v0.1")))

libraryDependencies ++= Seq(
  "org.typelevel" %% "algebra" % "0.6.0",
  "org.scalaz" %% "scalaz-core" % "7.3.0-M8",
  "org.scalaz" %% "scalaz-scalacheck-binding" % "7.3.0-M8",
  "com.chuusai" %% "shapeless" % "2.3.2",
  "com.github.julien-truffaut"  %%  "monocle-core" % "1.4.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4"
)

fork := true
