name := "nutcracker"

version := "0.2-SNAPSHOT"

organization := "com.github.tomasmikula"

scalaVersion := "2.12.1"

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
  "-Xfatal-warnings",
  "-Yno-adapted-args",
  "-Ypartial-unification",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard",
  "-Ypatmat-exhaust-depth", "40",
  "-Xfuture")

testFrameworks += new TestFramework("scalaprops.ScalapropsFramework")

parallelExecution in Test := false // currently, ScalaProps does not support parallel execution

libraryDependencies ++= Seq(
  "org.typelevel" %% "algebra" % "0.6.0",
  "org.scalaz" %% "scalaz-core" % "7.3.0-M10",
  "com.chuusai" %% "shapeless" % "2.3.2",
  "com.github.scalaprops" %% "scalaprops" % "0.4.1",

  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalaz" %% "scalaz-scalacheck-binding" % "7.3.0-M10" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
)

fork := true


/******************
 *** Publishing ***
 ******************/

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

pomExtra := (
  <url>https://github.com/TomasMikula/nutcracker</url>
  <licenses>
    <license>
      <name>Apache License, Version 2.0</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:TomasMikula/nutcracker.git</url>
    <connection>scm:git:git@github.com:TomasMikula/nutcracker.git</connection>
  </scm>
  <developers>
    <developer>
      <id>TomasMikula</id>
      <name>Tomas Mikula</name>
    </developer>
  </developers>)
