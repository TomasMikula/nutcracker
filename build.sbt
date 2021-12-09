name := "nutcracker"

organization := "com.github.tomasmikula"

lazy val scala213 = "2.13.7"
lazy val scala3   = "3.1.0"

scalaVersion := scala3
crossScalaVersions := Seq(scala213, scala3)

autoCompilerPlugins := true

scalacOptions ++=
  Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
  ) ++ (
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) =>
        Seq("-Ykind-projector")
      case Some((2, 13)) =>
        Seq("-language:higherKinds", "-Wunused:imports", "-Xsource:3")
      case other =>
        sys.error(s"Unexpected Scala version $other")
    }
  )

testFrameworks += new TestFramework("scalaprops.ScalapropsFramework")

Test / parallelExecution := false // currently, ScalaProps does not support parallel execution

lazy val ScalazVersion = "7.4.0-M9"

libraryDependencies ++= Seq(
  "org.typelevel" %% "algebra" % "2.2.3",
  "org.scalaz" %% "scalaz-core" % ScalazVersion,
  "com.github.scalaprops" %% "scalaprops" % "0.8.3",

  "org.scalatest" %% "scalatest" % "3.2.10" % "test",
  "org.scalatestplus" %% "scalacheck-1-15" % "3.2.10.0" % "test",
  "org.scalaz" %% "scalaz-scalacheck-binding" % ScalazVersion % "test",
) ++ (CrossVersion.partialVersion(scalaVersion.value) match {
  case Some((2, 13)) =>
    Seq(compilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full))
  case _ =>
    Seq()
})

fork := true


/******************
 *** Publishing ***
 ******************/

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

import ReleaseTransformations._

releaseCrossBuild := true

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  releaseStepCommandAndRemaining("+publishSigned"),
  releaseStepCommandAndRemaining("sonatypeReleaseAll"),
  setNextVersion,
  commitNextVersion,
  //pushChanges,
)
