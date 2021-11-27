name := "nutcracker"

organization := "com.github.tomasmikula"

lazy val scala213 = "2.13.7"

scalaVersion := scala213
crossScalaVersions := Seq(scala213)

autoCompilerPlugins := true
addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)

scalacOptions ++=
  Seq(
    "-language:higherKinds",
    "-Xlint:-unused,_", // Exclude "unused", because:
                        //  - it produces some false positives;
                        //  - sometimes we use them as just implicit evidence.
    "-unchecked",
    "-deprecation",
    "-feature",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Ypatmat-exhaust-depth", "40",
  ) ++ (
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) => Seq("-Wunused:imports", "-Xsource:3")
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
)

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
