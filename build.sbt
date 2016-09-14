name := "nutcracker"

version := "0.1.5-SNAPSHOT"

organization := "com.github.tomasmikula"

scalaVersion := "2.11.8"


resolvers += "bintray/non" at "http://dl.bintray.com/non/maven"
resolvers += Resolver.sonatypeRepo("releases")

autoCompilerPlugins := true
addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.0" cross CrossVersion.binary)
addCompilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full)
addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.15")

scalastyleFailOnError := true

scalacOptions ++= Seq(
  "-Xlint",
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Xfatal-warnings",
  "-Yno-adapted-args",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ypatmat-exhaust-depth", "40",
  "-Xfuture")

javacOptions ++= Seq(
  "-source", "1.8",
  "-target", "1.8",
  "-Xlint:unchecked",
  "-Xlint:deprecation")

lazy val root = (project in file(".")).
  dependsOn(RootProject(uri("https://github.com/TomasMikula/Principled.git")))

libraryDependencies ++= Seq(
  "org.typelevel" %% "algebra" % "0.5.0",
  "org.scalaz" %% "scalaz-core" % "7.3.0-M5",
  "com.chuusai" %% "shapeless" % "2.3.2",
  "com.github.julien-truffaut"  %%  "monocle-core" % "1.2.2",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.2"
)

fork := true

// A configuration which is like 'compile' except it performs additional
// static analysis. Execute static analysis via `lint:compile`
val LintTarget = config("lint").extend(Compile)

addMainSourcesToLintTarget 

addSlowScalacSwitchesToLintTarget

def addMainSourcesToLintTarget = {
  inConfig(LintTarget) {
    Defaults.compileSettings ++ Seq(
      sources in LintTarget := {
        val lintSources = (sources in LintTarget).value
        lintSources ++ (sources in Compile).value
      }
    )
  }
}

def addSlowScalacSwitchesToLintTarget = {
  inConfig(LintTarget) {
    scalacOptions in LintTarget ++= Seq(
      "-Ywarn-unused-import",
      "-Ywarn-dead-code"
    )
  }
}
