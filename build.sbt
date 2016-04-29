name := "nutcracker"

version := "0.1-SNAPSHOT"

organization := "com.github.tomasmikula"

scalaVersion := "2.11.8"


resolvers += "bintray/non" at "http://dl.bintray.com/non/maven"
resolvers += Resolver.sonatypeRepo("releases")

autoCompilerPlugins := true
addCompilerPlugin("org.spire-math" % "kind-projector" % "0.6.0" cross CrossVersion.binary)
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.12")

scalastyleFailOnError := true

scalacOptions ++= Seq(
  "-Xlint",
  "-unchecked",
  "-deprecation",
  "-feature",
//  "-Xfatal-warnings",
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

libraryDependencies ++= Seq(
  "org.spire-math" %% "algebra" % "0.3.2-SNAPSHOT",
  "org.spire-math" %% "algebra-std" % "0.3.2-SNAPSHOT",
  "org.scalaz" %% "scalaz-core" % "7.3.0-M2",
  "com.chuusai" %% "shapeless" % "2.3.0",
  "com.github.julien-truffaut"  %%  "monocle-core" % "1.2.0",
  "org.principled" %% "principled" % "0.1-SNAPSHOT",
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.4"
)

fork := true

// A configuration which is like 'compile' except it performs additional
// static analysis. Execute static analysis via `lint:compile`
val LintTarget = config("lint").extend(Compile)

addMainSourcesToLintTarget 

addSlowScalacSwitchesToLintTarget

addWartRemoverToLintTarget

removeWartRemoverFromCompileTarget 

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

def addWartRemoverToLintTarget = {
  import wartremover._
  import Wart._
  inConfig(LintTarget) {
    wartremoverErrors ++= Seq(
      Wart.Any,
      Wart.Serializable,
      Wart.Product,
      Wart.ListOps,
      Wart.OptionPartial,
      Wart.EitherProjectionPartial,
      Wart.Any2StringAdd
    )
  }
}

def removeWartRemoverFromCompileTarget = {
  // WartRemover's sbt plugin calls addCompilerPlugin which always adds
  // directly to the Compile configuration. The bit below removes all
  // switches that could be passed to scalac about WartRemover during
  // a non-lint compile.
  scalacOptions in Compile := (scalacOptions in Compile).value filterNot { switch =>
    switch.startsWith("-P:wartremover:") ||
    "^-Xplugin:.*/org[.]brianmckenna/.*wartremover.*[.]jar$".r.pattern.matcher(switch).find
  }
}
