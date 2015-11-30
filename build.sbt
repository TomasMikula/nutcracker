name := "NutCracker"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.7"


resolvers += "bintray/non" at "http://dl.bintray.com/non/maven"
resolvers += Resolver.sonatypeRepo("releases")

autoCompilerPlugins := true
addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.3")
addCompilerPlugin("org.spire-math" % "kind-projector" % "0.6.0" cross CrossVersion.binary)
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)

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

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "acyclic" % "0.1.3" % "provided",
  "com.github.mpilquist" %% "simulacrum" % "0.4.0",
  "org.spire-math" %% "algebra" % "0.3.2-SNAPSHOT",
  "org.spire-math" %% "algebra-std" % "0.3.2-SNAPSHOT",
  "org.scalaz" %% "scalaz-core" % "7.2.0-RC1",
  "com.chuusai" %% "shapeless" % "2.2.5",
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

addFoursquareLinterToLintTarget 

removeFoursquareLinterFromCompileTarget 

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

def addFoursquareLinterToLintTarget = {
  Seq(
    resolvers += "Linter Repository" at "https://hairyfotr.github.io/linteRepo/releases",
    addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.12"),
    // See https://github.com/HairyFotr/linter#list-of-implemented-checks
    // for a list of checks that foursquare linter implements.
    // By default linter enables all checks.
    // I don't mind using match on boolean variables.
    scalacOptions in LintTarget += "-P:linter:disable:PreferIfToBooleanMatch"
  )
}

def removeFoursquareLinterFromCompileTarget = {
  // We call addCompilerPlugin in project/plugins.sbt to add a depenency
  // on the foursquare linter so that sbt magically manages the JAR for us.
  // Unfortunately, addCompilerPlugin also adds a switch to scalacOptions
  // in the Compile config to load the plugin.
  // The bit below removes all switches that could be passed to scalac
  // about foursquare linter during a non-lint compile.
  scalacOptions in Compile := (scalacOptions in Compile).value filterNot { switch =>
    switch.startsWith("-P:linter:") ||
      "^-Xplugin:.*/com[.]foursquare[.]lint/.*linter.*[.]jar$".r.pattern.matcher(switch).find
  }
}
