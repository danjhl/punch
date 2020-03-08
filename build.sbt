// Projects

lazy val root =
  project
    .in(file("."))
    .settings(rootSettings)
    .enablePlugins(GraalVMNativeImagePlugin)

// Settings

lazy val rootSettings =
  Seq(
    name                       := "punch",
    version                    := "0.1.0",
    scalaVersion               := "2.12.8",
    organization               := "io.punch",
    organizationName           := "punch",
    mainClass in assembly      := Some("punch.Main"),
    test in assembly           := {},
    graalVMNativeImageOptions ++= graalSettings,
    libraryDependencies       ++= rootDeps)

lazy val graalSettings =
  Seq("--allow-incomplete-classpath")

// Dependencies

lazy val rootDeps =
  Seq(
    Deps.scalaTest % Test,
    Deps.scribe,
    Deps.zio,
    Deps.jline,
    Deps.jlineJansi,
    Deps.jansi,
    Deps.fastparse)

// Tasks

lazy val installToolchainUbuntu = taskKey[Unit]("installs toolchain on ubuntu")
lazy val installGraalVmUbuntu = taskKey[Unit]("installs GraalVM on ubuntu")
lazy val copyBin = taskKey[Unit]("copy binary to root directory")

installToolchainUbuntu := { Tasks.installToolchainUbuntu() }
installGraalVmUbuntu := { Tasks.installGraalVmUbuntu() }
copyBin := { Tasks.copyBin(name.value) }

// Aliases

addCommandAlias("packBin", ";show graalvm-native-image:packageBin;copyBin")