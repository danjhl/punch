// Projects

lazy val root =
  project
    .in(file("."))
    .settings(rootSettings)
    .enablePlugins(GraalVMNativeImagePlugin)

lazy val jar =
  project
    .in(file("build/jar"))
    .settings(jarSettings)
    .enablePlugins(AssemblyPlugin)
    .dependsOn(root)

// Settings

lazy val rootSettings =
  Seq(
    name                       := "punch",
    version                    := "0.1.2",
    scalaVersion               := "2.12.8",
    organization               := "io.punch",
    organizationName           := "punch",
    graalVMNativeImageOptions ++= graalSettings,
    libraryDependencies       ++= rootDeps)

lazy val graalSettings =
  Seq("--allow-incomplete-classpath")

lazy val jarSettings =
  Seq(
    name                       := "punch-jar",
    version                    := "0.1.2",
    scalaVersion               := "2.12.8",
    organization               := "io.punch",
    organizationName           := "punch",
    mainClass in assembly      := Some("punch.Main"),
    test in assembly           := {},
    libraryDependencies       ++= jarDeps)

// Dependencies

lazy val rootDeps =
  Seq(
    Deps.scalaTest % Test,
    Deps.scribe,
    Deps.zio,
    Deps.jlineNative,
    Deps.fastparse)

lazy val jarDeps =
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