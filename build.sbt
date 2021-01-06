// Projects

lazy val root = project
  .in(file("."))
  .settings(rootSettings)
  .enablePlugins(AssemblyPlugin)

// Settings

lazy val rootSettings = Seq(
  name                       := "punch",
  version                    := "0.1.3-SNAPSHOT",
  scalaVersion               := "2.12.8",
  organization               := "io.punch",
  organizationName           := "punch",
  mainClass in assembly      := Some("punch.Main"),
  test in assembly           := {},
  libraryDependencies       ++= rootDeps)

// Dependencies

lazy val rootDeps = Seq(
  Deps.scalaTest % Test,
  Deps.scalaMock % Test,
  Deps.scribe,
  Deps.zio,
  Deps.jline,
  Deps.jlineJansi,
  Deps.jansi,
  Deps.fastparse)