// Projects

lazy val root = 
  project.in(file("."))
         .settings(rootSettings)
         .enablePlugins(GraalVMNativeImagePlugin)

// Settings

lazy val rootSettings = 
  Seq(name             := "punch"         ,
      version          := "0.1.0-SNAPSHOT",
      scalaVersion     := "2.12.8"        ,
      organization     := "io.punch"      ,
      organizationName := "punch"         ) ++ rootDeps

// Dependencies

lazy val rootDeps = 
  libraryDependencies ++= Seq(Deps.scalaTest % Test,
                              Deps.scribe          ,
                              Deps.catsEffect      ,
                              Deps.jline           ,
                              Deps.fastparse       )

// Tasks

lazy val installToolchainUbuntu = taskKey[Unit]("installs toolchain on ubuntu")
lazy val installGraalVmUbuntu = taskKey[Unit]("installs GraalVM on ubuntu")
lazy val copyBin = taskKey[Unit]("copy binary to root directory")

installToolchainUbuntu := { Tasks.installToolchainUbuntu() }
installGraalVmUbuntu := { Tasks.installGraalVmUbuntu() }
copyBin := { Tasks.copyBin(name.value) }

// Aliases

addCommandAlias("packBin", ";show graalvm-native-image:packageBin;copyBin")
