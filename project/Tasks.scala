import sbt._
import scala.sys.process._

object Tasks {
  def installToolchainUbuntu(): Unit = {
    (s"sudo apt-get --assume-yes install libc6-dev zlib1g-dev" !)
  }

  def installGraalVmUbuntu(): Unit = {
    val home = sys.env.get("HOME").get

    val graalVersion = "1.0.0-rc15"
    val graalDistribution = s"graalvm-ce-${graalVersion}"
    val graalParentDir = s"${home}/.local/share"
    val fileName = s"${graalDistribution}-linux-amd64.tar.gz"
    val gzFile = s"${graalParentDir}/${fileName}"
    val nativeImage = s"${graalParentDir}/${graalDistribution}/bin/native-image"
    val nativeImageTargetDir = s"${home}/.local/bin"
    val nativeImageTarget = s"${nativeImageTargetDir}/native-image"
    val graalUrl = s"https://github.com/oracle/graal/releases/download/vm-${graalVersion}/${fileName}"

    if (!file(nativeImage).exists) {
      println("installing graalvm...")

      (s"mkdir -p ${graalParentDir}" !)
      (url(graalUrl) #> file(gzFile) !)
      (s"tar zxf ${gzFile} -C ${graalParentDir}" !)
      (s"rm -f ${gzFile}" !)
      (s"mkdir -p ${nativeImageTargetDir}" !)
      (s"ln -f -s ${nativeImage} ${nativeImageTarget}" !)
    }
    else {
      println("graalvm already installed")
    }
  }

  def copyBin(name: String): Unit = {
    (s"cp target/graalvm-native-image/${name} ${name}" !)
  }
}

