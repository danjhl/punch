package punch.cli

case class Activity(
    name: String,
    project: String,
    start: Long,
    end: Long) {
  
  def timeInSeconds: Long = end - start
}