import scala.io.Source
import scala.collection.mutable.Seq

object Day2 {

  val filename = "input.txt"

  def main(args: Array[String]) : Unit = {

    var horizontal = 0
    var depth = 0

    Source.fromFile(filename).getLines()
      .map(_ match {
        case s"$instruction $magnitude" => Some(instruction, Integer.parseInt(magnitude))
        case _ => None
      })
      .map(_.get)
      .foreach((instruction, magnitude) => instruction match {
        case "forward" => horizontal += magnitude
        case "up" => depth -= magnitude
        case "down" => depth += magnitude
      })
    
    val answer = horizontal*depth

    println(s"Answer to Part 1 is $answer")
  }

}
