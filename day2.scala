import scala.io.Source

object Day2 {

  val filename = "input.txt"

  var horizontal = 0
  var depth = 0
  var aim = 0

  def main(args: Array[String]) : Unit = {

    Source.fromFile(filename).getLines()
      .map(_ match {
        case s"$instruction $magnitude" => Some(instruction, Integer.parseInt(magnitude))
        case _ => None
      })
      .map(_.get)
// Part 1
//    .foreach((instruction, magnitude) => instruction match {
//      case "forward" => horizontal += magnitude
//      case "up" => depth -= magnitude
//      case "down" => depth += magnitude
//    })
// Part 2
      .foreach((instruction, magnitude) => instruction match {
        case "forward" => forward(magnitude)
        case "up" => aim -= magnitude
        case "down" => aim += magnitude
      })

    val answer = horizontal*depth

    println(s"Answer is $answer")
  }

  private def forward(magnitude: Int): Unit = {
    horizontal += magnitude
    depth += aim*magnitude
  }

}
