import scala.io.Source
import Function.tupled

object Day5 {

  val filename = "input.txt"
  case class Line(x1 : Int, y1 : Int, x2: Int, y2: Int)

  def main(args: Array[String]) : Unit = {

    val answer = Source.fromFile(filename).getLines()
      .map(in => {
        val s"$x1,$y1 -> $x2,$y2" = in
        Line(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
      })
        .flatMap(toPointsOnLine)
        .toSeq
        .groupBy(identity)
        .view
        .mapValues(_.size)
        .filter((k, v) => v > 1)
        .size

        println(s"Answer to Part 2 is $answer")
  }

  private def toPointsOnLine : (Line => Seq[(Int, Int)]) = {
    line => if (line.x1 == line.x2) {
      // vertical
      val step = if (line.y1 < line.y2) then 1 else -1
      for (y <- line.y1 to line.y2 by step) yield (line.x1, y)
    } else if (line.y1 == line.y2) {
      // horizontal
      val step = if (line.x1 < line.x2) then 1 else -1
      for (x <- line.x1 to line.x2 by step) yield (x, line.y1)
    } else {
      // 45deg diagonal
      val stepX = if (line.x1 < line.x2) then 1 else -1
      val stepY = if (line.y1 < line.y2) then 1 else -1
      (line.x1 to line.x2 by stepX).zip(line.y1 to line.y2 by stepY)
    }
  }

}
