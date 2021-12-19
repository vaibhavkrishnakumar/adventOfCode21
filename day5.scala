import scala.io.Source
import Function.tupled

object Day5 {

  val filename = "input.txt"
  case class Line(x1 : Int, y1 : Int, x2: Int, y2: Int)

  def main(args: Array[String]) : Unit = {
    val grid : Seq[Seq[Int]] = Seq()
    val answer = Source.fromFile(filename).getLines()
      .map(s => {
        val s"$x1,$y1 -> $x2,$y2" = s
        Line(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
      })
        .filter(isHorizontalOrVertical)
        .flatMap(toPointsOnLine)
        .toSeq
        .groupBy(identity)
        .view
        .mapValues(_.size)
        .filter((k, v) => v > 1)
        .size

        println(s"Answer to Part 1 is $answer")
  }

  private def isHorizontalOrVertical : (Line => Boolean) = {
    line => line.x1 == line.x2 || line.y1 == line.y2
  }

  private def toPointsOnLine : (Line => Seq[(Int, Int)]) = {
    line => if (line.x1 == line.x2) {
      val (min, max) = minMax(line.y1, line.y2)
      for (y <- min to max) yield (line.x1, y)
    } else if (line.y1 == line.y2) {
      val (min, max) = minMax(line.x1, line.x2)
      for (x <- min to max) yield (x, line.y1)
    } else {
      Seq()
    }
  }

  private def minMax(a:Int, b:Int) : (Int, Int) = {
    val min = Math.min(a, b)
    val max = if (min == a) b else a
    (min, max)
  }

}
