import scala.io.Source
import scala.collection.mutable.Seq

object Day1 {

  val filename = "input.txt"

  def main(args: Array[String]) : Unit = {

    val input = Source.fromFile(filename).getLines()
      .map(Integer.parseInt(_))
      .toSeq
    val shifted = input.drop(1)

    val pairs = input zip shifted
    val answer = pairs
      .map(p => p._1 - p._2)
      .filter(_ < 0)
      .size

    println(s"Answer is $answer")
  }

}
