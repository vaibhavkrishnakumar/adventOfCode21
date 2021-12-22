import scala.io.Source
import Function.tupled

object Day6 {

  val filename = "input.txt"
  val days = 80

  def main(args: Array[String]) : Unit = {
    var ages = Source.fromFile(filename)
      .mkString
      .split(",")
      .map(_.trim)
      .map(_.toInt)
      .toSeq

    0 until days foreach { day => ages = tick(ages) }
    val answer = ages.size

    println(s"Answer is $answer")
  }

  private def tick(ages: Seq[Int]) : Seq[Int] = {
    ages.flatMap(age => age match {
      case 0 => Seq(6, 8)
      case _ => Seq(age - 1)
    })
  }

}
