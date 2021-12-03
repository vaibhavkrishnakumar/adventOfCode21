import scala.io.Source
import collection.immutable.SortedMap

object Day3 {

  val filename = "input.txt"

  def main(args: Array[String]) : Unit = {

    val gammaStr = Source.fromFile(filename).getLines()
      .flatMap(_.toSeq.zipWithIndex)
      .map(_.swap)
      .map(p => (p._1, if (p._2 == '0') -1 else 1))
      .toSeq
      .groupMap(_._1)(_._2)
      // if sum is positive, more 1s than 0s
      .transform((_, values) => if (values.sum > 0) '1' else '0')
      // Sorted ensures we keep the indices in order
      .to(SortedMap)
      .values
      .mkString

    // b"111..." upto length of gammaStr, in decimal
    val mask = (math.pow(2, gammaStr.length) - 1).toInt

    val gamma = Integer.parseInt(gammaStr, 2)
    val epsilon = gamma^mask

    val answer = gamma*epsilon
    println(s"Answer is $answer")
  }

}
