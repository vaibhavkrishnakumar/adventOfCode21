import scala.io.Source

object Day4 {

  val filename = "input.txt"

  def main(args: Array[String]) : Unit = {
    val (called, boards) = readInput()
    val markedBoards : Seq[IndexedSeq[IndexedSeq[Boolean]]] = boards.map(_.map(_.map(_ => false)))

    for (call <- called) {
      for (board <- boards.indices) {
        for (row <- boards(board).indices) {
          for (num <- boards(board)(row).indices) {
            if (call == boards(board)(row)(num)) {
              markedBoards(board)(row)(num) = true
            }
          }
        }
      }
    }
  }

  def readInput(): (Seq[Int], Seq[IndexedSeq[IndexedSeq[Int]]]) = {
    val x: Iterator[String] = Source.fromFile(filename).getLines()

    // First line is bingo numbers called
    val called: Seq[Int] = x.next().split(",").map(_.toInt).toIndexedSeq

    var currentBoard : IndexedSeq[IndexedSeq[Int]] = IndexedSeq()
    var boards: Seq[IndexedSeq[IndexedSeq[Int]]] = Seq()

    x.next() // Skip the second line, which is blank
    for (in <- x) {
      if (in.isEmpty) {
        boards = boards ++ Seq(currentBoard)
        currentBoard = IndexedSeq()
      } else {
        currentBoard = currentBoard ++ IndexedSeq(in.strip.split(" +").map(_.toInt).toIndexedSeq)
      }
    }
    boards = boards ++ Seq(currentBoard)
    (called, boards)
  }
}
