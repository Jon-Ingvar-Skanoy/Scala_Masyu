

object Main {
  def main(args: Array[String]): Unit = {
    val newBoard: Board = Board("src/main/scala/puzzle_unsolved.txt", 1);
    newBoard.input()
    //    println(newBoard.tiles(0)(2).ttype)
    newBoard.draw_left(-1,4,1)

    newBoard.printBoard()
    newBoard.print_ugly()
  }
}

