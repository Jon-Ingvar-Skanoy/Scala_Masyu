

object Main {
  def main(args: Array[String]): Unit = {
    val newBoard: Board = Board("src/main/scala/puzzle_unsolved.txt", 1);
    newBoard.input()
    //    println(newBoard.tiles(0)(2).ttype)

    newBoard.border_Top()
    newBoard.border_Bottom()
    newBoard.border_Left()
    newBoard.border_Right()
    newBoard.printBoard()
    newBoard.print_ugly()
  }
}

