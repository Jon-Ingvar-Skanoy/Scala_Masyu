

object Main {
  def main(args: Array[String]): Unit = {
    val newBoard: Board = Board("C:/Users/isakk/IdeaProjects/Scala-readable/src/main/scala/puzzle_unsolved.txt", 1);
    newBoard.input()
    //    println(newBoard.tiles(0)(2).ttype)
    newBoard.draw_down(-1,4,2)
    newBoard.draw_down(-1,4,3)
    newBoard.printBoard()
  }
}

