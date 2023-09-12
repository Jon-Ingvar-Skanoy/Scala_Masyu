

object Main {
  def main(args: Array[String]): Unit = {
    val newBoard: Board = Board("src/main/scala/puzzle_unsolved.txt", 0);
    newBoard.input()
    //    println(newBoard.tiles(0)(2).ttype)

    newBoard.borders()
    newBoard.set_Up()

    for(i<-0  until 100 ){
      newBoard.illegal_moves()
      newBoard.legal_moves()
    }

    newBoard.printBoard()
    newBoard.print_ugly()
  }
}

