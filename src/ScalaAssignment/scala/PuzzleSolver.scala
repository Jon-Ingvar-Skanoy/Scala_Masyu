
import PuzzleReaderWriter.{closing, getNumPizzles, getPuzzle, initRW}
object Main {
  def main(args: Array[String]): Unit = {
    initRW("src/ScalaAssignment/scala/puzzle_unsolved.txt","src/ScalaAssignment/scala/puzzle_solved.txt")
    getNumPizzles()
    val newBoard: Puzzle = getPuzzle(4)



    //    println(newBoard.til  es(0)(2).ttype)

    newBoard.borders()
    newBoard.set_Up()


    for(i<-0  until 100){
      newBoard.illegal_moves()
      newBoard.legal_moves()
    }

    newBoard.printBoard
    newBoard.print_ugly

    closing()
  }
}

