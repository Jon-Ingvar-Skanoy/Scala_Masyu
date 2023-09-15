
import PuzzleReaderWriter.{closing, getNumPizzles, getPuzzle, initRW, writeAnswer}
import scala.collection.mutable._



object PuzzleSolver {
  def main(args: Array[String]): Unit = {
    //val line1: String = args(0)
    //val line2: String = args(1)
     //initRW(line1, line2)
    initRW("src/ScalaAssignment/scala/puzzle_unsolved.txt", "src/ScalaAssignment/scala/puzzle_solved.txt")
    var newBoard: Puzzle =  Puzzle(0,0,Array.ofDim[Tile](0, 0))
    val puzzleCount: Int = getNumPizzles()
    for (i <- 0 until 1) {

      newBoard=  getPuzzle(i)


      val clones: Array[Array[Tile]] = newBoard.copyTiles()
      //    println(newBoard.til  es(0)(2).ttype)

      newBoard.borders()
      newBoard.set_Up()
      val solutionstring: String = newBoard.boardString

      for (i <- 0 until 100) {
        newBoard.illegal_moves()
        newBoard.legal_moves()
      }

      val sd:Puzzle= new Puzzle(newBoard.height,newBoard.height,clones)
      sd.print_ugly
      newBoard.print_ugly


      //print(newBoard.won())

      writeAnswer(board = newBoard)


    }
    closing()
  }



}
