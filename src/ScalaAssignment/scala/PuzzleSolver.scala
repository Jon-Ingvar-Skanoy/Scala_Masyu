
import PuzzleReaderWriter.{closing, getNumPizzles, getPuzzle, initRW, writeAnswer}





object PuzzleSolver {
  def main(args: Array[String]): Unit = {
    //val line1: String = args(0)
    //val line2: String = args(1)
    //initRW(line1, line2)
    initRW("src/ScalaAssignment/scala/puzzle_unsolved.txt", "src/ScalaAssignment/scala/puzzle_solved.txt")
    var newBoard: Puzzle =  Puzzle(0,0,Array.ofDim[Tile](0, 0))
    val puzzleCount: Int = getNumPizzles()
    for (i <- 0 until puzzleCount) {

      newBoard=  getPuzzle(i)




      //    println(newBoard.til  es(0)(2).ttype)

      newBoard = newBoard.borders()
      newBoard = newBoard.set_Up()


     newBoard = solve(newBoard,0)



      val solutionstring: String = newBoard.boardString






      //print(newBoard.won())

      writeAnswer(board = newBoard)


    }
    closing()
  }

  private def solve(puzzle: Puzzle, dept:Int): Puzzle = {





    for (i <- 0 until 20) {

      puzzle.illegal_moves()
      puzzle.legal_moves()

    }
    puzzle.illegal_moves()

    if (dept > 15) {

      return puzzle

    }

    if(puzzle.won()){
      print("WON32")
      return puzzle
    }


     if(puzzle.lost()) {

       return puzzle
     }

    var copy:Puzzle =  Puzzle(puzzle.width,puzzle.height,puzzle.copyTiles())
    var random_move:Array[Int] = Array(0,2,33)

    while(random_move(0) != - 1){
      copy =  Puzzle(puzzle.width,puzzle.height,puzzle.copyTiles())
      for (i <- 0 until 20) {

        copy.illegal_moves()
        copy.legal_moves()

      }

      copy.illegal_moves()

      random_move = copy.find_move()




      if (random_move(2) == 0) {

        copy.draw_left(1, random_move(1), random_move(0))
      }
      if (random_move(2) == 3) {

        copy.draw_right(1, random_move(1), random_move(0))
      }
      if (random_move(2) == 1) {


        copy.draw_down(1, random_move(1), random_move(0))
      }
      if (random_move(2) == 2) {

        copy.draw_up(1, random_move(1), random_move(0))
      }




      copy= solve(copy,dept+1)

      if (copy.won()) {
        println("won")
        return copy
      }
      if (copy.lost()){


        if (random_move(2) == 0) {
          puzzle.draw_right(-1, random_move(1), random_move(0))
        }
        if (random_move(2) == 3) {
          puzzle.draw_right(-1, random_move(1), random_move(0))
        }
        if (random_move(2) == 1) {
          puzzle.draw_down(-1, random_move(1), random_move(0))
        }
        if (random_move(2) == 2) {
          puzzle.draw_up(-1, random_move(1), random_move(0))
        }


      }


    }


     puzzle.illegalize()


  }



}

