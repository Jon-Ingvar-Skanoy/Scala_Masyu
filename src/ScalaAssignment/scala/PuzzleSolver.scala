
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
      newBoard = set_Up(newBoard)


     newBoard = solve(newBoard,0)








      //print(newBoard.won())

      writeAnswer(board = newBoard)


    }
    closing()
  }

  private def solve(puzzle: Puzzle, dept:Int): Puzzle = {





    for (i <- 0 until 10) {

      puzzle.illegal_moves()
      puzzle.legal_moves()

    }
    puzzle.illegal_moves()

    if (dept > 20) {

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
    var move:Array[Int] = Array(0,2,33)

    while(move(0) != - 1){
      copy =  Puzzle(puzzle.width,puzzle.height,puzzle.copyTiles())
      for (i <- 0 until 20) {

        copy.illegal_moves()
        copy.legal_moves()

      }

      copy.illegal_moves()
      move = copy.find_move()




      if (move(2) == 0) {

        copy.draw_left(1, move(1), move(0))
      }
      if (move(2) == 3) {

        copy.draw_right(1, move(1), move(0))
      }
      if (move(2) == 1) {


        copy.draw_down(1, move(1), move(0))
      }
      if (move(2) == 2) {

        copy.draw_up(1, move(1), move(0))
      }




      copy= solve(copy,dept+1)

      if (copy.won()) {
        println("won")
        return copy
      }
      if (copy.lost()){


        if (move(2) == 0) {
          puzzle.draw_right(-1, move(1), move(0))
        }
        if (move(2) == 3) {
          puzzle.draw_right(-1, move(1), move(0))
        }
        if (move(2) == 1) {
          puzzle.draw_down(-1, move(1), move(0))
        }
        if (move(2) == 2) {
          puzzle.draw_up(-1, move(1), move(0))
        }


      }


    }


    puzzle.illegalize()


  }

  def set_Up(puzzle: Puzzle): Puzzle = {
    var newpuzzle = Puzzle(puzzle.width,puzzle.height,puzzle.tiles)
    // calls all set_up functions in the relevant tiles
    val flatTiles = newpuzzle.tiles.flatMap(tile => tile)
    flatTiles.foreach(tile => {
      if (tile.isWhite) {
        newpuzzle = set_up_white_vertical(tile.width, tile.height,newpuzzle)
      }
      if (tile.isWhite) newpuzzle.set_up_white_horizontal(tile.width, tile.height)
      if (tile.isBlack) {
        newpuzzle = set_up_black(tile.width, tile.height, puzzle)
        newpuzzle.set_up_black_diagonal_whites(tile.width, tile.height)
        newpuzzle.set_up_black_line(tile.width, tile.height)
      }

    })
    Puzzle(newpuzzle.width, newpuzzle.height, newpuzzle.tiles)
  }

  def set_up_black(x: Int, y: Int,puzzle: Puzzle): Puzzle = {
    // function that checks if the black dot in the given tile is next to an other black dot
    // if so defines it illegal to move between them.
    if (puzzle.tiles(y)(x).rightMissing() && puzzle.tiles(y)(x + 1).isBlack) {
      puzzle.draw_right(-1, x, y)
    }
    if (puzzle.tiles(y)(x).downMissing() && puzzle.tiles(y + 1)(x).isBlack) {
      puzzle.draw_down(-1, x, y)
    }
    Puzzle(puzzle.width, puzzle.height, puzzle.tiles)
  }

  def set_up_white_vertical(x: Int, y: Int, puzzle: Puzzle): Puzzle = {
    // checks if there is 3 white dots next to each other vertical
    if ((puzzle.tiles(y)(x).downMissing() && puzzle.tiles(y + 1)(x).isWhite) && (puzzle.tiles(y + 1)(x).downMissing() && puzzle.tiles(y + 2)(x).isWhite)) {
      puzzle.draw_down(-1, x, y)
    }
    Puzzle(puzzle.width, puzzle.height, puzzle.tiles)
  }



}

