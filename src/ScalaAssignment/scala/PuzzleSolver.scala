
import PuzzleReaderWriter.{closing, getNumPizzles, getPuzzle, initRW, writeAnswer}
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future, TimeoutException}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global


object PuzzleSolver {


  def withTimeLimit[T](timeout: Duration)(block: => T):Any = {
    val promise = Promise[T]()
    val future = Future {
      val result = block
      if (!promise.isCompleted) {
        promise.success(result)
      }
    }

    try {
      Some(Await.result(future, timeout))
    } catch {
      case ex: TimeoutException =>
        promise.tryFailure(new TimeoutException)
        None
    }
  }

    def main(args: Array[String]): Unit = {
    //val line1: String = args(0)
    //val line2: String = args(1)
    //initRW(line1, line2)
    initRW("src/ScalaAssignment/scala/puzzle_unsolved.txt", "src/ScalaAssignment/scala/puzzle_solved.txt")
    var newBoard: Puzzle =  Puzzle(0,0,Array.ofDim[Tile](0, 0))
    val puzzleCount: Int = getNumPizzles()
    for (i <- 0 until puzzleCount) {

      newBoard=  getPuzzle(i)






      newBoard = newBoard.borders()
      newBoard = set_Up(newBoard)
      val result = withTimeLimit(Duration(10,"seconds")) {
        newBoard = solve(newBoard,0)
      }










      //print(newBoard.won())

      writeAnswer(board = newBoard)


    }
    closing()
  }



  private def solve(puzzle: Puzzle, dept:Int): Puzzle = {
    var newpuzzle = Puzzle(puzzle.width, puzzle.height, puzzle.copyTiles())



    var oldPuzzle = Puzzle(newpuzzle.width,newpuzzle.height,newpuzzle.copyTiles())
    while(!compare(oldPuzzle,newpuzzle)) {
      oldPuzzle = Puzzle(newpuzzle.width,newpuzzle.height,newpuzzle.copyTiles())

    newpuzzle = illegal_moves(newpuzzle)
      newpuzzle = legal_moves(newpuzzle)

    }

    newpuzzle = illegal_moves(newpuzzle)

    if (dept > 15) {

      return newpuzzle

    }

    if(newpuzzle.won()){
      print("WON32")
      return newpuzzle
    }


     if(newpuzzle.lost()) {

       return newpuzzle
     }

    var copy:Puzzle =  Puzzle(newpuzzle.width,newpuzzle.height,newpuzzle.copyTiles())
    var move:Array[Int] = Array(0,2,33)

    while(move(0) != - 1){
      copy =  Puzzle(puzzle.width,puzzle.height,puzzle.copyTiles())
      copy = illegal_moves(copy)
      copy = legal_moves(copy)
      while (!compare(oldPuzzle, copy)) {
        oldPuzzle = Puzzle(copy.width, copy.height, copy.copyTiles())
        copy = illegal_moves(copy)
        copy = legal_moves(copy)



      }


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

 def compare(oldPuzzle:Puzzle,newPuzzle:Puzzle) = {
   val oldTilesFlat = oldPuzzle.tiles.flatMap(_.flatMap(_.paths))
   val newTilesFlat = newPuzzle.tiles.flatMap(_.flatMap(_.paths))
   (oldTilesFlat.sameElements(newTilesFlat))
 }
  def legal_moves(puzzle: Puzzle): Puzzle = {
    // calls functions in relevant tiles to determine if any moves are
    var newpuzzle = Puzzle(puzzle.width, puzzle.height, puzzle.copyTiles())

    val flatTiles = newpuzzle.tiles.flatten
    flatTiles.foreach(tile => {
      if (tile.isBlack) newpuzzle = newpuzzle.legal_black(tile.width, tile.height)
      if (tile.Illegal_crowded() && tile.inn_ring()) newpuzzle = newpuzzle.legal_crowded(tile.width, tile.height)
    })
    Puzzle(newpuzzle.width, newpuzzle.height, newpuzzle.tiles)
  }

  def illegal_moves(puzzle: Puzzle): Puzzle = {
    var newpuzzle = Puzzle(puzzle.width, puzzle.height, puzzle.copyTiles())
    //

    val flatTiles = newpuzzle.tiles.flatten
    flatTiles.foreach(tile => {
      if (tile.isBlack) newpuzzle.illegal_black_dot(tile.width, tile.height)
      if (tile.isWhite) newpuzzle.illegal_white_dots(tile.width, tile.height)
      if (tile.crowded() | tile.dead_end()) newpuzzle.illegal_crowded(tile.width, tile.height)
      if (!tile.crowded() && tile.inn_ring()) newpuzzle.avoid_circle_one_move(tile.width, tile.height)
    })
    Puzzle(newpuzzle.width, newpuzzle.height, newpuzzle.tiles)

  }



}




