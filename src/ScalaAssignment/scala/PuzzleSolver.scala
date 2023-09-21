
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
    val puzzleCount: Int = getNumPuzzles
      val counter = 0 until puzzleCount
    counter.foreach(i=>{

      newBoard=  getPuzzle(i)
      newBoard = newBoard.borders
      newBoard = setUp(newBoard)
      withTimeLimit(Duration(15,"seconds")) {
        newBoard = solve(newBoard,0)
      }

      writeAnswer(board = newBoard)})
    closing
  }



  private def solve(puzzle: Puzzle, dept:Int): Puzzle = {
    var newpuzzle = Puzzle(puzzle.width, puzzle.height, puzzle.copyTiles())



    var oldPuzzle = Puzzle(newpuzzle.width,newpuzzle.height,newpuzzle.copyTiles())
    while(!compare(oldPuzzle,newpuzzle)) {
      oldPuzzle = Puzzle(newpuzzle.width,newpuzzle.height,newpuzzle.copyTiles())

    newpuzzle = illegalMoves(newpuzzle)
      newpuzzle = legalMoves(newpuzzle)

    }

    newpuzzle = illegalMoves(newpuzzle)

    if (dept > 15) {

      return newpuzzle

    }
    if(newpuzzle.won().head._1){
      print("WON32")
      return newpuzzle
    }


     if(newpuzzle.lost()) {

       return newpuzzle
     }

    var copy:Puzzle =  Puzzle(newpuzzle.width,newpuzzle.height,newpuzzle.copyTiles())
    var move:Array[Int] = Array(0,2,33)

    while(move(0) != - 1){
      copy =  Puzzle(puzzle.width,puzzle.height,puzzle.copyTiles)
      copy = illegalMoves(copy)
      copy = legalMoves(copy)
      while (!compare(oldPuzzle, copy)) {
        oldPuzzle = Puzzle(copy.width, copy.height, copy.copyTiles)
        copy = illegalMoves(copy)
        copy = legalMoves(copy)



      }


      move = copy.find_move()




      if (move(2) == 0) {

        copy.drawLeft(1, move(1), move(0))
      }
      if (move(2) == 3) {

        copy.drawRight(1, move(1), move(0))
      }
      if (move(2) == 1) {


        copy.drawDown(1, move(1), move(0))
      }
      if (move(2) == 2) {

        copy.drawUp(1, move(1), move(0))
      }




      copy= solve(copy,dept+1)
      val wonResults = copy.won()
      if (wonResults.head._1) {
        return copy.cleanUp(wonResults.head._2, wonResults.head._3, wonResults.head._4, wonResults.head._5, copy.countDots, wonResults.head._6)
        }
      if (copy.lost()){


        if (move(2) == 0) {
          puzzle.drawRight(-1, move(1), move(0))
        }
        if (move(2) == 3) {
          puzzle.drawRight(-1, move(1), move(0))
        }
        if (move(2) == 1) {
          puzzle.drawDown(-1, move(1), move(0))
        }
        if (move(2) == 2) {
          puzzle.drawUp(-1, move(1), move(0))
        }


      }


    }


    puzzle.illegalize()


  }

  def set_up_black(x: Int, y: Int, puzzle: Puzzle): Puzzle = {
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


  def set_Up(puzzle: Puzzle): Puzzle = {
    var newpuzzle = Puzzle(puzzle.width,puzzle.height,puzzle.copyTiles())
    // calls all set_up functions in the relevant tiles
    val flatTiles = newpuzzle.tiles.flatMap(tile => tile)
    flatTiles.foreach(tile => {
      if (tile.isWhite) {
        newpuzzle = setUpWhiteVertical(tile.width, tile.height,newpuzzle)
      }
      if (tile.isWhite) newpuzzle =newpuzzle.setUpWhiteHorizontal(tile.width, tile.height)
      if (tile.isBlack) {
        newpuzzle = setUpBlack(tile.width, tile.height, newpuzzle)
        newpuzzle = newpuzzle.setUpBlackWithDiagonalWhites(tile.width, tile.height)
        newpuzzle = newpuzzle.setUpBlackLine(tile.width, tile.height)
      }

    })
    Puzzle(newpuzzle.width, newpuzzle.height, newpuzzle.tiles)
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
  private def legalMoves(puzzle: Puzzle): Puzzle = {
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
      if (tile.isBlack) newpuzzle = newpuzzle.illegalBlackDot(tile.width, tile.height)
      if (tile.isWhite) newpuzzle = newpuzzle.illegalWhiteDot(tile.width, tile.height)
      if (tile.crowded | tile.deadEnd) newpuzzle = newpuzzle.illegalCrowded(tile.width, tile.height)
      if (!tile.crowded && tile.inRing) newpuzzle = newpuzzle.avoidCircleOneMove(tile.width, tile.height)
    })
    Puzzle(newpuzzle.width, newpuzzle.height, newpuzzle.tiles)

  }



}




