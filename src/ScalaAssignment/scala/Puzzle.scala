import scala.annotation.tailrec

case class Puzzle(x:Int, y:Int, sol: Array[Array[Tile]]  ){

   val width: Int = x
   val height: Int = y

   val tiles: Array[Array[Tile]] = sol

  def copyTiles: Array[Array[Tile]] = {
    // function to deepcopy the class
    for (row <- tiles) yield {
      for (item <- row) yield item.copyTile(0)
    }
  }
  def lost:Boolean= {
    // function to evaluate if the board is wrong
    val flatTiles = tiles.flatten
    flatTiles.foreach(tile => {
        if (tile.inRing && tile.deadEnd) return true
      })


     false
  }

  def findMove:Array[Int] = {

    // function to find a legal move that can be made

    //Sort tiles by proximity to middle square and some fancy math.
    val flatTiles = tiles.flatten.sortBy {
      tile=>
        val prox1 = Math.abs((height/2)^2-(tile.height)^2)
        val prox2 = Math.abs((width/16)^4-(tile.width/8)^4)
        prox1^2-prox2
    }
    flatTiles.foreach(tile => {
              if (!tile.crowded) {
                  if (!tile.up && !tile.upIllegal) {
                    return Array[Int](tile.height, tile.width, 2)
                  }
                  if (tile.downMissing) {
                    return Array[Int](tile.height, tile.width, 1)
                  }
                  if (tile.leftMissing) {
                    return Array[Int](tile.height, tile.width, 0)
                  }
                  if (tile.rightMissing) {
                    return Array[Int](tile.height, tile.width, 3)

                }
              }
            })
     Array[Int] (-1,-1,-1)
  }

  def boardString: String = {
    // function that returns a string of the board to be delivered in file
    val str: String = ""
    val strArray = for(row<-tiles) yield addCharToBoardString(str,0,row)
    strArray.mkString
  }
  @tailrec
  private def addCharToBoardString(str: String, i: Int, flatTiles: Array[Tile]): String = {

    if (str.length == width-1) {
      // function that recursively adds a char to the board string that mach the content of the tile
      if (flatTiles(i).isBlack) {
        str + "┼\n"
      }
      else if (flatTiles(i).isWhite) {
        if (flatTiles(i).left) {
          str + "╨\n"
        }
        else if (flatTiles(i).down) {
          str + "╡\n"
        }
        else {
          str + " \n"
        }
      }
      else if (flatTiles(i).left) {
        if (flatTiles(i).down) {
          // print left + down
          str + "┐\n"
        }
        else if (flatTiles(i).up) {
          //print left + up
          str + "┘\n"
        }
        else if (flatTiles(i).right) {
          //print left + right
          str + "─\n"
        }
        else {
          str + " \n"
        }
      }
      else if (flatTiles(i).up) {
        if (flatTiles(i).down) {
          //print up+down
          str + "│\n"
        }
        else if (flatTiles(i).right) {
          //print up + right
          str + "└\n"
        }
        else {
          str + " \n"
        }
      }
      else if (flatTiles(i).right) {
        // print down+right
        if (flatTiles(i).down) {
          str + "┌\n"
        }
        else {
          str + " \n"
        }
      }
      else {
        str + " \n"

      }
    }

    else{

      if (flatTiles(i).isBlack) {
        addCharToBoardString(str + "┼",i+1,flatTiles)
      }
      else if (flatTiles(i).isWhite) {
        if (flatTiles(i).left) {
          addCharToBoardString( str + "╨",i+1,flatTiles)
        }
        else if (flatTiles(i).down) {
          addCharToBoardString(str + "╡",i+1,flatTiles)
        }
        else {
          addCharToBoardString(str + " ",i+1,flatTiles)
        }
      }
      else if (flatTiles(i).left) {
        if (flatTiles(i).down) {
          // print left + down
          addCharToBoardString(str + "┐",i+1,flatTiles)
        }
        else if (flatTiles(i).up) {
          //print left + up
          addCharToBoardString(str + "┘",i+1,flatTiles)
        }
        else if (flatTiles(i).right) {
          //print left + right
          addCharToBoardString(str + "─",i+1,flatTiles)
        }
        else {
          addCharToBoardString(str + " ",i+1,flatTiles)
        }
      }
      else if (flatTiles(i).up) {
        if (flatTiles(i).down) {
          //print up+down
          addCharToBoardString(str + "│",i+1,flatTiles)
        }
        else if (flatTiles(i).right) {
          //print up + right
          addCharToBoardString(str + "└",i+1,flatTiles)
        }
        else {
          addCharToBoardString(str + " ",i+1,flatTiles)
        }
      }
      else if (flatTiles(i).right) {
        // print down+right
        if (flatTiles(i).down) {
          addCharToBoardString(str + "┌",i+1,flatTiles)
        }
        else {
          addCharToBoardString(str + " ",i+1,flatTiles)
        }
      }
      else {
        addCharToBoardString( str + " ",i+1,flatTiles)

      }
    }
  }
  def countDots: Int = {
    // function that counts the black and white dots on the board
    val flatTiles = tiles.flatten
    val flatBWTiles = for(tile<-flatTiles if !tile.isEmpty) yield tile
    flatBWTiles.length
  }

   def drawDown(legality: Int, x: Int, y: Int): Unit = {
     // function that draws a line from a tile to the tile below
     if (tiles(y)(x).downMissing && !tiles(y)(x).crowded&& !tiles(y+1)(x).crowded  && legality == 1) {
       tiles(y)(x).paths(1) = Line.Placed
       tiles(y + 1)(x).paths(2) = Line.Placed
     }
     if (!tiles(y)(x).downIllegal && legality == -1) {
       tiles(y)(x).paths(1) = Line.Illegal
       tiles(y + 1)(x).paths(2) = Line.Illegal
     }
   }

   def drawUp(legality: Int, x: Int, y: Int) : Unit = {
     // function that draws a line from a tile to the tile above
    if (tiles(y)(x).upMissing && !tiles(y)(x).crowded && !tiles(y-1)(x).crowded&& legality == 1) {
      tiles(y)(x).paths(2) = Line.Placed
      tiles(y - 1)(x).paths(1) = Line.Placed
    }
    if (!tiles(y)(x).upIllegal && legality == -1) {
      tiles(y)(x).paths(2) = Line.Illegal
      tiles(y - 1)(x).paths(1) = Line.Illegal
    }
  }

   def drawLeft(legality: Int, x: Int, y: Int): Unit = {
     // function that draws a line from a tile to the tile to the left
    if (tiles(y)(x).leftMissing && !tiles(y)(x).crowded && !tiles(y)(x-1).crowded && legality == 1) {
      tiles(y)(x).paths(0) = Line.Placed
      tiles(y)(x - 1).paths(3) = Line.Placed
    }
    if (!tiles(y)(x).leftIllegal && legality == -1) {
      tiles(y)(x).paths(0) = Line.Illegal
      tiles(y)(x - 1).paths(3) = Line.Illegal
    }
  }

   def drawRight(legality: Int, x: Int, y: Int):  Unit = {
     // function that draws a line from a tile to the tile to the left
    if (tiles(y)(x).rightMissing && !tiles(y)(x).crowded&& !tiles(y)(x+1).crowded  && legality == 1) {
      tiles(y)(x).paths(3) = Line.Placed
      tiles(y)(x +1 ).paths(0) = Line.Placed
    }
    if (!tiles(y)(x).rightIllegal && legality == -1) {
      tiles(y)(x).paths(3) = Line.Illegal
      tiles(y)(x + 1).paths(0) = Line.Illegal
    }
  }

  @tailrec
  private def borderTop(x:Int): Array[Array[Tile]] = {

    // function that creates the top border, tells the top tiles that it is illegal to move up
    if (x >= width) {
      return tiles
    }
    tiles(0)(x).paths(2)=Line.Illegal
     borderTop(x+1)
  }

  @tailrec
  private def borderBottom(x:Int): Array[Array[Tile]] = {
    // function that creates the left border, tells the left tiles that it is illegal to move left
    if(x >= width){
      return tiles
    }
    tiles(height-1)(x).paths(1) = Line.Illegal
     borderBottom(x+1)
    }

  @tailrec
  private def borderLeft(x:Int): Array[Array[Tile]] = {
    // function that creates the left border, tells the left tiles that it is illegal to move left
    if (x >= height) {
      return tiles
    }


      tiles(x)(0).paths(0) = Line.Illegal
     borderLeft(x+1)
  }

  @tailrec
  private def borderRight(x:Int): Array[Array[Tile]] = {
    // function that creates the right border, tells the bottom tiles that it is illegal to move right
    if (x >= height) {
      return tiles
    }
      tiles(x)(width-1).paths(3) = Line.Illegal
       borderRight(x+1)
  }

  def borders:Puzzle = {
    // function that creates the borders, calls function that does that
    val newPuzzle = Puzzle(width, height, copyTiles)

    newPuzzle.borderLeft(0)
    newPuzzle.borderRight(0)
    newPuzzle.borderTop(0)
    newPuzzle.borderBottom(0)
    newPuzzle

  }


  def setUpWhiteHorizontal(x: Int, y: Int): Puzzle = {
    // checks if there is 3 white dots next to each other horizontaly
    val newPuzzle = Puzzle(width, height, copyTiles)

    if ((newPuzzle.tiles(y)(x).leftMissing && newPuzzle.tiles(y)(x - 1).isWhite) && (newPuzzle.tiles(y)(x - 1).leftMissing && newPuzzle.tiles(y)(x - 2).isWhite)) {
      newPuzzle.drawLeft(-1, x, y)

    }
    newPuzzle

  }
   def setUpBlackLine(x: Int, y: Int):Puzzle = {
    // checks if there are the following pattern in any direction * _ o o  from this black dot, in that case defines the move it that direction illegal

     val newPuzzle = Puzzle(width, height, copyTiles)

    if(newPuzzle.tiles(y)(x).rightMissing && newPuzzle.tiles(y)(x+1).rightMissing &&  newPuzzle.tiles(y)(x+2).rightMissing && (newPuzzle.tiles(y)(x+2).isWhite && newPuzzle.tiles(y)(x+3).isWhite )) drawRight(-1,x,y)
  if (newPuzzle.tiles(y)(x).leftMissing && newPuzzle.tiles(y)(x - 1).leftMissing && newPuzzle.tiles(y)(x - 2).leftMissing && (newPuzzle.tiles(y)(x - 2).isWhite && newPuzzle.tiles(y)(x - 3).isWhite)) drawLeft(-1, x, y)
    if (newPuzzle.tiles(y)(x).upMissing && newPuzzle.tiles(y - 1)(x).upMissing && newPuzzle.tiles(y - 2)(x).upMissing && (newPuzzle.tiles(y - 2)(x).isWhite && newPuzzle.tiles(y - 3)(x).isWhite)) drawUp(-1, x, y)
    if (newPuzzle.tiles(y)(x).downMissing && newPuzzle.tiles(y + 1)(x).downMissing && newPuzzle.tiles(y + 2)(x).downMissing && (newPuzzle.tiles(y + 2)(x).isWhite && newPuzzle.tiles(y + 3)(x).isWhite)) drawDown(-1, x, y)
     newPuzzle
   }


   def setUpBlackWithDiagonalWhites(x: Int, y: Int): Puzzle = {
    // check for
    //    *
    //  o _ o
    // if so defines the move in that direction illegal
    val newPuzzle = Puzzle(width, height, copyTiles)
    if(newPuzzle.tiles(y)(x).downMissing&&newPuzzle.tiles(y)(x).leftMissing&&newPuzzle.tiles(y)(x).rightMissing){
      if(newPuzzle.tiles(y+1)( x-1).isWhite  & newPuzzle.tiles(y+1)(x+1).isWhite){
        drawDown(-1,x,y)
      }
    }
    if (newPuzzle.tiles(y)(x).upMissing && newPuzzle.tiles(y)(x).leftMissing && newPuzzle.tiles(y)(x).rightMissing) {
      if (newPuzzle.tiles(y - 1)(x - 1).isWhite & newPuzzle.tiles(y - 1)(x + 1).isWhite) {
        newPuzzle.drawUp(-1, x, y)
      }
    }
    if (newPuzzle.tiles(y)(x).leftMissing && (newPuzzle.tiles(y)(x).downMissing && newPuzzle.tiles(y)(x).upMissing)) {
      if (newPuzzle.tiles(y + 1)(x - 1).isWhite & newPuzzle.tiles(y - 1)(x - 1).isWhite) {
        drawLeft(-1, x, y)
      }
    }
    if (tiles(y)(x).rightMissing && (tiles(y)(x).downMissing && tiles(y)(x).upMissing)) {
      if (tiles(y + 1)(x + 1).isWhite & tiles(y - 1)(x + 1).isWhite) {
        newPuzzle.drawRight(-1, x, y)
      }
    }
     newPuzzle
  }

  def won: Set[(Boolean,Int,Int,Int,Int,Int)] = {
    // checks if the board is completed
    val flatTiles = tiles.flatten
    flatTiles.foreach(tile => {

        if(tile.isEmpty){
          if (tile.left && circle(tile.width-1 , tile.height, tile.width, tile.height, countDots, 3) == 1) {

            return Set((true,tile.width,tile.height,tile.width-1,tile.height,3))
          }
          if (tile.right && circle(tile.width + 1, tile.height, tile.width, tile.height, countDots, 0) == 1) {

            return Set((true,tile.width,tile.height,tile.width+1,tile.height,0))
          }
        }
      })


    Set((false,0,0,0,0,0))
  }

  def illegalBlackDot(x: Int, y: Int): Puzzle = {
    // checks if any move is illegal for a black dot
    val newPuzzle = Puzzle(width, height, copyTiles)

    if (newPuzzle.tiles(y)(x).downMissing && (newPuzzle.tiles(y + 1)(x).downIllegal | (newPuzzle.tiles(y + 1)(x).left | newPuzzle.tiles(y + 1)(x).right))) newPuzzle.drawDown(-1, x, y)
    if (newPuzzle.tiles(y)(x).upMissing && (newPuzzle.tiles(y - 1)(x).upIllegal | newPuzzle.tiles(y - 1)(x).left | newPuzzle.tiles(y - 1)(x).right)) newPuzzle.drawUp(-1, x, y)
    if (newPuzzle.tiles(y)(x).leftMissing && (newPuzzle.tiles(y)(x - 1).leftIllegal | (newPuzzle.tiles(y)(x - 1).up | newPuzzle.tiles(y)(x - 1).down))) newPuzzle.drawLeft(-1, x, y)
    if (newPuzzle.tiles(y)(x).rightMissing && (newPuzzle.tiles(y)(x + 1).rightIllegal | newPuzzle.tiles(y)(x + 1).up | newPuzzle.tiles(y)(x + 1).down)) newPuzzle.drawRight(-1, x, y)
    // check if circle is formed
    if (newPuzzle.tiles(y)(x).downMissing && newPuzzle.circle(x, y + 2, x, y, newPuzzle.countDots, 2) == -1) newPuzzle.drawDown(-1, x, y)
    if (newPuzzle.tiles(y)(x).upMissing && newPuzzle.circle(x, y - 2, x, y, newPuzzle.countDots, 1) == -1) newPuzzle.drawUp(-1, x, y)
    if (newPuzzle.tiles(y)(x).leftMissing && newPuzzle.circle(x - 2, y, x, y, newPuzzle.countDots, 3) == -1) newPuzzle.drawLeft(-1, x, y)
    if (newPuzzle.tiles(y)(x).rightMissing && newPuzzle.circle(x + 2, y, x, y, newPuzzle.countDots, 0) == -1) newPuzzle.drawRight(-1, x, y)

    newPuzzle
  }

  def legalCrowded(x: Int, y: Int): Puzzle = {
    // called when there are two illegal moves in the tile and one placed, this function setts the last move to placed
    val newPuzzle = Puzzle(width, height,tiles)

    if (newPuzzle.tiles(y)(x).leftMissing) newPuzzle.drawLeft(1, x, y)
    if (newPuzzle.tiles(y)(x).downMissing) newPuzzle.drawDown(1, x, y)
    if (newPuzzle.tiles(y)(x).upMissing) newPuzzle.drawUp(1, x, y)
    if (newPuzzle.tiles(y)(x).rightMissing) newPuzzle.drawRight(1, x, y)

    newPuzzle
  }

  def illegalWhiteDot(x: Int, y: Int): Puzzle = {
    // checks if any move is illegal for a white dot

    val newPuzzle = Puzzle(width, height, copyTiles)

    if (newPuzzle.tiles(y)(x).down && newPuzzle.tiles(y + 1)(x).down) newPuzzle.drawUp(-1, x, y - 1)
    if (newPuzzle.tiles(y)(x).up && newPuzzle.tiles(y - 1)(x).up) newPuzzle.drawDown(-1, x, y + 1)
    if (newPuzzle.tiles(y)(x).right && newPuzzle.tiles(y)(x + 1).right) newPuzzle.drawLeft(-1, x - 1, y)
    if (newPuzzle.tiles(y)(x).left && newPuzzle.tiles(y)(x - 1).left) newPuzzle.drawRight(-1, x + 1, y)
    if (newPuzzle.tiles(y)(x).left) {
      newPuzzle.drawUp(-1, x, y)
      newPuzzle.drawDown(-1, x, y)

    }
    if (newPuzzle.tiles(y)(x).right) {
      newPuzzle.drawUp(-1, x, y)
      newPuzzle.drawDown(-1, x, y)

    }
    if (newPuzzle.tiles(y)(x).up) {
      newPuzzle.drawLeft(-1, x, y)
      newPuzzle.drawRight(-1, x, y)

    }
    if (newPuzzle.tiles(y)(x).down) {
      newPuzzle.drawLeft(-1, x, y)
      newPuzzle.drawRight(-1, x, y)

    }
    if (newPuzzle.tiles(y)(x).leftIllegal) newPuzzle.drawRight(-1, x, y)
    if (newPuzzle.tiles(y)(x).downIllegal) newPuzzle.drawUp(-1, x, y)
    if (newPuzzle.tiles(y)(x).upIllegal) newPuzzle.drawDown(-1, x, y)
    if (newPuzzle.tiles(y)(x).rightIllegal) newPuzzle.drawLeft(-1, x, y)
    if ((!newPuzzle.tiles(y)(x).leftIllegal && newPuzzle.tiles(y)(x - 1).left) && (!newPuzzle.tiles(y)(x).rightIllegal && newPuzzle.tiles(y)(x + 1).right)) {
      newPuzzle.drawLeft(-1, x, y)
      newPuzzle.drawRight(-1, x, y)
    }
    if ((!newPuzzle.tiles(y)(x).upIllegal && newPuzzle.tiles(y - 1)(x).up) && (!newPuzzle.tiles(y)(x).downIllegal && newPuzzle.tiles(y + 1)(x).down)) {
      newPuzzle.drawUp(-1, x, y)
      newPuzzle.drawDown(-1, x, y)
    }

    newPuzzle
  }

  def illegalCrowded(x: Int, y: Int): Puzzle = {
    // called when a tile has two placed moves this function defines remaining moves to illegal.
    val newPuzzle = Puzzle(width, height, copyTiles)


    if (newPuzzle.tiles(y)(x).leftMissing) newPuzzle.drawLeft(-1, x, y)
    if (newPuzzle.tiles(y)(x).downMissing) newPuzzle.drawDown(-1, x, y)
    if (newPuzzle.tiles(y)(x).upMissing) newPuzzle.drawUp(-1, x, y)
    if (newPuzzle.tiles(y)(x).rightMissing) newPuzzle.drawRight(-1, x, y)
    newPuzzle
  }

   def legalBlack(x: Int, y: Int): Puzzle = {
    // called for every black dot, checks if a move is illegal that makes an other move forced
    val newPuzzle = Puzzle(width, height, copyTiles)
    if (newPuzzle.tiles(y)(x).leftIllegal) {
      newPuzzle.drawRight(1, x, y)
      newPuzzle.drawRight(1, x + 1, y)
    }
    if (newPuzzle.tiles(y)(x).rightIllegal) {
      newPuzzle.drawLeft(1, x, y)
      newPuzzle.drawLeft(1, x - 1, y)
    }
    if (newPuzzle.tiles(y)(x).downIllegal) {
      newPuzzle.drawUp(1, x, y)
      newPuzzle.drawUp(1, x, y - 1)
    }
    if (newPuzzle.tiles(y)(x).upIllegal) {
      newPuzzle.drawDown(1, x, y)
      newPuzzle.drawDown(1, x, y + 1)
    }
     newPuzzle

  }
   private def circle(start_x: Int, start_y: Int, Current_x: Int, current_y: Int, Remaining_dots: Int, Current_direction: Int): Int={
    // recursive function that determine of from start position one there is a line to the current position and if so if it passes trough every dot
    if(Remaining_dots <0) return -1
    if(!tiles(current_y)(Current_x).isEmpty) {

      if (start_y == current_y && start_x == Current_x) {
        if (Remaining_dots ==0) {
          return 1
        }
        else{

        return -1
        }
      }



        if (tiles(current_y)(Current_x).up && 2 != Current_direction) return circle(start_x, start_y, Current_x, current_y-1, Remaining_dots-1, 1)
        if (tiles(current_y)(Current_x).down && 1 != Current_direction) return circle(start_x, start_y, Current_x, current_y+1, Remaining_dots-1, 2)
        if (tiles(current_y)(Current_x).right && 3 != Current_direction) return circle(start_x, start_y, Current_x+1, current_y, Remaining_dots-1, 0)
        if (tiles(current_y)(Current_x).left && 0 != Current_direction) return circle(start_x , start_y, Current_x-1, current_y, Remaining_dots-1, 3)
      if (!tiles(current_y)(Current_x).crowded) return 0

    }
    else {
        if (start_y == current_y & start_x == Current_x) {

        if (Remaining_dots == 0) {
          return 1
        }
        else{


          return -1
        }
      }

        if (tiles(current_y)(Current_x).up && 2 != Current_direction) return circle(start_x, start_y , Current_x, current_y-1, Remaining_dots, 1)
        if (tiles(current_y)(Current_x).down && 1 != Current_direction) return circle(start_x, start_y, Current_x, current_y +1, Remaining_dots, 2)
        if (tiles(current_y)(Current_x).right && 3 != Current_direction) return circle(start_x, start_y, Current_x+1, current_y, Remaining_dots, 0)
        if (tiles(current_y)(Current_x).left && 0 != Current_direction) return circle(start_x, start_y, Current_x-1, current_y, Remaining_dots, 3)
      if (!tiles(current_y)(Current_x).crowded) return 0


    }
     0
  }

  private def circleList(start_x: Int, start_y: Int, Current_x: Int, current_y: Int, Remaining_dots: Int, Current_direction: Int, visited: List[Tuple2[Int, Int]]): List[Tuple2[Int, Int]] = {
    // recursive function that determine of from start position one there is a line to the current position and if so if it passes trough every dot

    val currentTileSet: Tuple2[Int, Int] = Tuple2(current_y, Current_x)
    if (!tiles(current_y)(Current_x).isEmpty) {

      if (start_y == current_y && start_x == Current_x) {

        return visited :+ currentTileSet


      }
      if (tiles(current_y)(Current_x).up && 2 != Current_direction) return circleList(start_x, start_y, Current_x, current_y - 1, Remaining_dots - 1, 1, visited :+ currentTileSet)
      if (tiles(current_y)(Current_x).down && 1 != Current_direction) return circleList(start_x, start_y, Current_x, current_y + 1, Remaining_dots - 1, 2, visited :+ currentTileSet)
      if (tiles(current_y)(Current_x).right && 3 != Current_direction) return circleList(start_x, start_y, Current_x + 1, current_y, Remaining_dots - 1, 0, visited :+ currentTileSet)
      if (tiles(current_y)(Current_x).left && 0 != Current_direction)  circleList(start_x, start_y, Current_x - 1, current_y, Remaining_dots - 1, 3, visited :+ currentTileSet)
      else {
        List.empty[Tuple2[Int, Int]]
      }
    }
    else {
      if (start_y == current_y & start_x == Current_x) {
        return visited :+ currentTileSet
      }

      if (tiles(current_y)(Current_x).up && 2 != Current_direction) return circleList(start_x, start_y, Current_x, current_y - 1, Remaining_dots, 1, visited :+ currentTileSet)
      if (tiles(current_y)(Current_x).down && 1 != Current_direction) return circleList(start_x, start_y, Current_x, current_y + 1, Remaining_dots, 2, visited :+ currentTileSet)
      if (tiles(current_y)(Current_x).right && 3 != Current_direction) return circleList(start_x, start_y, Current_x + 1, current_y, Remaining_dots, 0, visited :+ currentTileSet)
      if (tiles(current_y)(Current_x).left && 0 != Current_direction)  circleList(start_x, start_y, Current_x - 1, current_y, Remaining_dots, 3, visited :+ currentTileSet)

      else {

        List.empty[Tuple2[Int, Int]]
      }
    }
  }
def cleanUp(start_x: Int, start_y: Int, Current_x: Int, current_y: Int, Remaining_dots: Int, Current_direction: Int): Puzzle = {
  // function to clean away lines not in the completed circle
    val newPuzzle = Puzzle(width,height,copyTiles)
    val startList: List[(Int, Int)] = List.empty[(Int, Int)]
    val mainLoop = newPuzzle.circleList(start_x: Int, start_y: Int, Current_x: Int, current_y: Int, Remaining_dots: Int, Current_direction: Int, startList)
    val messyTiles: Array[Tile] = newPuzzle.tiles.flatMap(_.filter(tile => !mainLoop.contains(Tuple2(tile.height, tile.width))))

    messyTiles.foreach(tile=> newPuzzle.tiles(tile.height)(tile.width).paths(0) = Line.Illegal)
  messyTiles.foreach(tile=> newPuzzle.tiles(tile.height)(tile.width).paths(1) = Line.Illegal)
  messyTiles.foreach(tile=> newPuzzle.tiles(tile.height)(tile.width).paths(2) = Line.Illegal)
  messyTiles.foreach(tile=> newPuzzle.tiles(tile.height)(tile.width).paths(3) = Line.Illegal)
   newPuzzle
}
   def avoidCircleOneMove(x: Int, y: Int): Puzzle = {
     // called in tiles if any move can results in an mini circle if so sets this move to illegal
    val newPuzzle = Puzzle(width, height, copyTiles)
    if (newPuzzle.tiles(y)(x).downMissing && newPuzzle.circle(x, y, x, y + 1, countDots, 2) == -1) newPuzzle.drawDown(-1, x, y)
    if (newPuzzle.tiles(y)(x).upMissing && newPuzzle.circle(x, y, x, y - 1, countDots, 1) == -1) newPuzzle.drawUp(-1, x, y)
    if (newPuzzle.tiles(y)(x).leftMissing && newPuzzle.circle(x, y, x - 1, y, countDots, 3) == -1) newPuzzle.drawLeft(-1, x, y)
    if (newPuzzle.tiles(y)(x).rightMissing && newPuzzle.circle(x, y, x + 1, y, countDots, 0) == -1) newPuzzle.drawRight(-1, x, y)

     newPuzzle
  }

  def illegalize: Puzzle = {
    // called in search when there are no moves possible
    // this function sett all moves to illegal to tel the lower function call that this path is illegal or the move possible due to depth
    val newPuzzle = Puzzle(width,height,copyTiles)
    val flatTiles = newPuzzle.tiles.flatten
    flatTiles.foreach(tile => {
      if(tile.paths(0)==Line.Missing) tile.paths(0)= Line.Illegal
      if(tile.paths(1)==Line.Missing) tile.paths(1)= Line.Illegal
      if(tile.paths(2)==Line.Missing) tile.paths(2)= Line.Illegal
      if(tile.paths(3)==Line.Missing) tile.paths(3)= Line.Illegal})
      newPuzzle
  }
}


