import scala.annotation.tailrec

case class Puzzle(x:Int, y:Int, sol: Array[Array[Tile]]  ){

   val width: Int = x
   val height: Int = y

   val tiles: Array[Array[Tile]] = sol

  def copyTiles(): Array[Array[Tile]] = {
    // function to deepcopy the class
    for (row <- tiles) yield {
      for (item <- row) yield item.copyTile(0)
    }
  }
  def lost():Boolean= {
    // function to evaluate if the board is wrong
    val flatTiles = tiles.flatten
    flatTiles.foreach(tile => {
        if (tile.inn_ring() && tile.dead_end()) return true
      })


     false
  }

  def find_move():Array[Int] = {

    // function to find a legal move that can be made

    val flatTiles = tiles.flatten.sortBy {
      tile=>
        val prox1 = Math.abs((height/4)^2-(tile.height/2)^2)
        val prox2 = Math.abs((width/4)^2-(tile.width/2)^2)
        0.1*prox1+0.1*prox2
    }
    flatTiles.foreach(tile => {
              if (!tile.crowded()) {
                //if (tile.inn_ring() | !tile.isEmpty ) {
                  if (!tile.up() && !tile.upIllegal()) {
                    return Array[Int](tile.height, tile.width, 2)
                  }
                  if (tile.downMissing()) {
                    return Array[Int](tile.height, tile.width, 1)
                  }
                  if (tile.leftMissing()) {
                    return Array[Int](tile.height, tile.width, 0)
                  }
                  if (tile.rightMissing()) {
                    return Array[Int](tile.height, tile.width, 3)
                  //}
                }
              }
            })
     Array[Int] (-1,-1,-1)
  }

  def boardString: String = {
    // function that return a string of the board to be delivered in file
    val str: String = ""
    val strArray = for(row<-tiles) yield addCharToBoardString(str,0,row)
    strArray.mkString
  }
  private def addCharToBoardString(str: String, i: Int, flatTiles: Array[Tile]): String = {

    if (str.length == width-1) {
      // function that ands a char to the board string that mach the content of the tile
      if (flatTiles(i).isBlack) {
        str + "┼\n"
      }
      else if (flatTiles(i).isWhite) {
        if (flatTiles(i).left()) {
          str + "╨\n"
        }
        else if (flatTiles(i).down()) {
          str + "╡\n"
        }
        else {
          str + " \n"
        }
      }
      else if (flatTiles(i).left()) {
        if (flatTiles(i).down()) {
          // print left + down
          str + "┐\n"
        }
        else if (flatTiles(i).up()) {
          //print left + up
          str + "┘\n"
        }
        else if (flatTiles(i).right()) {
          //print left + right
          str + "─\n"
        }
        else {
          str + " \n"
        }
      }
      else if (flatTiles(i).up()) {
        if (flatTiles(i).down()) {
          //print up+down
          str + "│\n"
        }
        else if (flatTiles(i).right()) {
          //print up + right
          str + "└\n"
        }
        else {
          str + " \n"
        }
      }
      else if (flatTiles(i).right()) {
        // print down+right
        if (flatTiles(i).down()) {
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
        if (flatTiles(i).left()) {
          addCharToBoardString( str + "╨",i+1,flatTiles)
        }
        else if (flatTiles(i).down()) {
          addCharToBoardString(str + "╡",i+1,flatTiles)
        }
        else {
          addCharToBoardString(str + " ",i+1,flatTiles)
        }
      }
      else if (flatTiles(i).left()) {
        if (flatTiles(i).down()) {
          // print left + down
          addCharToBoardString(str + "┐",i+1,flatTiles)
        }
        else if (flatTiles(i).up()) {
          //print left + up
          addCharToBoardString(str + "┘",i+1,flatTiles)
        }
        else if (flatTiles(i).right()) {
          //print left + right
          addCharToBoardString(str + "─",i+1,flatTiles)
        }
        else {
          addCharToBoardString(str + " ",i+1,flatTiles)
        }
      }
      else if (flatTiles(i).up()) {
        if (flatTiles(i).down()) {
          //print up+down
          addCharToBoardString(str + "│",i+1,flatTiles)
        }
        else if (flatTiles(i).right()) {
          //print up + right
          addCharToBoardString(str + "└",i+1,flatTiles)
        }
        else {
          addCharToBoardString(str + " ",i+1,flatTiles)
        }
      }
      else if (flatTiles(i).right()) {
        // print down+right
        if (flatTiles(i).down()) {
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
  private def count_dots: Int = {
    // function who count the dots on the board
    var count = 0
    for (ii <- 0 until height) {
      for (j <- 0 until width) {
        if (!tiles(ii)(j).isEmpty) {
          count = count + 1
        }
      }
    }
    count
  }

   def draw_down(legality: Int, x: Int, y: Int): Unit = {
     // function that draws a line from a tile to the tile below
     if (tiles(y)(x).downMissing() && !tiles(y)(x).crowded&& !tiles(y+1)(x).crowded  && legality == 1) {
       tiles(y)(x).paths(1) = Line.Placed
       tiles(y + 1)(x).paths(2) = Line.Placed
     }
     if (!tiles(y)(x).downIllegal && legality == -1) {
       tiles(y)(x).paths(1) = Line.Illegal
       tiles(y + 1)(x).paths(2) = Line.Illegal
     }
   }

   def draw_up(legality: Int, x: Int, y: Int) : Unit = {
     // function that draws a line from a tile to the tile above
    if (tiles(y)(x).upMissing() && !tiles(y)(x).crowded && !tiles(y-1)(x).crowded&& legality == 1) {
      tiles(y)(x).paths(2) = Line.Placed
      tiles(y - 1)(x).paths(1) = Line.Placed
    }
    if (!tiles(y)(x).upIllegal && legality == -1) {
      tiles(y)(x).paths(2) = Line.Illegal
      tiles(y - 1)(x).paths(1) = Line.Illegal
    }
  }

   def draw_left(legality: Int, x: Int, y: Int): Unit = {
     // function that draws a line from a tile to the tile to the left
    if (tiles(y)(x).leftMissing() && !tiles(y)(x).crowded && !tiles(y)(x-1).crowded && legality == 1) {
      tiles(y)(x).paths(0) = Line.Placed
      tiles(y)(x - 1).paths(3) = Line.Placed
    }
    if (!tiles(y)(x).leftIllegal && legality == -1) {
      tiles(y)(x).paths(0) = Line.Illegal
      tiles(y)(x - 1).paths(3) = Line.Illegal
    }
  }

   def draw_right(legality: Int, x: Int, y: Int):  Unit = {
     // function that draws a line from a tile to the tile to the left
    if (tiles(y)(x).rightMissing() && !tiles(y)(x).crowded&& !tiles(y)(x+1).crowded  && legality == 1) {
      tiles(y)(x).paths(3) = Line.Placed
      tiles(y)(x +1 ).paths(0) = Line.Placed
    }
    if (!tiles(y)(x).rightIllegal && legality == -1) {
      tiles(y)(x).paths(3) = Line.Illegal
      tiles(y)(x + 1).paths(0) = Line.Illegal
    }
  }

  @tailrec
  private def border_Top(x:Int): Array[Array[Tile]] = {

    // function that creates the top border, tells the top tiles that it is illegal to move up
    if (x >= width) {
      return tiles
    }
    tiles(0)(x).paths(2)=Line.Illegal
     border_Top(x+1)
  }

  @tailrec
  private def border_Bottom(x:Int): Array[Array[Tile]] = {
    // function that creates the left border, tells the left tiles that it is illegal to move left
    if(x >= width){
      return tiles
    }
    tiles(height-1)(x).paths(1) = Line.Illegal
     border_Bottom(x+1)
    }

  @tailrec
  private def border_Left(x:Int): Array[Array[Tile]] = {
    // function that creates the left border, tells the left tiles that it is illegal to move left
    if (x >= height) {
      return tiles
    }


      tiles(x)(0).paths(0) = Line.Illegal
     border_Left(x+1)
  }

  @tailrec
  private def border_Right(x:Int): Array[Array[Tile]] = {
    // function that creates the right border, tells the bottom tiles that it is illegal to move right
    if (x >= height) {
      return tiles
    }
      tiles(x)(width-1).paths(3) = Line.Illegal
       border_Right(x+1)
  }

  def borders():Puzzle = {
    var newpuzzle = Puzzle(width, height, copyTiles())
    // function that creates the borders, calls function that does that
    newpuzzle.border_Left(0)
    newpuzzle.border_Right(0)
    newpuzzle.border_Top(0)
    newpuzzle.border_Bottom(0)
    Puzzle(newpuzzle.width, newpuzzle.height, newpuzzle.tiles)

  }


  def set_up_white_horizontal(x: Int, y: Int): Puzzle = {
    var newpuzzle = Puzzle(width, height, copyTiles())
    // checks if there is 3 white dots next to each other horizontaly
    if ((newpuzzle.tiles(y)(x).leftMissing() && newpuzzle.tiles(y)(x - 1).isWhite) && (newpuzzle.tiles(y)(x - 1).leftMissing() && newpuzzle.tiles(y)(x - 2).isWhite)) {
      newpuzzle.draw_left(-1, x, y)

    }
    Puzzle(newpuzzle.width, newpuzzle.height, newpuzzle.tiles)

  }
   def set_up_black_line(x: Int, y: Int):Puzzle = {
     var newpuzzle = Puzzle(width, height, copyTiles())
    // checks if there are the following pattern in any direction * _ o o  from this black dot, in that case defines the move it that direction illegal
    if(newpuzzle.tiles(y)(x).rightMissing() && newpuzzle.tiles(y)(x+1).rightMissing() &&  newpuzzle.tiles(y)(x+2).rightMissing() && (newpuzzle.tiles(y)(x+2).isWhite && newpuzzle.tiles(y)(x+3).isWhite )) draw_right(-1,x,y)
  if (newpuzzle.tiles(y)(x).leftMissing() && newpuzzle.tiles(y)(x - 1).leftMissing() && newpuzzle.tiles(y)(x - 2).leftMissing() && (newpuzzle.tiles(y)(x - 2).isWhite && newpuzzle.tiles(y)(x - 3).isWhite)) draw_left(-1, x, y)
    if (newpuzzle.tiles(y)(x).upMissing() && newpuzzle.tiles(y - 1)(x).upMissing() && newpuzzle.tiles(y - 2)(x).upMissing() && (newpuzzle.tiles(y - 2)(x).isWhite && newpuzzle.tiles(y - 3)(x).isWhite)) draw_up(-1, x, y)
    if (newpuzzle.tiles(y)(x).downMissing() && newpuzzle.tiles(y + 1)(x).downMissing() && newpuzzle.tiles(y + 2)(x).downMissing() && (newpuzzle.tiles(y + 2)(x).isWhite && newpuzzle.tiles(y + 3)(x).isWhite)) draw_down(-1, x, y)
     Puzzle(newpuzzle.width, newpuzzle.height, newpuzzle.tiles)
   }


   def set_up_black_diagonal_whites(x: Int, y: Int): Puzzle = {
    // check for
    //    *
    //  o _ o
    // if so defines the move in that direction illegal
    var newpuzzle = Puzzle(width, height, copyTiles())
    if(newpuzzle.tiles(y)(x).downMissing()&&newpuzzle.tiles(y)(x).leftMissing()&&newpuzzle.tiles(y)(x).rightMissing()){
      if(newpuzzle.tiles(y+1)( x-1).isWhite  & newpuzzle.tiles(y+1)(x+1).isWhite){
        draw_down(-1,x,y)
      }
    }
    if (newpuzzle.tiles(y)(x).upMissing() && newpuzzle.tiles(y)(x).leftMissing() && newpuzzle.tiles(y)(x).rightMissing()) {
      if (newpuzzle.tiles(y - 1)(x - 1).isWhite & newpuzzle.tiles(y - 1)(x + 1).isWhite) {
        newpuzzle.draw_up(-1, x, y)
      }
    }
    if (newpuzzle.tiles(y)(x).leftMissing() && (newpuzzle.tiles(y)(x).downMissing() && newpuzzle.tiles(y)(x).upMissing())) {
      if (newpuzzle.tiles(y + 1)(x - 1).isWhite & newpuzzle.tiles(y - 1)(x - 1).isWhite) {
        draw_left(-1, x, y)
      }
    }
    if (tiles(y)(x).rightMissing() && (tiles(y)(x).downMissing() && tiles(y)(x).upMissing())) {
      if (tiles(y + 1)(x + 1).isWhite & tiles(y - 1)(x + 1).isWhite) {
        newpuzzle.draw_right(-1, x, y)
      }
    }
     Puzzle(newpuzzle.width, newpuzzle.height, newpuzzle.tiles)
  }

  def print_ugly(): Unit = {
    // function that print the board for debugging, contains information that is not in the solution string


    println("Left")
    for (ii <- 0 until height) {
      for (j <- 0 until width) {

        if (tiles(ii)(j).leftMissing()) {
          print(" 0 ")
        }
        if (tiles(ii)(j).leftIllegal()) {
          print(" x ")
        }
        if (tiles(ii)(j).left()) {
          print(" ─ ")
        }
      }
      println
    }
    println("Down")
    for (ii <- 0 until height) {
      for (j <- 0 until width) {

        if (tiles(ii)(j).downMissing()) {
          print(" 0 ")
        }
        if (tiles(ii)(j).downIllegal()) {
          print(" x ")
        }
        if (tiles(ii)(j).down()) {
          print(" | ")
        }
      }
      println
    }

    println("Right")
    for (ii <- 0 until height) {
      for (j <- 0 until width) {

        if (tiles(ii)(j).rightMissing()) {
          print(" 0 ")
        }
        if (tiles(ii)(j).rightIllegal()) {
          print(" x ")
        }
        if (tiles(ii)(j).right()) {
          print(" ─ ")
        }
      }
      println()
    }
    println("UP")
    for (ii <- 0 until height) {
      for (j <- 0 until width) {

        if (tiles(ii)(j).upMissing()) {
          print(" 0 ")
        }
        if (tiles(ii)(j).upIllegal()) {
          print(" x ")
        }
        if (tiles(ii)(j).up()) {
          print(" | ")
        }
      }
      println()
    }
  }

  def won(): Boolean = {
    // checks if the board is completed
    val flatTiles = tiles.flatten
    flatTiles.foreach(tile => {

        if(tile.isEmpty){



       //   if (tiles(ii)(j).down() && circle(j, ii+1, j, ii, count_dots, 2) == 1) {
         //   println("w,d")
        //    return true
         // }
       //   if (tiles(ii)(j).up() && circle(j, ii+1, j, ii , count_dots, 2) == 1)  {
         //   println("w,u")
          //  return true
         // }

         // if (tiles(ii)(j).left() && circle(j, ii, j+1 , ii, count_dots, 0) == 1) {
           // println("w,l")
            //return true
          //}tile.width, tile.height
          if (tile.left() && circle(tile.width-1 , tile.height, tile.width, tile.height, count_dots, 3) == 1) {
            println("w,r")
            return true
          }
          if (tile.right() && circle(tile.width + 1, tile.height, tile.width, tile.height, count_dots, 0) == 1) {
            println("w,r")
            return true
          }
        //  if (tiles(ii)(j).right() && circle(j+1, ii, j, ii, count_dots, 0) == 1) {
          //  println("w,r")
            //return true
          //}


        }
      })


    false
  }

  def illegal_black_dot(x: Int, y: Int): Puzzle = {
    var newpuzzle = Puzzle(width, height, copyTiles())



    // checks if any move is illegal for a black dot
    if (newpuzzle.tiles(y)(x).downMissing() && (newpuzzle.tiles(y + 1)(x).downIllegal() | (newpuzzle.tiles(y + 1)(x).left() | newpuzzle.tiles(y + 1)(x).right()))) newpuzzle.draw_down(-1, x, y)
    if (newpuzzle.tiles(y)(x).upMissing() && (newpuzzle.tiles(y - 1)(x).upIllegal() | newpuzzle.tiles(y - 1)(x).left() | newpuzzle.tiles(y - 1)(x).right())) newpuzzle.draw_up(-1, x, y)
    if (newpuzzle.tiles(y)(x).leftMissing() && (newpuzzle.tiles(y)(x - 1).leftIllegal() | (newpuzzle.tiles(y)(x - 1).up() | newpuzzle.tiles(y)(x - 1).down()))) newpuzzle.draw_left(-1, x, y)
    if (newpuzzle.tiles(y)(x).rightMissing() && (newpuzzle.tiles(y)(x + 1).rightIllegal() | newpuzzle.tiles(y)(x + 1).up() | newpuzzle.tiles(y)(x + 1).down())) newpuzzle.draw_right(-1, x, y)
    // check if circle is formed
    if (newpuzzle.tiles(y)(x).downMissing() && newpuzzle.circle(x, y + 2, x, y, newpuzzle.count_dots, 2) == -1) newpuzzle.draw_down(-1, x, y)
    if (newpuzzle.tiles(y)(x).upMissing() && newpuzzle.circle(x, y - 2, x, y, newpuzzle.count_dots, 1) == -1) newpuzzle.draw_up(-1, x, y)
    if (newpuzzle.tiles(y)(x).leftMissing() && newpuzzle.circle(x - 2, y, x, y, newpuzzle.count_dots, 3) == -1) newpuzzle.draw_left(-1, x, y)
    if (newpuzzle.tiles(y)(x).rightMissing() && newpuzzle.circle(x + 2, y, x, y, newpuzzle.count_dots, 0) == -1) newpuzzle.draw_right(-1, x, y)

    Puzzle(newpuzzle.width, newpuzzle.height, newpuzzle.tiles)
  }

  def legal_crowded(x: Int, y: Int): Puzzle = {
    var newpuzzle = Puzzle(width, height,tiles)
    // called when there are two illegal moves in the tile and one placed, this function setts the last move to placed


    if (newpuzzle.tiles(y)(x).leftMissing()) newpuzzle.draw_left(1, x, y)
    if (newpuzzle.tiles(y)(x).downMissing()) newpuzzle.draw_down(1, x, y)
    if (newpuzzle.tiles(y)(x).upMissing()) newpuzzle.draw_up(1, x, y)
    if (newpuzzle.tiles(y)(x).rightMissing()) newpuzzle.draw_right(1, x, y)

    Puzzle(newpuzzle.width, newpuzzle.height, newpuzzle.copyTiles())
  }

  def illegal_white_dots(x: Int, y: Int): Puzzle = {
    val newpuzzle = Puzzle(width, height, copyTiles())

    // checks if any move is illegal for a white dot
    if (newpuzzle.tiles(y)(x).down() && newpuzzle.tiles(y + 1)(x).down()) newpuzzle.draw_up(-1, x, y - 1)
    if (newpuzzle.tiles(y)(x).up() && newpuzzle.tiles(y - 1)(x).up()) newpuzzle.draw_down(-1, x, y + 1)
    if (newpuzzle.tiles(y)(x).right() && newpuzzle.tiles(y)(x + 1).right()) newpuzzle.draw_left(-1, x - 1, y)
    if (newpuzzle.tiles(y)(x).left() && newpuzzle.tiles(y)(x - 1).left()) newpuzzle.draw_right(-1, x + 1, y)
    if (newpuzzle.tiles(y)(x).left()) {
      newpuzzle.draw_up(-1, x, y)
      newpuzzle.draw_down(-1, x, y)

    }
    if (newpuzzle.tiles(y)(x).right()) {
      newpuzzle.draw_up(-1, x, y)
      newpuzzle.draw_down(-1, x, y)

    }
    if (newpuzzle.tiles(y)(x).up()) {
      newpuzzle.draw_left(-1, x, y)
      newpuzzle.draw_right(-1, x, y)

    }
    if (newpuzzle.tiles(y)(x).down()) {
      newpuzzle.draw_left(-1, x, y)
      newpuzzle.draw_right(-1, x, y)

    }
    if (newpuzzle.tiles(y)(x).leftIllegal()) newpuzzle.draw_right(-1, x, y)
    if (newpuzzle.tiles(y)(x).downIllegal()) newpuzzle.draw_up(-1, x, y)
    if (newpuzzle.tiles(y)(x).upIllegal()) newpuzzle.draw_down(-1, x, y)
    if (newpuzzle.tiles(y)(x).rightIllegal()) newpuzzle.draw_left(-1, x, y)
    if ((!newpuzzle.tiles(y)(x).leftIllegal && newpuzzle.tiles(y)(x - 1).left) && (!newpuzzle.tiles(y)(x).rightIllegal() && newpuzzle.tiles(y)(x + 1).right)) {
      newpuzzle.draw_left(-1, x, y)
      newpuzzle.draw_right(-1, x, y)
    }
    if ((!newpuzzle.tiles(y)(x).upIllegal && newpuzzle.tiles(y - 1)(x).up) && (!newpuzzle.tiles(y)(x).downIllegal && newpuzzle.tiles(y + 1)(x).down)) {
      newpuzzle.draw_up(-1, x, y)
      newpuzzle.draw_down(-1, x, y)
    }

    Puzzle(newpuzzle.width, newpuzzle.height, newpuzzle.tiles)
  }

  def illegal_crowded(x: Int, y: Int): Puzzle = {
    val newpuzzle = Puzzle(width, height, copyTiles())
    // called when a tile has two placed moves this function defines remaining moves to illegal.

    if (newpuzzle.tiles(y)(x).leftMissing()) newpuzzle.draw_left(-1, x, y)
    if (newpuzzle.tiles(y)(x).downMissing()) newpuzzle.draw_down(-1, x, y)
    if (newpuzzle.tiles(y)(x).upMissing()) newpuzzle.draw_up(-1, x, y)
    if (newpuzzle.tiles(y)(x).rightMissing()) newpuzzle.draw_right(-1, x, y)
    Puzzle(newpuzzle.width, newpuzzle.height, newpuzzle.tiles)
  }

   def legal_black(x: Int, y: Int): Puzzle = {
    // called for every black dot, checks if a move is illegal that makes an other move forced
    var newpuzzle = Puzzle(width, height, copyTiles())
    if (newpuzzle.tiles(y)(x).leftIllegal()) {
      newpuzzle.draw_right(1, x, y)
      newpuzzle.draw_right(1, x + 1, y)
    }
    if (newpuzzle.tiles(y)(x).rightIllegal()) {
      newpuzzle.draw_left(1, x, y)
      newpuzzle.draw_left(1, x - 1, y)
    }
    if (newpuzzle.tiles(y)(x).downIllegal()) {
      newpuzzle.draw_up(1, x, y)
      newpuzzle.draw_up(1, x, y - 1)
    }
    if (newpuzzle.tiles(y)(x).upIllegal()) {
      newpuzzle.draw_down(1, x, y)
      newpuzzle.draw_down(1, x, y + 1)
    }
     Puzzle(newpuzzle.width, newpuzzle.height, newpuzzle.tiles)

  }
   def circle(start_x: Int, start_y: Int, Current_x: Int, current_y: Int, Remaining_dots: Int, Current_direction: Int): Int={
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



        if (tiles(current_y)(Current_x).up() && 2 != Current_direction) return circle(start_x, start_y, Current_x, current_y-1, Remaining_dots-1, 1)
        if (tiles(current_y)(Current_x).down() && 1 != Current_direction) return circle(start_x, start_y, Current_x, current_y+1, Remaining_dots-1, 2)
        if (tiles(current_y)(Current_x).right() && 3 != Current_direction) return circle(start_x, start_y, Current_x+1, current_y, Remaining_dots-1, 0)
        if (tiles(current_y)(Current_x).left() && 0 != Current_direction) return circle(start_x , start_y, Current_x-1, current_y, Remaining_dots-1, 3)
      if (!tiles(current_y)(Current_x).crowded()) return 0

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

        if (tiles(current_y)(Current_x).up() && 2 != Current_direction) return circle(start_x, start_y , Current_x, current_y-1, Remaining_dots, 1)
        if (tiles(current_y)(Current_x).down() && 1 != Current_direction) return circle(start_x, start_y, Current_x, current_y +1, Remaining_dots, 2)
        if (tiles(current_y)(Current_x).right() && 3 != Current_direction) return circle(start_x, start_y, Current_x+1, current_y, Remaining_dots, 0)
        if (tiles(current_y)(Current_x).left() && 0 != Current_direction) return circle(start_x, start_y, Current_x-1, current_y, Remaining_dots, 3)
      if (!tiles(current_y)(Current_x).crowded()) return 0


    }
     0
  }

   def avoid_circle_one_move(x: Int, y: Int): Puzzle = {
     // called in tiles if any move can results in an mini circle if so sets this move to illegal
    var newpuzzle = Puzzle(width, height, copyTiles())
    if (newpuzzle.tiles(y)(x).downMissing() && newpuzzle.circle(x, y, x, y + 1, count_dots, 2) == -1) newpuzzle.draw_down(-1, x, y)
    if (newpuzzle.tiles(y)(x).upMissing() && newpuzzle.circle(x, y, x, y - 1, count_dots, 1) == -1) newpuzzle.draw_up(-1, x, y)
    if (newpuzzle.tiles(y)(x).leftMissing() && newpuzzle.circle(x, y, x - 1, y, count_dots, 3) == -1) newpuzzle.draw_left(-1, x, y)
    if (newpuzzle.tiles(y)(x).rightMissing() && newpuzzle.circle(x, y, x + 1, y, count_dots, 0) == -1) newpuzzle.draw_right(-1, x, y)

     Puzzle(newpuzzle.width, newpuzzle.height, newpuzzle.tiles)
  }

  def illegalize(): Puzzle = {
    // called in search when there are no moves possible
    // this function sett all moves to illegal to tel the lower function call that this path is illegal or the move possible due to depth
    val flatTiles = tiles.flatten
    flatTiles.foreach(tile => {
        for (h <- 0 until 4){
          if(tile.paths(h)==Line.Missing) tile.paths(h)= Line.Illegal
        }
      })

      Puzzle(width, height, tiles)
  }
}


