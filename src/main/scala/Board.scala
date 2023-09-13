import scala.io.Source._

case class Board(fn: String, nr: Int) {
  private val filename = fn
  private val width: Int = set_width()
  private val height: Int = set_height()

  private val iterator: Array[Int] = Array.ofDim[Int](3)
  private val tiles: Array[Array[Tile]] = Array.ofDim[Tile](height, width)


  def input(): Unit = {
    for (line <- fromFile(filename).getLines()) {

      if (line.startsWith("size")) iterator(0) = iterator(0) + 1
      else if (iterator(0) == nr + 1) {
        for (i <- line if ' ' != i) {


          tiles(iterator(1))(iterator(2)) = Tile(i, iterator(1), iterator(2))

          iterator(2) = iterator(2) + 1


        }

        iterator(2) = 0
        iterator(1) = iterator(1) + 1
      }
    }
  }


  private def set_height(): Int = {

    val startLines: List[String] =
      for (i <- fromFile(filename).getLines().toList if i.startsWith("size")) yield i
    //startLines.foreach(x:String

    startLines(nr).split(" ")(1).split("x")(1).toInt


  }

  private def set_width(): Int = {
    val startLines: List[String] =
      for (i <- fromFile(filename).getLines().toList if i.startsWith("size")) yield i
    //startLines.foreach(x:String

    startLines(nr).split(" ")(1).split("x")(0).toInt
    //return startLines.split(" ")(1).split("x")(1).toInt


  }
  // def print(): Unit = {

  // }

  def printBoard(): Any = {
    println()
    for (i <- 0 until height) {
      for (j <- 0 until width) {
        if (tiles(i)(j).isBlack()) {
          print('┼')
        }
        else if (tiles(i)(j).isWhite()) {
          if (tiles(i)(j).left()) {
            print('╨')
          }
          else if (tiles(i)(j).down()) {
            print('╡')
          }
          else {
            print(' ')
          }
        }
        else if (tiles(i)(j).left()) {
          if (tiles(i)(j).down()) {
            // print left + down
            print('┐')
          }
          else if (tiles(i)(j).up()) {
            //print left + up
            print('┘')
          }
          else if (tiles(i)(j).right()) {
            //print left + right
            print('─')
          }
          else{
            print(' ')
          }
        }
        else if (tiles(i)(j).up()) {
          if (tiles(i)(j).down()) {
            //print up+down
            print('│')
          }
          else if (tiles(i)(j).right()) {
            //print up + right
            print('└')
          }
          else {
            print(' ')
          }
        }
        else if (tiles(i)(j).right()) {
          // print down+right
          print('┌')
        }
        else {
          print(' ')
        }

      }
      println()
    }

  }

  def count_dots(): Int = {
    var count = 0

    for (ii <- 0 until height) {
      for (j <- 0 until width) {
        if (tiles(ii)(j).ttype != TileType.Empty) {
          count = count + 1
        }
      }
    }

    return count
  }


  def draw_down(legality: Int, x: Int, y: Int): Unit = {
    if(!tiles(y)(x).down() && !tiles(y)(x).crowded() && legality == 1){

      tiles(y)(x).paths(1)= Line.Placed
      tiles(y+1)(x).paths(2)=Line.Placed
    }
    if (!tiles(y)(x).downIllegal()  && legality == -1) {

      tiles(y)(x).paths(1) = Line.Illegal
      tiles(y + 1)(x).paths(2) = Line.Illegal
    }


  }

  def draw_Up(legality: Int, x: Int, y: Int) : Unit = {
    if (!tiles(y)(x).up() && !tiles(y)(x).crowded() && legality == 1) {

      tiles(y)(x).paths(2) = Line.Placed
      tiles(y - 1)(x).paths(1) = Line.Placed
    }
    if (!tiles(y)(x).upIllegal() && legality == -1) {

      tiles(y)(x).paths(2) = Line.Illegal
      tiles(y - 1)(x).paths(1) = Line.Illegal
    }
  }

  def draw_left(legality: Int, x: Int, y: Int): Unit = {
    if (!tiles(y)(x).left() && !tiles(y)(x).crowded() && legality == 1) {

      tiles(y)(x).paths(0) = Line.Placed
      tiles(y)(x - 1).paths(3) = Line.Placed
    }
    if (!tiles(y)(x).leftIllegal() && legality == -1) {

      tiles(y)(x).paths(0) = Line.Illegal
      tiles(y)(x - 1).paths(3) = Line.Illegal
    }
  }

  def draw_Right(legality: Int, x: Int, y: Int):  Unit = {
    if (!tiles(y)(x).right() && !tiles(y)(x).crowded() && legality == 1) {

      tiles(y)(x).paths(3) = Line.Placed
      tiles(y)(x +1 ).paths(0) = Line.Placed
    }
    if (!tiles(y)(x).rightIllegal() && legality == -1) {

      tiles(y)(x).paths(3) = Line.Illegal
      tiles(y)(x + 1).paths(0) = Line.Illegal
    }
  }


  def print_ugly(): Unit = {
    for (ii <- 0 until height) {
      for (j <- 0 until width) {
        print(tiles(ii)(j).ttype + " ")
      }
      println()

    }

    println("Left")
    for (ii <- 0 until height) {
      for (j <- 0 until width) {

        if(tiles(ii)(j).leftMissing()){
          print(" 0 ")
        }
        if (tiles(ii)(j).leftIllegal()) {
          print(" x ")
        }
        if (tiles(ii)(j).left()) {
          print(" ─ ")
        }
      }
      println()
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
      println()
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


  private def border_Top(): Unit = {
    for (i <- 0 until width){
      tiles(0)(i).paths(2)=Line.Illegal
    }
  }

  private def border_Bottom(): Unit = {
    for (i <- 0 until width) {
      tiles(height-1)(i).paths(1) = Line.Illegal
    }
  }

  private def border_Left(): Unit = {
    for (i <- 0 until height) {
      tiles(i)(0).paths(0) = Line.Illegal
    }
  }

  private def border_Right(): Unit = {
    for (i <- 0 until height) {
      tiles(i)(width-1).paths(3) = Line.Illegal
    }
  }

  def borders():Unit = {
    border_Left()
    border_Right()
    border_Top()
    border_Bottom()
  }
  def set_up_black(x: Int, y: Int): Unit = {
    if(tiles(y)(x).rightMissing() && tiles(y)(x+1).isBlack()){
      draw_Right(-1,x,y)
    }
    if (tiles(y)(x).downMissing() && tiles(y+1)(x).isBlack()) {
      draw_down(-1, x, y)
    }
  }
  def set_up_white_vertical(x: Int, y: Int): Unit = {
    if((tiles(y)(x).downMissing() && tiles(y+1)(x).isWhite())&& ((tiles(y+1)(x).downMissing() && tiles(y+2)(x).isWhite()))){
      draw_down(-1,x,y)
      println(222)
    }
  }

  def set_up_white_horizontal(x: Int, y: Int): Unit = {
    if ((tiles(y)(x).leftMissing() && tiles(y)(x-1).isWhite()) && ((tiles(y)(x-1).leftMissing() && tiles(y)(x-2).isWhite()))) {
      draw_left(-1, x, y)
      println(222)
    }
  }
  def set_up_black_diagonal_whites(x: Int, y: Int): Unit = {
    if((tiles(y)(x).downMissing())&&((tiles(y)(x).leftMissing())&&(tiles(y)(x).rightMissing()))){
      if((tiles(y+1)( x-1).isWhite() ) & (tiles(y+1)(x+1).isWhite())){
        draw_down(-1,x,y)
      }
    }
    if ((tiles(y)(x).upMissing()) && ((tiles(y)(x).leftMissing()) && (tiles(y)(x).rightMissing()))) {
      if ((tiles(y - 1)(x - 1).isWhite()) & (tiles(y - 1)(x + 1).isWhite())) {
        draw_Up(-1, x, y)
      }
    }
    if ((tiles(y)(x).leftMissing()) && ((tiles(y)(x).downMissing()) && (tiles(y)(x).upMissing()))) {
      if ((tiles(y + 1)(x - 1).isWhite()) & (tiles(y - 1)(x - 1).isWhite())) {
        draw_Right(-1, x, y)
      }
    }
    if ((tiles(y)(x).rightMissing()) && ((tiles(y)(x).downMissing()) && (tiles(y)(x).upMissing()))) {
      if ((tiles(y + 1)(x + 1).isWhite()) & (tiles(y - 1)(x + 1).isWhite())) {
        draw_left(-1, x, y)
      }
    }
  }


  def set_Up():Unit ={
    for (ii <- 0 until height) {
      for (j <- 0 until width) {
        if(tiles(ii)(j).isBlack()) {
          set_up_black(j,ii)
          set_up_black_diagonal_whites(j,ii)
        }
        if(tiles(ii)(j).isWhite()) set_up_white_vertical(j,ii)
        if(tiles(ii)(j).isWhite()) set_up_white_horizontal(j,ii)

      }
    }

  }
  def illegal_black_dot(x: Int, y: Int):Unit = {
    if(tiles(y)(x).downMissing() && (tiles(y+1)(x).downIllegal() | tiles(y+1)(x).left() | tiles(y+1)(x).right())) draw_down(-1,x, y)
    if(tiles(y)(x).upMissing() && (tiles(y-1)(x).upIllegal() | tiles(y-1)(x).left() | tiles(y-1)(x).right())) draw_Up(-1,x, y)
    if(tiles(y)(x).leftMissing() && (tiles(y)(x-1).leftIllegal() | tiles(y)(x-1).up() | tiles(y)(x-1).down())) draw_left(-1,x, y)
    if(tiles(y)(x).rightMissing() && (tiles(y)(x+1).rightIllegal() | tiles(y)(x+1).up() | tiles(y)(x+1).down())) draw_Right(-1,x, y)
    // check if circle is formed
    if(tiles(y)(x).downMissing()  && circle(x,y+2,x,y,count_dots(),2)== -1) draw_down(-1,x, y)
    if(tiles(y)(x).upMissing()  && circle(x,y-2,x,y,count_dots(),1)== -1) draw_Up(-1,x, y)
    if (tiles(y)(x).leftMissing() && circle(x-2, y, x, y, count_dots(), 3) == -1) draw_left(-1, x, y)
    if (tiles(y)(x).rightMissing() && circle(x-2, y, x, y, count_dots(), 0) == -1) draw_Right(-1, x, y)

  }
  def legal_crowded(x: Int, y: Int):Unit = {

    if (tiles(y)(x).leftMissing()) draw_left(1, x, y)
    if (tiles(y)(x).downMissing()) draw_down(1, x, y)
    if (tiles(y)(x).upMissing()) draw_Up(1, x, y)
    if (tiles(y)(x).rightMissing()) draw_Right(1, x, y)


  }
  def illegal_white_dots(x: Int, y: Int):Unit = {
    if(tiles(y)(x).down() && (tiles(y+1)(x).down())) draw_Up(-1,x, y-1)
    if(tiles(y)(x).up() && (tiles(y-1)(x).up())) draw_down(-1,x, y+1)
    if(tiles(y)(x).right() && (tiles(y)(x+1).right())) draw_left(-1,x-1, y)
    if(tiles(y)(x).left() && (tiles(y)(x-1).left())) draw_Right(-1,x+1, y)
    if(tiles(y)(x).left()) {
      draw_Up(-1,x, y)
      draw_down(-1,x,y)

    }
    if (tiles(y)(x).right()) {
      draw_Up(-1, x, y)
      draw_down(-1, x, y)

    }
    if (tiles(y)(x).up()) {
      draw_left(-1, x, y)
      draw_Right(-1, x, y)

    }
    if (tiles(y)(x).down()) {
      draw_left(-1, x, y)
      draw_Right(-1, x, y)

    }
    if (tiles(y)(x).leftIllegal()) draw_Right(-1, x, y)
    if (tiles(y)(x).downIllegal()) draw_Up(-1, x, y)
    if (tiles(y)(x).upIllegal()) draw_down(-1, x, y)
    if (tiles(y)(x).rightIllegal()) draw_left(-1, x, y)

  }
  def illegal_crowded(x: Int, y: Int):Unit = {

    if(tiles(y)(x).leftMissing()) draw_left(-1,x,y)
    if(tiles(y)(x).downMissing()) draw_down(-1,x,y)
    if(tiles(y)(x).upMissing()) draw_Up(-1,x,y)
    if(tiles(y)(x).rightMissing()) draw_Right(-1,x,y)
  }
  def legal_black(x: Int, y: Int):Unit = {
    if(tiles(y)(x).leftIllegal()){
      draw_Right(1,x,y)
      draw_Right(1,x+1,y)
    }
    if (tiles(y)(x).rightIllegal()) {
      draw_left(1, x, y)
      draw_left(1, x - 1, y)
    }
    if (tiles(y)(x).downIllegal()) {
      draw_Up(1, x, y)
      draw_Up(1, x, y-1)
    }
    if (tiles(y)(x).upIllegal()) {
      draw_down(1, x, y)
      draw_down(1, x, y + 1)
    }
  }
  def circle(start_x: Int, start_y: Int, Current_x: Int, current_y: Int, Remaining_dots: Int, Current_direction: Int): Int={
    if(tiles(current_y)(Current_x).ttype != TileType.Empty) {

      if (start_y == current_y && start_x == Current_x) {
        println(2)
        if (Remaining_dots ==0) {
          return 1
        }
        else{
          println(Remaining_dots, current_y,Current_x,start_y,start_x)
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
        println(2)
        if (Remaining_dots == 0) {
          return 1
        }
        else{
          println(Remaining_dots, current_y, Current_x, start_x, start_y)

          return -1
        }
      }

        if (tiles(current_y)(Current_x).up() && 2 != Current_direction) return circle(start_x, start_y , Current_x, current_y-1, Remaining_dots, 1)
        if (tiles(current_y)(Current_x).down() && 1 != Current_direction) return circle(start_x, start_y, Current_x, current_y +1, Remaining_dots, 2)
        if (tiles(current_y)(Current_x).right() && 3 != Current_direction) return circle(start_x, start_y, Current_x+1, current_y, Remaining_dots, 0)
        if (tiles(current_y)(Current_x).left() && 0 != Current_direction) return circle(start_x, start_y, Current_x-1, current_y, Remaining_dots, 3)
      if (!tiles(current_y)(Current_x).crowded()) return 0


    }


    return 0
  }
  def avoid_circle_empthy(x: Int, y: Int): Unit={

    if (tiles(y)(x).downMissing() && circle(x, y , x, y+1, count_dots(), 2) == -1) draw_down(-1, x, y)
    if (tiles(y)(x).upMissing() && circle(x, y, x, y-1, count_dots(), 1) == -1) draw_Up(-1, x, y)
    if (tiles(y)(x).leftMissing() && circle(x, y, x-1, y, count_dots(), 3) == -1) draw_left(-1, x, y)
    if (tiles(y)(x).rightMissing() && circle(x , y, x+1, y, count_dots(), 0) == -1) draw_Right(-1, x, y)
  }


  def illegal_moves():Boolean={
    for (ii <- 0 until height) {
      for (j <- 0 until width) {
        if(tiles(ii)(j).ttype==TileType.Black) illegal_black_dot(j,ii)
        if(tiles(ii)(j).ttype==TileType.White) illegal_white_dots(j,ii)
        if( tiles(ii)(j).crowded() | tiles(ii)(j).dead_end()) illegal_crowded(j,ii)
        if(!tiles(ii)(j).crowded() && tiles(ii)(j).inn_ring()) avoid_circle_empthy(j,ii)

      }

      }
     true
  }
  def legal_moves():Unit= {
    for (ii <- 0 until height) {
      for (j <- 0 until width) {
        if(tiles(ii)(j).ttype==TileType.Black) legal_black(j,ii)
        if(tiles(ii)(j).Illegal_crowded() && tiles(ii)(j).inn_ring()) legal_crowded(j,ii)

      }
    }
  }

  def createAlteredBoard(board: Board, arr:Array[Array[Array[Boolean]]]): Board = {
    val newBoard = board
    for(i<-0 until height){
      for (j<-0 until width){
        for (d<-0 until 4){
          if(arr(i)(j)(d) == true ){
            if(newBoard.tiles(i)(j).paths(d) != Line.Illegal && !newBoard.tiles(i)(j).crowded()){
              newBoard.tiles(i)(j).paths(d) = Line.Placed
            }
          }
        }
      }
    }
    return newBoard
  }

  def createPathCombination(combination: Int): Array[Boolean] = {
    if(combination==0){
      return Array[Boolean](false,false,false,false)
   }
    else if(combination==1){
      return Array[Boolean](true,false,false,false)
    }
    else if(combination==2){
      return Array[Boolean](false,true,false,false)
    }
    else if(combination==3){
      return Array[Boolean](false,false,true,false)
    }
    else if (combination == 4) {
      return Array[Boolean](false, false, false, true)
    }
    else if (combination == 5) {
      return Array[Boolean](true, true, false, false)
    }
    else if (combination == 6) {
      return Array[Boolean](true, false, true, false)
    }
    else if (combination == 7) {
      return Array[Boolean](true, false, false, true)
    }
    else if (combination == 8) {
      return Array[Boolean](false, true, true, false)
    }
    else if (combination == 9) {
      return Array[Boolean](false, true, false, true)
    }
    else if (combination == 10) {
      return Array[Boolean](false, false, true, true)
    }
    else{
      return Array[Boolean](true, true, true, true)
    }
  }
}


