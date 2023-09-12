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
        if (tiles(i)(j).ttype == TileType.Black) {
          print('┼')
        }
        else if (tiles(i)(j).ttype == TileType.White) {
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
    if (tiles(y)(x).paths(1) != Line.Illegal  && legality == -1) {

      tiles(y)(x).paths(1) = Line.Illegal
      tiles(y + 1)(x).paths(2) = Line.Illegal
    }


  }

  def draw_Up(legality: Int, x: Int, y: Int) : Unit = {
    if (!tiles(y)(x).up() && !tiles(y)(x).crowded() && legality == 1) {

      tiles(y)(x).paths(2) = Line.Placed
      tiles(y - 1)(x).paths(1) = Line.Placed
    }
    if (tiles(y)(x).paths(2) != Line.Illegal && legality == -1) {

      tiles(y)(x).paths(2) = Line.Illegal
      tiles(y - 1)(x).paths(1) = Line.Illegal
    }
  }

  def draw_left(legality: Int, x: Int, y: Int): Unit = {
    if (!tiles(y)(x).left() && !tiles(y)(x).crowded() && legality == 1) {

      tiles(y)(x).paths(0) = Line.Placed
      tiles(y)(x - 1).paths(3) = Line.Placed
    }
    if (tiles(y)(x).paths(0) != Line.Illegal  && legality == -1) {

      tiles(y)(x).paths(0) = Line.Illegal
      tiles(y)(x - 1).paths(3) = Line.Illegal
    }
  }

  def draw_Right(legality: Int, x: Int, y: Int):  Unit = {
    if (!tiles(y)(x).right() && !tiles(y)(x).crowded() && legality == 1) {

      tiles(y)(x).paths(3) = Line.Placed
      tiles(y)(x +1 ).paths(0) = Line.Placed
    }
    if (tiles(y)(x).paths(3) != Line.Illegal  && legality == -1) {

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

        if(tiles(ii)(j).paths(0) == Line.Missing){
          print(" 0 ")
        }
        if (tiles(ii)(j).paths(0) == Line.Illegal) {
          print(" x ")
        }
        if (tiles(ii)(j).paths(0) == Line.Placed) {
          print(" ─ ")
        }
      }
      println()
    }
    println("Down")
    for (ii <- 0 until height) {
      for (j <- 0 until width) {

        if (tiles(ii)(j).paths(1) == Line.Missing) {
          print(" 0 ")
        }
        if (tiles(ii)(j).paths(1) == Line.Illegal) {
          print(" x ")
        }
        if (tiles(ii)(j).paths(1) == Line.Placed) {
          print(" | ")
        }
      }
      println()
    }

    println("Right")
    for (ii <- 0 until height) {
      for (j <- 0 until width) {

        if (tiles(ii)(j).paths(3) == Line.Missing) {
          print(" 0 ")
        }
        if (tiles(ii)(j).paths(3) == Line.Illegal) {
          print(" x ")
        }
        if (tiles(ii)(j).paths(3) == Line.Placed) {
          print(" ─ ")
        }
      }
      println()
    }
    println("UP")
    for (ii <- 0 until height) {
      for (j <- 0 until width) {

        if (tiles(ii)(j).paths(2) == Line.Missing) {
          print(" 0 ")
        }
        if (tiles(ii)(j).paths(2) == Line.Illegal) {
          print(" x ")
        }
        if (tiles(ii)(j).paths(2) == Line.Placed) {
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
    if(tiles(y)(x).paths(3) == Line.Missing && tiles(y)(x+1).ttype == TileType.Black){
      draw_Right(-1,x,y)
    }
    if (tiles(y)(x).paths(1) == Line.Missing && tiles(y+1)(x).ttype == TileType.Black) {
      draw_down(-1, x, y)
    }
  }
  def set_Up():Unit ={
    for (ii <- 0 until height) {
      for (j <- 0 until width) {
        if(tiles(ii)(j).ttype == TileType.Black) set_up_black(j,ii)
      }
    }

  }
  def illegal_black_dot(x: Int, y: Int):Unit = {
    if(tiles(y)(x).paths(1) == Line.Missing && (tiles(y+1)(x).paths(1)==Line.Illegal | tiles(y+1)(x).paths(0)==Line.Placed | tiles(y+1)(x).paths(3)==Line.Placed)) draw_down(-1,x, y)
    if(tiles(y)(x).paths(2) == Line.Missing && (tiles(y-1)(x).paths(2)==Line.Illegal | tiles(y-1)(x).paths(0)==Line.Placed | tiles(y-1)(x).paths(3)==Line.Placed)) draw_Up(-1,x, y)
    if(tiles(y)(x).paths(0) == Line.Missing && (tiles(y)(x-1).paths(0)==Line.Illegal | tiles(y)(x-1).paths(2)==Line.Placed | tiles(y)(x-1).paths(1)==Line.Placed)) draw_left(-1,x, y)
    if(tiles(y)(x).paths(3) == Line.Missing && (tiles(y)(x+1).paths(3)==Line.Illegal | tiles(y)(x+1).paths(2)==Line.Placed | tiles(y)(x+1).paths(1)==Line.Placed)) draw_Right(-1,x, y)
    // check if circle is formed
    if(tiles(y)(x).paths(1) == Line.Missing  && circle(x,y+2,x,y,count_dots(),1)== -1) draw_down(-1,x, y)
    if(tiles(y)(x).paths(2) == Line.Missing  && circle(x,y-2,x,y,count_dots(),2)== -1) draw_down(-1,x, y)
    if (tiles(y)(x).paths(0) == Line.Missing && circle(x-2, y, x, y, count_dots(), 1) == -1) draw_down(-1, x, y)
    if (tiles(y)(x).paths(3) == Line.Missing && circle(x-2, y, x, y, count_dots(), 2) == -1) draw_down(-1, x, y)

  }
  def legal_crowded(x: Int, y: Int):Unit = {

    if (tiles(y)(x).paths(0) == Line.Missing) draw_left(1, x, y)
    if (tiles(y)(x).paths(1) == Line.Missing) draw_down(1, x, y)
    if (tiles(y)(x).paths(2) == Line.Missing) draw_Up(1, x, y)
    if (tiles(y)(x).paths(3) == Line.Missing) draw_Right(1, x, y)


  }
  def illegal_white_dots(x: Int, y: Int):Unit = {
    if(tiles(y)(x).paths(1) == Line.Placed && (tiles(y+1)(x).paths(1)==Line.Placed)) draw_Up(-1,x, y-1)
    if(tiles(y)(x).paths(2) == Line.Placed && (tiles(y-1)(x).paths(2)==Line.Placed)) draw_down(-1,x, y+1)
    if(tiles(y)(x).paths(3) == Line.Placed && (tiles(y)(x+1).paths(3)==Line.Placed)) draw_left(-1,x-1, y)
    if(tiles(y)(x).paths(0) == Line.Placed && (tiles(y)(x-1).paths(0)==Line.Placed)) draw_Right(-1,x+1, y)
    if(tiles(y)(x).paths(0) == Line.Placed) {
      draw_Up(-1,x, y)
      draw_down(-1,x,y)

    }
    if (tiles(y)(x).paths(3) == Line.Placed) {
      draw_Up(-1, x, y)
      draw_down(-1, x, y)

    }
    if (tiles(y)(x).paths(2) == Line.Placed) {
      draw_left(-1, x, y)
      draw_Right(-1, x, y)

    }
    if (tiles(y)(x).paths(1) == Line.Placed) {
      draw_left(-1, x, y)
      draw_Right(-1, x, y)

    }
    if (tiles(y)(x).paths(0) == Line.Illegal) draw_Right(-1, x, y)
    if (tiles(y)(x).paths(1) == Line.Illegal) draw_Up(-1, x, y)
    if (tiles(y)(x).paths(2) == Line.Illegal) draw_down(-1, x, y)
    if (tiles(y)(x).paths(3) == Line.Illegal) draw_left(-1, x, y)

  }
  def illegal_crowded(x: Int, y: Int):Unit = {

    if(tiles(y)(x).paths(0)==Line.Missing) draw_left(-1,x,y)
    if(tiles(y)(x).paths(1)==Line.Missing) draw_down(-1,x,y)
    if(tiles(y)(x).paths(2)==Line.Missing) draw_Up(-1,x,y)
    if(tiles(y)(x).paths(3)==Line.Missing) draw_Right(-1,x,y)
  }
  def legal_black(x: Int, y: Int):Unit = {
    if(tiles(y)(x).paths(0)==Line.Illegal){
      draw_Right(1,x,y)
      draw_Right(1,x+1,y)
    }
    if (tiles(y)(x).paths(3) == Line.Illegal) {
      draw_left(1, x, y)
      draw_left(1, x - 1, y)
    }
    if (tiles(y)(x).paths(1) == Line.Illegal) {
      draw_Up(1, x, y)
      draw_Up(1, x, y-1)
    }
    if (tiles(y)(x).paths(2) == Line.Illegal) {
      draw_down(1, x, y)
      draw_down(1, x, y + 1)
    }
  }
  def circle(start_x: Int, start_y: Int, Current_x: Int, current_y: Int, Remaining_dots: Int, Current_direction: Int): Int={
    if(tiles(current_y)(Current_x).ttype != TileType.Empty) {

      if (start_y == current_y & start_x == Current_x) {
        println(2)
        if (Remaining_dots == 1) {
          return 1
        }
        if (!tiles(current_y)(Current_x).crowded()) return 0
        if (tiles(current_y)(Current_x).paths(2) == Line.Placed & 2 != Current_direction) return circle(start_x, start_y - 1, Current_x, current_y, Remaining_dots-1, 2)
        if (tiles(current_y)(Current_x).paths(1) == Line.Placed & 1 != Current_direction) return circle(start_x, start_y + 1, Current_x, current_y, Remaining_dots-1, 1)
        if (tiles(current_y)(Current_x).paths(3) == Line.Placed & 3 != Current_direction) return circle(start_x + 1, start_y, Current_x, current_y, Remaining_dots-1, 3)
        if (tiles(current_y)(Current_x).paths(0) == Line.Placed & 0 != Current_direction) return circle(start_x + 1, start_y, Current_x, current_y, Remaining_dots-1, 0)

      }
    }
    else{

      if (start_y == current_y & start_x == Current_x) {
        println(2)
        if (Remaining_dots == 0) {
          return 1
        }
        if (!tiles(current_y)(Current_x).crowded()) return 0
        if (tiles(current_y)(Current_x).paths(2) == Line.Placed & 2 != Current_direction) return circle(start_x, start_y - 1, Current_x, current_y, Remaining_dots, 2)
        if (tiles(current_y)(Current_x).paths(1) == Line.Placed & 1 != Current_direction) return circle(start_x, start_y + 1, Current_x, current_y, Remaining_dots, 1)
        if (tiles(current_y)(Current_x).paths(3) == Line.Placed & 3 != Current_direction) return circle(start_x + 1, start_y, Current_x, current_y, Remaining_dots, 3)
        if (tiles(current_y)(Current_x).paths(0) == Line.Placed & 0 != Current_direction) return circle(start_x + 1, start_y, Current_x, current_y, Remaining_dots, 0)

      }
    }


    return 0
  }


  def illegal_moves():Boolean={
    for (ii <- 0 until height) {
      for (j <- 0 until width) {
        if(tiles(ii)(j).ttype==TileType.Black) illegal_black_dot(j,ii)
        if(tiles(ii)(j).ttype==TileType.White) illegal_white_dots(j,ii)
        if( tiles(ii)(j).crowded() | tiles(ii)(j).dead_end()) illegal_crowded(j,ii)

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

}

