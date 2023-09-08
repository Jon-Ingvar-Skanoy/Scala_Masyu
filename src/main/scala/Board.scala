import scala.io.Source._

case class Board(fn: String, nr: Int) {
  private val  filename = fn
  private val width: Int = set_width()
  private val height: Int = set_height()

  private val iterator: Array[Int] = Array.ofDim[Int](3)

  private val tiles: Array[Array[Tile]] = Array.ofDim[Tile](height, width)

  def input(): Unit = {
    for (line <- fromFile(filename).getLines()) {

      if (line.startsWith("size")) iterator(0) = iterator(0)+1
      else if (iterator(0) == nr+1){
        for(i<- line if ' '!=i){



            tiles(iterator(1))(iterator(2)) =  Tile(i)

            iterator(2) = iterator(2)+1



        }

        iterator(2) = 0
        iterator(1) = iterator(1)+1
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
      for(i <- fromFile(filename).getLines().toList if i.startsWith("size") ) yield i
    //startLines.foreach(x:String

    startLines(nr).split(" ")(1).split("x")(0).toInt
    //return startLines.split(" ")(1).split("x")(1).toInt



  }
 // def print(): Unit = {

 // }

  def printBoard(): Any = {
    for(i <- 0 until height){
      for (j <- 0 until width) {
        if(tiles(i)(j).ttype == TileType.Black){
          print('┼')
        }
        else if(tiles(i)(j).ttype == TileType.White){
          if(tiles(i)(j).left()){
            print('╨')
          }
          else if(tiles(i)(j).down()){
            print('╡')
          }
          else{
            print(' ')
          }
        }
        else if(tiles(i)(j).left()){
          if(tiles(i)(j).down()){
            // print left + down
            print('┐')
          }
          if(tiles(i)(j).up()){
            //print left + up
            print('┘')
          }
          if(tiles(i)(j).right()){
            //print left + right
            print('-')
          }
          else{
            print(' ')
          }
        }
        else if(tiles(i)(j).up()){
          if(tiles(i)(j).down()){
            //print up+down
            print('│')
          }
          if(tiles(i)(j).right()){
            //print down + right
            print('┌')
          }
          else{
            print(' ')
          }
        }
        else if(tiles(i)(j).paths(2)==Line.Placed){
          // print up+right
          print('└')
        }
        else{
          print(' ')
        }

      }
      println()
    }

  }

  def draw_down(legality: Int, x: Int, y: Int): Unit = {
    if(!tiles(y)(x).down() && !tiles(y)(x).crowded() && legality == 1){

      tiles(y)(x).paths(1)= Line.Placed
      tiles(y+1)(x).paths(2)=Line.Placed
    }
    if (tiles(y)(x).paths(1) != Line.Illegal && !tiles(y)(x).crowded() && legality == -1) {

      tiles(y)(x).paths(1) = Line.Illegal
      tiles(y + 1)(x).paths(2) = Line.Illegal
    }


  }

  def draw_Up(legality: Int, x: Int, y: Int) : Unit = {
    if (!tiles(y)(x).up() && !tiles(y)(x).crowded() && legality == 1) {

      tiles(y)(x).paths(2) = Line.Placed
      tiles(y - 1)(x).paths(1) = Line.Placed
    }
    if (tiles(y)(x).paths(2) != Line.Illegal && !tiles(y)(x).crowded() && legality == -1) {

      tiles(y)(x).paths(1) = Line.Illegal
      tiles(y - 1)(x).paths(2) = Line.Illegal
    }
  }

  def draw_left(legality: Int, x: Int, y: Int): Unit = {
    if (!tiles(y)(x).left() && !tiles(y)(x).crowded() && legality == 1) {

      tiles(y)(x).paths(0) = Line.Placed
      tiles(y - 1)(x).paths(3) = Line.Placed
    }
    if (tiles(y)(x).paths(0) != Line.Illegal && !tiles(y)(x).crowded() && legality == -1) {

      tiles(y)(x).paths(0) = Line.Illegal
      tiles(y)(x - 1).paths(3) = Line.Illegal
    }
  }

  def draw_Right(legality: Int, x: Int, y: Int):  Unit = {
    if (!tiles(y)(x).right() && !tiles(y)(x).crowded() && legality == 1) {

      tiles(y)(x).paths(3) = Line.Placed
      tiles(y - 1)(x).paths(0) = Line.Placed
    }
    if (tiles(y)(x).paths(3) != Line.Illegal && !tiles(y)(x).crowded() && legality == -1) {

      tiles(y)(x).paths(3) = Line.Illegal
      tiles(y)(x - 1).paths(0) = Line.Illegal
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
          print(" - ")
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
          print(" - ")
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


}

