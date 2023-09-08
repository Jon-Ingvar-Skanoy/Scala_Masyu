import scala.io.Source._
import Array._
case class Board(fn: String, nr: Int) {
  val filename = fn
  val width: Int = set_width()
  val height: Int = set_height()
  println(height)
  val iterator: Array[Int] = Array.ofDim[Int](3)
  println(width)
  val tiles: Array[Array[Tile]] = Array.ofDim[Tile](width, height)

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


  def set_height(): Int = {

    val startLines: List[String] =
      for (i <- fromFile(filename).getLines().toList if i.startsWith("size")) yield i
    //startLines.foreach(x:String

    return startLines(nr).split(" ")(1).split("x")(0).toInt


    return 1
  }

  def set_width(): Int = {
    val startLines: List[String] =
      for(i <- fromFile(filename).getLines().toList if i.startsWith("size") ) yield i
    //startLines.foreach(x:String

    return startLines(nr).split(" ")(1).split("x")(1).toInt
    //return startLines.split(" ")(1).split("x")(1).toInt


  return  1
  }
 // def print(): Unit = {

 // }

  def printBoard(): Any = {
    for(i <- 0 until width){
      for (j <- 0 until height) {
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

  def draw_down(legality: Int, x: Int, y: Int) {
    if(!tiles(y)(x).down() && !tiles(y)(x).crowded() && legality == 1){

      tiles(y)(x).paths(1)= Line.Placed
      tiles(y+1)(x).paths(2)=Line.Placed
    }
    if (tiles(y)(x).paths(1) == Line.Illegal && !tiles(y)(x).crowded() && legality == -1) {

      tiles(y)(x).paths(1) = Line.Illegal
      tiles(y + 1)(x).paths(2) = Line.Illegal
    }


  }

  def draw_Up(legality: Int, x: Int, y: Int) {
    if (!tiles(y)(x).up() && !tiles(y)(x).crowded() && legality == 1) {

      tiles(y)(x).paths(2) = Line.Placed
      tiles(y - 1)(x).paths(1) = Line.Placed
    }
    if (tiles(y)(x).paths(2) == Line.Illegal && !tiles(y)(x).crowded() && legality == -1) {

      tiles(y)(x).paths(1) = Line.Illegal
      tiles(y - 1)(x).paths(2) = Line.Illegal
    }
  }

  def draw_Up(legality: Int, x: Int, y: Int) {
    if (!tiles(y)(x).up() && !tiles(y)(x).crowded() && legality == 1) {

      tiles(y)(x).paths(2) = Line.Placed
      tiles(y - 1)(x).paths(1) = Line.Placed
    }
    if (tiles(y)(x).paths(2) == Line.Illegal && !tiles(y)(x).crowded() && legality == -1) {

      tiles(y)(x).paths(1) = Line.Illegal
      tiles(y - 1)(x).paths(2) = Line.Illegal
    }
  }


}

