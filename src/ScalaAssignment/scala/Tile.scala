
sealed abstract class Line(val label: String)
object Line {
  final case object Placed extends Line(label = "Placed")
  final case object Missing extends Line(label = "Missing")
  final case object Illegal extends Line(label = "Illegal")
};

sealed abstract class TileType(val label: String)
object TileType{
  final case object Black extends TileType(label = "Black")
  final case object White extends TileType(label = "White")
  final case object Empty extends TileType(label = "Empty")
}



case class Tile ( char: Char,  h: Int,  w: Int) {

  private def returnTileType(char: Char): TileType = {
    if (char == '*') {
     TileType.Black
    }
    else if (char == 'o') {
     TileType.White
    }
    else {
     TileType.Empty
    }
  }
  //index 0 = left, 1 = down, 2 = up, 3 = right
  private def initializeLine(): Array[Line] = {
    Array[Line](Line.Missing, Line.Missing, Line.Missing, Line.Missing)
  }

private  val ttype: TileType = returnTileType(char)
  val paths: Array[Line] = initializeLine()
  val width: Int = w
  val height: Int = h

  def copyTile(x:Int): Tile = {


    if (x >= 3) {
      val newTile: Tile = Tile(char, h, w)
      newTile.paths(x) = paths(x)
      return  newTile
    }
    val newTile: Tile =  copyTile(x+1)
    newTile.paths(x) = paths(x)


   newTile
  }
  def left(): Boolean = {
    if (paths(0) == Line.Placed) {
       return true
    }
    false
  }

  def down(): Boolean = {
    if (paths(1) == Line.Placed) {
      return true
    }
    false
  }
// hello
  def up(): Boolean = {
    if (paths(2) == Line.Placed) {
      return true
    }
    false
  }

  def right(): Boolean = {
    if (paths(3) == Line.Placed) {
      return true
    }
    false
  }

  def crowded(): Boolean = {
    val lineCount =
      for (i <- paths.toList if i == Line.Placed) yield i
    if (lineCount.length >= 2) {
      return true
    }
    false
  }

  def Illegal_crowded(): Boolean = {
    val lineCount =
      for (i <- paths.toList if i == Line.Illegal) yield i
    if (lineCount.length >= 2) {
      return true
    }
    false
  }

  def inn_ring(): Boolean = {
    val lineCount =
      for (i <- paths.toList if i == Line.Placed) yield i
    if (lineCount.nonEmpty | ttype != TileType.Empty) {
      return true
    }
    false



  }
  def dead_end(): Boolean = {
    val lineCount =
      for (i <- paths.toList if i == Line.Illegal) yield i
    if (lineCount.length >= 3) {
      return true
    }
    false
  }

  def leftMissing(): Boolean = {
    if (paths(0) == Line.Missing){
      return true
    }
    false
  }


  def upMissing(): Boolean = {
    if (paths(2) == Line.Missing) {
      return true
    }
    false
  }

  def downMissing(): Boolean = {
    if (paths(1) == Line.Missing) {
      return true
    }
    false
  }

  def rightMissing(): Boolean = {
    if (paths(3) == Line.Missing) {
      return true
    }
    false
  }
  def rightIllegal(): Boolean = {
    if (paths(3) == Line.Illegal) {
      return true
    }
    false
  }

  def leftIllegal(): Boolean = {
    if (paths(0) == Line.Illegal) {
      return true
    }
    false
  }

  def upIllegal(): Boolean = {
    if (paths(2) == Line.Illegal) {
      return true
    }
    false
  }

  def downIllegal(): Boolean = {
    if (paths(1) == Line.Illegal) {
      return true
    }
    false
  }

  def isEmpty: Boolean = {
    if(ttype == TileType.Empty){
      return true
    }
    false
  }

  def isWhite: Boolean = {
    if (ttype == TileType.White) {
      return true
    }
    false
  }


  def isBlack: Boolean = {
    if (ttype == TileType.Black) {
      return true
    }
    false
  }
def placedCount: Int = {
  val placed = {
  for(i <- paths if i==Line.Placed) yield i
  }
  return placed.length
}

  def illegalCount: Int = {
    val placed = {
      for(i <- paths if i==Line.Illegal) yield i
    }
    return placed.length
  }

  def missingCount: Int = {
    val placed = {
      for(i <- paths if i==Line.Missing) yield i
    }
    return placed.length
  }}