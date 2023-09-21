import java.io.FileWriter
import scala.io._

object PuzzleReaderWriter{

  private var unsolvedFile: String = ""
  private var SolvedFile: String = ""
  private var lines: List[String] = Nil
  private var fw: FileWriter = null
  private val iterator: Array[Int] = Array.ofDim[Int](3)

  def initRW(infile: String, outfile: String): Unit = {
    unsolvedFile = infile
    SolvedFile = outfile
    lines = Source.fromFile(unsolvedFile).getLines().toList
    fw = new FileWriter(SolvedFile, false)
  }
  def getNumPuzzles:Int={
    val count = lines.head.split(" ").last.toInt
    fw.write("puzzles "+ count.toString + "\n")
     count
  }

  private def input(x:Int, y:Int, index:Int): Array[Array[Tile]] = {
    // function that converts a input string to an array

     val tiles: Array[Array[Tile]] = Array.ofDim[Tile](y, x)
    iterator(0) = 0
    iterator(1) = 0
    iterator(2) = 0

    for (line <- lines) {



      if (line.startsWith("size")) iterator(0) = iterator(0) + 1
      else if (iterator(0) == index + 1) {
        for (i <- line if ' ' != i) {


          tiles(iterator(1))(iterator(2)) = Tile(i, iterator(1), iterator(2))

          iterator(2) = iterator(2) + 1


        }

        iterator(2) = 0
        iterator(1) = iterator(1) + 1
      }
    }
     tiles
  }

  def closing: Unit={
    fw.close()
  }

  def writeAnswer(board:Puzzle): Unit = {
    fw.write("size "+board.width+"x"+board.height+"\n")
    fw.write(board.boardString+"\n")
  }
  def getPuzzle(index:Int):Puzzle={
    lines = Source.fromFile(unsolvedFile).getLines().toList
    val sizeNumbers = lines.filter(_ startsWith "size")(index).split(" ").last.split("x")

    Puzzle(sizeNumbers(0).toInt,sizeNumbers.last.toInt, input(sizeNumbers(0).toInt,sizeNumbers.last.toInt,index))
  }


}
