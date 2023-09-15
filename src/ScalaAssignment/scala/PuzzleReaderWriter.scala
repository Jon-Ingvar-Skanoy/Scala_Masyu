import java.io.FileWriter
import scala.io.Source.fromFile
import scala.io._

object PuzzleReaderWriter{

  var unsolvedFile: String = ""
  var SolvedFile: String = ""
  var lines: List[String] = Nil
  var fw: FileWriter = null
  private val iterator: Array[Int] = Array.ofDim[Int](3)

  def initRW(infile: String, outfile: String) = {
    unsolvedFile = infile
    SolvedFile = outfile
    lines = Source.fromFile(unsolvedFile).getLines().toList
    fw = new FileWriter(SolvedFile, false)
  }
  def getNumPizzles():Int={
    val count = lines(0).split(" ").last.toInt
    fw.write("puzzles "+ count.toString + "\n")
    return count
  }

  def input(x:Int, y:Int, index:Int): Array[Array[Tile]] = {

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

  def closing()={
    fw.close()
  }
  def getPuzzle(index:Int):Puzzle={
    lines = Source.fromFile(unsolvedFile).getLines().toList
    val sizeNumbers = lines.filter(_ startsWith("size"))(index).split(" ").last.split("x")

    return new Puzzle(sizeNumbers(0).toInt,sizeNumbers.last.toInt, input(sizeNumbers(0).toInt,sizeNumbers.last.toInt,index))
  }


}
