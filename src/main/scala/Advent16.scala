import scala.io.Source

/* Notes:
- Simple recursive solution
- Kept track of visited splitters to avoid infinite loops
- Dataset is relatively small
 */
object Advent16 {
  def main(args: Array[String]): Unit = {
    val Test = false
    val file = if (Test) "advent16test.txt" else "advent16.txt"
    val lines = Source.fromFile(s"src/main/resources/$file").getLines.toList
    println(lines.mkString("\n"))

    val mirrorMaze = lines.map(_.toCharArray.toList)
    val maxRow     = mirrorMaze.size - 1
    val maxCol     = mirrorMaze.head.size - 1

    sealed trait Direction
    object Up extends Direction
    object Down extends Direction
    object Left extends Direction
    object Right extends Direction

    def move(curPos: (Int, Int), direction: Direction): (Int, Int) = {
      val (row, col) = curPos
      val result     = direction match {
        case Up    => (row - 1, col)
        case Down  => (row + 1, col)
        case Left  => (row, col - 1)
        case Right => (row, col + 1)
      }
      //  println(s"Moving from $curPos to $result with $direction")
      result
    }

    def travel(curPos: (Int, Int), prevDirection: Direction, beamPath: List[(Int, Int)], allBeamPaths: List[List[(Int, Int)]]): List[List[(Int, Int)]] = {
      var visitedSplitters: List[(Int, Int)] = List()

      def traverse(curPos: (Int, Int), prevDirection: Direction, beamPath: List[(Int, Int)], allBeamPaths: List[List[(Int, Int)]]): List[List[(Int, Int)]] = {
        //  println("curPos " + curPos)
        //  println("prevDirection " + prevDirection)
        //  println("beamPath " + beamPath)
        //  println("allBeamPaths " + allBeamPaths)
        val (curRow, curCol) = curPos
        if (curRow > maxRow || curCol > maxCol || curRow < 0 || curCol < 0) allBeamPaths :+ beamPath
        else {
          val curSpot = mirrorMaze(curRow)(curCol)
          //    println("curSpot " + curSpot)
          (curSpot, prevDirection) match {
            case ('.', _)      => traverse(move(curPos, prevDirection), prevDirection, beamPath :+ curPos, allBeamPaths)
            case ('/', Up)     => traverse(move(curPos, Right), Right, beamPath :+ curPos, allBeamPaths)
            case ('/', Down)   => traverse(move(curPos, Left), Left, beamPath :+ curPos, allBeamPaths)
            case ('/', Left)   => traverse(move(curPos, Down), Down, beamPath :+ curPos, allBeamPaths)
            case ('/', Right)  => traverse(move(curPos, Up), Up, beamPath :+ curPos, allBeamPaths)
            case ('\\', Up)    => traverse(move(curPos, Left), Left, beamPath :+ curPos, allBeamPaths)
            case ('\\', Down)  => traverse(move(curPos, Right), Right, beamPath :+ curPos, allBeamPaths)
            case ('\\', Left)  => traverse(move(curPos, Up), Up, beamPath :+ curPos, allBeamPaths)
            case ('\\', Right) => traverse(move(curPos, Down), Down, beamPath :+ curPos, allBeamPaths)
            case ('-', Up)     =>
              if (!visitedSplitters.contains(curPos)) {
                visitedSplitters = visitedSplitters :+ curPos
                traverse(move(curPos, Left), Left, beamPath :+ curPos, allBeamPaths) ::: traverse(move(curPos, Right), Right, List(curPos), List())
              } else allBeamPaths :+ beamPath
            case ('-', Down)   =>
              if (!visitedSplitters.contains(curPos)) {
                visitedSplitters = visitedSplitters :+ curPos
                traverse(move(curPos, Left), Left, beamPath :+ curPos, allBeamPaths) ::: traverse(move(curPos, Right), Right, List(curPos), List())
              } else allBeamPaths :+ beamPath
            case ('-', Left)   => traverse(move(curPos, prevDirection), prevDirection, beamPath :+ curPos, allBeamPaths)
            case ('-', Right)  => traverse(move(curPos, prevDirection), prevDirection, beamPath :+ curPos, allBeamPaths)
            case ('|', Up)     => traverse(move(curPos, Up), prevDirection, beamPath :+ curPos, allBeamPaths)
            case ('|', Down)   => traverse(move(curPos, Down), prevDirection, beamPath :+ curPos, allBeamPaths)
            case ('|', Left)   =>
              if (!visitedSplitters.contains(curPos)) {
                visitedSplitters = visitedSplitters :+ curPos
                traverse(move(curPos, Up), Up, beamPath :+ curPos, allBeamPaths) ::: traverse(move(curPos, Down), Down, List(curPos), List())
              } else allBeamPaths :+ beamPath
            case ('|', Right)  =>
              if (!visitedSplitters.contains(curPos)) {
                visitedSplitters = visitedSplitters :+ curPos
                traverse(move(curPos, Up), Up, beamPath :+ curPos, allBeamPaths) ::: traverse(move(curPos, Down), Down, List(curPos), List())
              } else allBeamPaths :+ beamPath
            case _             => throw new Exception("Shouldn't happen")
          }
        }
      }
      traverse(curPos, prevDirection, beamPath, allBeamPaths)
    }

    // Part 1
    println(s"Part 1: ${travel((0, 0), Right, List(), List()).flatten.toSet.size}")

    // Part 2
    val rightBeams = (0 to maxRow)
      .map(startRow => {
        travel((startRow, 0), Right, List(), List()).flatten.toSet.size
      })
      .toList

    val leftBeams = (0 to maxRow)
      .map(startRow => {
        travel((startRow, maxCol), Left, List(), List()).flatten.toSet.size
      })
      .toList

    val bottomBeams = (0 to maxCol)
      .map(startCol => {
        travel((maxRow, startCol), Up, List(), List()).flatten.toSet.size
      })
      .toList

    val topBeams = (0 to maxCol)
      .map(startCol => {
        travel((0, startCol), Down, List(), List()).flatten.toSet.size
      })
      .toList

    println(s"Part 2: ${(rightBeams ::: leftBeams ::: bottomBeams ::: topBeams).max}")
  }
}