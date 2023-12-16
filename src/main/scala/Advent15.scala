import scala.io.Source

// In Progress
object Advent15 {
  def main(args: Array[String]): Unit = {
    val Test = true
    val file = if (Test) "advent15test.txt" else "advent15.txt"
    val lines = Source.fromFile(s"src/main/resources/$file").getLines.toList
    println(lines)

    import scala.collection.mutable
    import scala.io.Source

    def hash(input: List[Int]): Int = {
      def iterate(input: List[Int], curValue: Int): Int = {
        if (input.isEmpty) curValue
        else {
          val newValue = curValue + input.head
          iterate(input.tail, (newValue * 17) % 256)
        }
      }
      iterate(input, 0)
    }

    val x = "qp".map(_.toInt).toList
    hash(x)

    1224 % 256

    val strings = lines.head.split(",").toList
    println(s"Part 1: ${strings.map(x => hash(x.map(_.toInt).toList)).sum}")

    // Part 2
//    val boxes: List[(Int, List[(String, Int)])] = (1 to 256).toList.map((_, List[(String, Int)]()))
//
//    def parseInput(lens: String) = {
//      val parts          = lens.split("=")
//      val (label, value) = parts.size match {
//        case 2 => (parts(0), parts(1).toInt)
//        case 1 => (parts(0).replace("-", ""), -1)
//        case _ => throw new Exception("INVALID INPUT")
//      }
//      (label, value)
//    }
//
//    val lenses = strings.map(parseInput)
//
//    val updatedBoxes = lenses.map(l => {
//      val boxNumber   = hash(l._1.map(_.toInt).toList) - 1
//      val key         = l._1
//      val focalLength = l._2
//      focalLength match {
//        case -1 => boxes.updated(boxNumber, boxes(boxNumber)._2.filterNot(lens => lens._1 == key))
//        case _  =>
//          val listOfLenses = boxes(boxNumber)._2.filter(_._1 == key) match {
//            case List() => boxes(boxNumber)._2 :+ key(focalLength)
//            case _      => boxes(boxNumber)._2.map { case (found, _) if found == key => (key, focalLength); case x => x }
//          }
//          boxes.updated(boxNumber, listOfLenses)
//      }
//    })

  }
}