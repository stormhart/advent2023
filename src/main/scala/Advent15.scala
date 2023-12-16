import scala.io.Source
import scala.collection.mutable

/* Notes:
- Part 1:
-- Simple recursion
-- Calling toInt() on a char will get its ascii equivalent
- Part 2:
-- Additional parsing
-- Simple mapping and calculations
-- Lenses in each box must be stored in an ordered list rather than an unordered map
 */
object Advent15 {
  def main(args: Array[String]): Unit = {
    val Test = false
    val file = if (Test) "advent15test.txt" else "advent15.txt"
    val lines = Source.fromFile(s"src/main/resources/$file").getLines.toList
    println(lines)



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

    val strings = lines.head.split(",").toList
    val asciiEquivalents = strings.map(string => string.map(_.toInt).toList)
    println(s"Part 1: ${asciiEquivalents.map(hash).sum}")

    // Part 2
    val boxes: mutable.Map[Int, List[(String, Int)]] = mutable.Map()

    def parseInput(lens: String) = {
      val parts          = lens.split("=")
      val (label, value) = parts.size match {
        case 2 => (parts(0), parts(1).toInt)
        case 1 => (parts(0).replace("-", ""), -1)
        case _ => throw new Exception("INVALID INPUT")
      }
      (label, value)
    }

    val lenses = strings.map(parseInput)
    // println(s"lenses $lenses")

    lenses.map(l => {
      val asciiSeq = l._1.map(_.toInt).toList
      val boxNumber   = hash(asciiSeq)
      val key         = l._1
      val focalLength = l._2
      focalLength match {
        case -1 => if (boxes.get(boxNumber).isDefined) boxes.update(boxNumber, boxes(boxNumber).filterNot(lens => lens._1 == key))
        case _  =>
          val maybeBox = boxes.get(boxNumber)
          val listOfLenses = maybeBox match {
            case None => List((key, focalLength))
            case Some(box) =>
              box.filter(_._1 == key) match {
                case List() => boxes(boxNumber) :+ (key, focalLength)
                case _ => boxes(boxNumber).map { case (found, _) if found == key => (key, focalLength); case x => x }
              }
          }
          if (boxes.get(boxNumber).isDefined) boxes.update(boxNumber, listOfLenses) else boxes.put(boxNumber, listOfLenses)
      }
    })

    val power = for (box <- 0 to 255) yield {boxes.getOrElse(box, List()).zipWithIndex.map(lenses => (box + 1) * (lenses._2 + 1) * lenses._1._2)}
    println(s"Part 2: ${power.flatten.sum}")

  }
}