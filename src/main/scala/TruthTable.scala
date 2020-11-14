import scala.util.Random

class TruthTable private(table: Seq[Boolean]) {
  def apply(vals: (Boolean, Boolean, Boolean)): Boolean = vals match {
    case (x, y, z) =>
      table(valuesToIdx(x, y, z))
  }

  def valuesToIdx(x: Boolean, y: Boolean, z: Boolean): Int = {
    (if (x) 4 else 0) + (if (y) 2 else 0) + (if (z) 1 else 0)
  }

  override def toString: String = table.zipWithIndex.map {
    case (b: Boolean, i: Int) => String.format("%3s | %b\n", i.toBinaryString, b)
  }.mkString
}

object TruthTable {
  def generateTable: Seq[Boolean] = Seq.fill(math.pow(2, 3).toInt) {
    Random.nextBoolean
  }

  def apply(): TruthTable = new TruthTable(generateTable)
}
