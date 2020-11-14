import scala.util.Random

class Network private(size: Int, truthTable: TruthTable, nodes: Seq[Node]) {
  def nextNode(node: Node): Node = {
    val (x, y, z) = node.inputs
    val nextVal = truthTable(nodes(x).value, nodes(y).value, nodes(z).value)
    Node(nextVal, node.inputs)
  }

  def nextIteration: Network = new Network(size, truthTable, nodes map {
    nextNode
  })

  def configToString: String = "Truth table:\n" ++ truthTable.toString ++ "Node connections:\n" ++ nodesToString

  def nodesToString: String = nodes.map {
    _.inputs
  }.zipWithIndex.map {
    case (t, i) => String.format("%2d | %2d, %2d, %2d\n", i, t._1, t._2, t._3)
  }.mkString

  override def toString: String =
    nodes.foldLeft(new StringBuilder)({
      _.append(_)
    }).append('\n').toString
}

object Network {
  val defaultSize = 20
  val defaultIterations: Int = 50

  def apply(size: Int = defaultSize): Network = {

    val nodeConnections: Seq[(Int, Int, Int)] = Seq.fill(size) {
      (Random.nextInt(size), Random.nextInt(size), Random.nextInt(size))
    }

    val nodes: Seq[Node] =
      nodeConnections map (x => Node(Random.nextBoolean, (x._1, x._2, x._3)))

    new Network(size, TruthTable(), nodes)
  }

  def run(iterations: Int = defaultIterations, size: Int = defaultSize)(f: Network => Unit): Unit = {
    var network: Network = Network(size)
    println(network.configToString)
    f(network)

    for (_ <- 0 until iterations) {
      network = network.nextIteration
      f(network)
    }
  }
}
