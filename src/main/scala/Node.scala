import Node.{falseString, trueString}

import scala.util.Random

case class Node(value: Boolean = Random.nextBoolean, inputs: (Int, Int, Int)) {
  override def toString: String = if (value) trueString else falseString
}

object Node {
  private val trueString = " "
  private val falseString = "0"
}

