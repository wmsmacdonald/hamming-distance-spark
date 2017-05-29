import collection.mutable.MutableList
import collection.immutable

case class IndexNode(fLSSeq: FLSSeq, children: MutableList[IndexNode],
                     value: MutableList[Int])

case class Level(nodes: List[IndexNode],
                 fLSSeqToIndexNode: immutable.HashMap[FLSSeq, IndexNode])

object DynamicHAIndex {

  def apply(vectors: List[Array[Byte]], windowSize: Int, depth: Int): List[IndexNode] = {

    val (sortedVectors, indexes) = DynamicHAIndex.grayOrder(vectors).unzip
    build(sortedVectors, indexes, windowSize, depth)
  }


  def grayOrder(vectors: List[Array[Byte]]): List[(Array[Byte], Int)] = {
    def bigIntegerLRS(n: BigInt, amount: Int): BigInt = {
      if (n >= 0) {
        n >> amount
      }
      else {
        // unset sign bit
        BigInt("0" + (n >> amount).toString(2).drop(1), 2)
      }
    }

    def leftPad(n: Int)(bytes: Array[Byte]): Array[Byte] = {
      Array.fill[Byte](n - bytes.length)(0.toByte) ++ bytes
    }

    def encode(n: BigInt): BigInt =  n ^ bigIntegerLRS(n, 1)

    val encoded = vectors.map(BigInt(_)).map(encode)

    val sorted = encoded.zip(vectors.zipWithIndex).sortBy(x => x._1).unzip
    sorted._2
  }

  def build(vectors: List[Array[Byte]],
            indexes: List[Int],
            windowSize: Int,
            depth: Int): List[IndexNode] = {

    val fLSSeqs = vectors.map(seq => FLSSeq(seq, Array.fill[Byte](seq.length)(-1)))
    val nodes = fLSSeqs.zip(indexes).map { case (fLSSeq, i) =>
      IndexNode(fLSSeq, MutableList(), MutableList(i))
    }
    val level = Level(nodes, immutable.HashMap[FLSSeq, IndexNode]())
    build(level, windowSize, depth)
  }


  def build(level: Level, windowSize: Int, depth: Int): List[IndexNode] = {
    if (depth == 0) {
      level.nodes
    }
    else {
      val windows: Iterator[List[IndexNode]] = level.nodes.sliding(windowSize, windowSize)

      val endLevel = windows.foldLeft(level)(DynamicHAIndex.buildWindowLevel)

      build(endLevel, windowSize, depth - 1)
    }
  }

  def buildWindowLevel(level: Level, nodes: List[IndexNode]): Level = {
    val fLSSeqs = nodes.map(_.fLSSeq)

    val parentFLSSeq = FLSSeq.extract(fLSSeqs)

    val parentNode = IndexNode(parentFLSSeq, MutableList[IndexNode](), MutableList[Int]())

    val newLevel = nodes.foldLeft(level)((level, childNode) => {
      val childFLSSeq = childNode.fLSSeq - parentFLSSeq
      assert(childFLSSeq.distance(parentFLSSeq) == 0)

      if (level.fLSSeqToIndexNode contains childFLSSeq) {
        val matchingIndexNode: IndexNode = level.fLSSeqToIndexNode(childFLSSeq)
        parentNode.children += matchingIndexNode
        matchingIndexNode.value += childNode.value.head
        println("matching")
        level
      }
      else {
        if (parentNode.fLSSeq.isEmpty) {
          println("no matches")
          Level(childNode::level.nodes, level.fLSSeqToIndexNode)
        }
        else {
          parentNode.children += childNode
          level
        }
      }

    })

    if (parentNode.children.isEmpty) {
      newLevel
    }
    else {
      Level(parentNode::newLevel.nodes, newLevel.fLSSeqToIndexNode)
    }
  }
}


class DynamicHAIndex() {

}
