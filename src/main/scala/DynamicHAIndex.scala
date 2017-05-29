import collection.mutable.MutableList
import collection.immutable

case class IndexNode(fLSSeq: FLSSeq, children: MutableList[IndexNode], value: Option[Any])

object DynamicHAIndex {
  def knn(nodes: List[IndexNode], threshold: Int, k: Int, vector: Array[Byte]): List[(Any, Int)] = {
    val fLSSeq = FLSSeq.full(vector)
    val resultNodes: List[IndexNode] = search(nodes, threshold, fLSSeq)
    println(resultNodes.length)
    val leafNodes = resultNodes.filter(n => n.value.isDefined)
    val distances = leafNodes.map(_.fLSSeq.distance(fLSSeq))
    leafNodes.zip(distances).sortBy(_._2).take(k).map {
      case (n, d) => (n.value.get, d)
    }
  }

  def search(nodes: List[IndexNode], threshold: Int, vector: FLSSeq): List[IndexNode] = {
    if (nodes.isEmpty) {
      List()
    }
    else {
      val closeNodes = nodes.filter(n => n.fLSSeq.distance(vector) <= threshold)
      closeNodes ++ closeNodes.flatMap(n => search(n.children.toList, threshold, vector))
    }
  }

  def apply(vectors: List[Array[Byte]], values: List[Any],
            windowFraction: Double, depth: Int): List[IndexNode] = {

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

    val (sortedVectors, sortedValues) =
      encoded.zip(vectors.zip(values)).sortBy(x => x._1).unzip._2.unzip

    build(sortedVectors, sortedValues, windowFraction, depth)
  }


  def build(vectors: List[Array[Byte]],
            values: List[Any],
            windowFraction: Double,
            depth: Int): List[IndexNode] = {

    val fLSSeqs = vectors.map(seq => FLSSeq.full(seq))
    val nodes = fLSSeqs.zip(values).map { case (fLSSeq, v) =>
      IndexNode(fLSSeq, MutableList(), Some(v))
    }
    buildLevel(nodes, List(), windowFraction, depth)
  }


  def buildLevel(nodes: List[IndexNode], level: List[IndexNode],
                 windowFraction: Double, depth: Int): List[IndexNode] = {
    if (depth == 0) {
      nodes
    }
    else {
      val windowLength = (nodes.length * windowFraction).toInt
      val windows: List[List[IndexNode]] = nodes.sliding(windowLength, windowLength).toList

      val nextNodes = windows.foldLeft(List[IndexNode]())(DynamicHAIndex.buildWindowLevel)

      buildLevel(nextNodes, List(), windowFraction, depth - 1)
    }
  }

  def buildWindowLevel(allNextLevelNodes: List[IndexNode],
                       windowNodes: List[IndexNode]): List[IndexNode] = {
    val fLSSeqs = windowNodes.map(_.fLSSeq)

    val parentFLSSeq = FLSSeq.extract(fLSSeqs)


    if (parentFLSSeq.isEmpty) {
      windowNodes ++ allNextLevelNodes
    }
    else {
      val parentNode = IndexNode(parentFLSSeq, MutableList[IndexNode](windowNodes:_*),
        None)
      parentNode :: allNextLevelNodes
    }
  }
}


