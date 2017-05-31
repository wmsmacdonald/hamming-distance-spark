import collection.mutable.MutableList
import collection.immutable

case class IndexNode(fLSSeq: FLSSeq, children: List[IndexNode],
                     value: Option[(FLSSeq, Any)])


object DynamicHAIndex {
  var nodesSearched = 0

  def knn(nodes: List[IndexNode], threshold: Int, k: Int, vector: Array[Byte]): List[(Any, Int)] = {
    val fLSSeq = FLSSeq.full(vector)
    val resultNodes: List[IndexNode] = search(nodes, threshold, fLSSeq)
    val distances: List[Int] =
      resultNodes.map(n => fLSSeq.distance(n.value.get._1))
    resultNodes.zip(distances).sortBy(_._2).take(k).map {
      case (n, d) => (n.value.get._2, d)
    }
  }

  def search(nodes: List[IndexNode], threshold: Int, vector: FLSSeq): List[IndexNode] = {
    if (nodes.isEmpty) {
      List()
    }
    else {
      nodesSearched += nodes.length
      val distances = nodes.map(_.fLSSeq.distance(vector))
      val closeNodes_distances = nodes.zip(distances).filter(_._2 <= threshold)

      val result: List[IndexNode] = closeNodes_distances.flatMap { case(n, d) =>
        search(n.children.toList, threshold - d, vector)
      }

      val leafNodes = closeNodes_distances.unzip._1.filter(n => n.value.isDefined)
      leafNodes:::result
    }
  }

  def apply(vectors: List[Array[Byte]], values: List[Any],
            windowSize: Int, depth: Int): List[IndexNode] = {

    def bigIntegerLRS(n: BigInt, amount: Int): BigInt = {
      if (n >= 0) {
        n >> amount
      }
      else {
        // unset sign bit
        BigInt("0" + (n >> amount).toString(2).drop(1), 2)
      }
    }

    def encode(n: BigInt): BigInt =  n ^ bigIntegerLRS(n, 1)

    val encoded = vectors.map(BigInt(_)).map(encode)

    val (sortedVectors, sortedValues) =
      encoded.zip(vectors.zip(values)).sortBy(x => x._1).unzip._2.unzip


    build(sortedVectors, sortedValues, windowSize, depth)
  }


  def build(vectors: List[Array[Byte]],
            values: List[Any],
            windowSize: Int,
            depth: Int): List[IndexNode] = {

    val fLSSeqs = vectors.map(seq => FLSSeq.full(seq))
    val nodes = fLSSeqs.zip(values).map { case (fLSSeq, v) =>
      IndexNode(fLSSeq, List(), Some(fLSSeq, v))
    }
    buildLevel(nodes, List(), windowSize, depth)
  }


  def buildLevel(nodes: List[IndexNode], level: List[IndexNode],
                 windowSize: Int, depth: Int): List[IndexNode] = {
    if (depth == 0) {
      nodes
    }
    else {
      val windowLength = (0.005 * nodes.length).toInt
      val windows: List[List[IndexNode]] = nodes.sliding(windowLength, windowLength).toList

      val nextNodes = windows.foldLeft(List[IndexNode]())(DynamicHAIndex.buildWindowLevel)

      val topNodes = nextNodes.map(n => updateBranch(FLSSeq(
        Array.fill[Byte](n.fLSSeq.sequence.length)((-1).toByte),
        Array.fill[Byte](n.fLSSeq.mask.length)(0.toByte)))(n)
      )
      buildLevel(topNodes, List(), windowSize, depth - 1)
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
      val complementNodes = windowNodes.map(
        n => IndexNode(n.fLSSeq - parentFLSSeq, n.children, n.value)
      )
      val parentNode = IndexNode(parentFLSSeq, complementNodes, None)
      parentNode :: allNextLevelNodes
    }
  }

  def updateBranch(parents: FLSSeq)(node: IndexNode): IndexNode = {
    if (node.children.isEmpty) {
      IndexNode(node.fLSSeq - parents, node.children, node.value)
    }
    else {
      val children = node.children.map(n =>
        updateBranch(parents & n.fLSSeq)(n)
      )
      IndexNode(node.fLSSeq - parents, children, node.value)
    }
  }
}


