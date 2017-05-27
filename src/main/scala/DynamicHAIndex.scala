import scala.collection.mutable.Queue

case class Node[T](children: List[Node[T]], value: T)

object DynamicHAIndex {
  def build(vectors: Array[FLSSeq], windowSize: Int): DynamicHAIndex = {
    val q = new Queue()
    val existingNodes = collection.mutable.Set[FLSSeq]()
    var root = Node(List(), None)

    for (i <- vectors.indices) {
      val parentFLSSeq = FLSSeq.extract(vectors.slice(i, i + windowSize))
      val childFLSSeq = FLSSeq.opposite(vectors(i), parentFLSSeq)
      println(parentFLSSeq)
      println(childFLSSeq)

      val child = Node(List(), childFLSSeq)
      val parent = Node(List(child), parentFLSSeq)

      if (!existingNodes.contains(childFLSSeq)){

      }

      if (!parentFLSSeq.isEmpty) {
        q.enqueue()
      }

    }
    println(q.mkString(", "))
    new DynamicHAIndex()
  }
}


class DynamicHAIndex() {

}
