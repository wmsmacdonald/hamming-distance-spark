import collection.mutable.{Queue, HashMap, MutableList}

case class IndexNode(fLSSeq: FLSSeq, children: MutableList[IndexNode],
                     value: MutableList[Int])

object DynamicHAIndex {
  def build(vectors: Array[FLSSeq], windowSize: Int): DynamicHAIndex = {
    val windows = vectors.zipWithIndex.sliding(windowSize, windowSize)
    val fLSSeqToIndexNode = new HashMap[FLSSeq, IndexNode]

    val level: MutableList[IndexNode] = MutableList()

    windows.foreach(window => {
      val (fLSSeqs, indexes): (Array[FLSSeq], Array[Int]) = window.unzip

      val parentFLSSeq = FLSSeq.extract(fLSSeqs)
      val childrenFLSSeq = fLSSeqs.map(_.opposite(parentFLSSeq))

      val parent = IndexNode(parentFLSSeq, MutableList[IndexNode](), MutableList[Int]())

      childrenFLSSeq.zip(indexes).foreach { case(fLSSeq, i) =>
        if (fLSSeqToIndexNode contains fLSSeq) {
          val matchingIndexNode: IndexNode = fLSSeqToIndexNode(fLSSeq)
          parent.children += matchingIndexNode
          matchingIndexNode.value += i
          println("matching")
        }
        else {
          val child = IndexNode(fLSSeq, MutableList(), MutableList(i))

          if (parent.fLSSeq.isEmpty) {
            println("no common")
            level += child
          }
          else {
            parent.children += child
          }
        }
      }

      if (parent.children.nonEmpty) {
        level += parent
      }

    })
    println(level.mkString("\n"))
    new DynamicHAIndex()
  }
}


class DynamicHAIndex() {

}
