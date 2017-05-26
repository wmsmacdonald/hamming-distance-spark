

object Partitioned {
  def apply(lastIndexes: List[Int]): List[Range] =
    (-1 :: lastIndexes).sliding(2, 1).map(n1_n2 => (n1_n2.head + 1) to n1_n2(1)).toList

  def getItemNum(ranges: List[Range])(index: Int): Option[Int] =
    ranges.zipWithIndex.find { case (r, _) => r.contains(index) }.map(_._2)
}
