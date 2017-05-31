
class LinearSearch {

  def numBitsSet(b: Byte): Int = (0 to 7).map(i => (b >>> i) & 1).sum

  def computeDistance(bytes1: Array[Byte])(bytes2: Array[Byte]): Int =
    bytes1.zip(bytes2).map { case (b1, b2) => (b1 ^ b2).toByte }.map(numBitsSet).sum

  // returns indexes of results
  def selectKnn(k: Int)(vector: Array[Byte]): List[(Int, Int)] = {

    val distances = this.indexedTrainingVectors.map {
      case (i, v) =>  i -> computeDistance(vector)(v)
    }

    distances.sortBy(_._2).take(k)
  }

  def joinKnn(k: Int)(vectors: List[Array[Byte]]): List[List[(Int, Int)]] =
    vectors.map(selectKnn(k)(_))

  def train(vectors: List[Array[Byte]]): LinearSearch = {
    this.trainingVectors = vectors
    this.indexedTrainingVectors = vectors.zipWithIndex.map { case (v, i) => i -> v }
    this
  }

  var trainingVectors: List[Array[Byte]] = _
  var indexedTrainingVectors: List[(Int, Array[Byte])] = _
}
