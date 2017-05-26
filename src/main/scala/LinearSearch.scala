import org.apache.spark.rdd._
import org.apache.spark.SparkContext._

import scala.reflect.ClassTag

class LinearSearch extends Search {

  def numBitsSet(b: Byte): Int = (0 to 7).map(i => (b >>> i) & 1).sum

  def computeDistance(bytes1: Array[Byte])(bytes2: Array[Byte]): Int =
    bytes1.zip(bytes2).map { case (b1, b2) => (b1 ^ b2).toByte }.map(numBitsSet).sum

  // returns indexes of results
  override def selectKnn(k: Int)(vector: Array[Byte]): Array[(Long, Int)] = {

    val distances = this.indexedTrainingVectors.map {
      case (i, v) =>  i -> computeDistance(vector)(v)
    }

    distances.takeOrdered(k)(Ordering.by[(Long, Int), Int](_._2))
  }

  override def joinKnn(k: Int)(vectors: RDD[Array[Byte]]): Array[Array[(Long, Int)]] =
    vectors.collect.map(selectKnn(k)(_))

  override def train(vectors: RDD[Array[Byte]]): Search = {
    this.trainingVectors = vectors
    this.indexedTrainingVectors = vectors.zipWithIndex.map { case (v, i) => i -> v }
    this
  }

  override var trainingVectors: RDD[Array[Byte]] = _
  var indexedTrainingVectors: RDD[(Long, Array[Byte])] = _
}
