import org.apache.spark.rdd._
import org.apache.spark.SparkContext._

import scala.reflect.ClassTag

class LinearSearch extends Search {
  override def selectKnn(k: Int)(vector: Array[Byte]): Array[(String, Int)] = {

    def numBitsSet(b: Byte): Int = (0 to 7).map(i => (b >>> i) % 1).sum

    def computeDistance(bytes1: Array[Byte])(bytes2: Array[Byte]): Int =
      (BigInt(bytes1) ^ BigInt(bytes2)).toByteArray.map(numBitsSet).sum

    val distances = this.indexedTrainingVectors.map {
      case (i, v) => i -> computeDistance(vector)(v)
    }

    val orderedDistances = distances.takeOrdered(k)(Ordering.by[(Long, Int), Int](_._2))
    orderedDistances.map { case (i, d) => (this.indexedValues.lookup(i).head, d) }
  }

  override def joinKnn(k: Int)(vectors: RDD[Array[Byte]]): Array[Array[(String, Int)]] =
    vectors.collect.map(selectKnn(k)(_))

  override def train(vectors: RDD[Array[Byte]], values: RDD[String]): Search = {
    this.trainingVectors = vectors
    this.indexedTrainingVectors = vectors.zipWithIndex.map { case (v, i) => i -> v }
    this.values = values
    this.indexedValues = values.zipWithIndex.map { case (v, i) => i -> v }
    this
  }

  override var trainingVectors: RDD[Array[Byte]] = _
  override var values: RDD[String] = _
  var indexedTrainingVectors: RDD[(Long, Array[Byte])] = _
  var indexedValues: RDD[(Long, String)] = _
}
