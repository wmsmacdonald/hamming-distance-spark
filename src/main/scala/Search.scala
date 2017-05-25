
import org.apache.spark.rdd.RDD

trait Search extends Serializable {
  def selectKnn(k: Int)(vector: Array[Byte]): Array[(String, Int)]
  def joinKnn(k: Int)(vectors: RDD[Array[Byte]]): Array[Array[(String, Int)]]
  def train(vectors: RDD[Array[Byte]], values: RDD[String]): Search

  var trainingVectors: RDD[Array[Byte]]
  var values: RDD[String]
}
