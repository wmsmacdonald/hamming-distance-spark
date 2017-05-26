
import org.apache.spark.rdd.RDD

trait Search extends Serializable {
  def selectKnn(k: Int)(vector: Array[Byte]): Array[(Long, Int)]
  def joinKnn(k: Int)(vectors: RDD[Array[Byte]]): Array[Array[(Long, Int)]]
  def train(vectors: RDD[Array[Byte]]): Search

  var trainingVectors: RDD[Array[Byte]]
}
