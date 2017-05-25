import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf
import org.apache.spark.rdd.RDD


object HammingDistance {

  def main(args: Array[String]) {
    val conf = new SparkConf().setAppName("HammingDistance").setMaster("local[8]")
    val sc = new SparkContext(conf)

    val descriptorsFile = "/home/bill/projects/image-feature-search/indexes/full_index"
    val namesFile = "/home/bill/projects/image-feature-search/indexes/names"

    val descriptors = sc.binaryRecords(descriptorsFile, 32)
    val names = sc.textFile(namesFile)

    val search = new LinearSearch()

    search.train(descriptors, names)

    val queryDescriptorsFile = "/home/bill/projects/image-feature-search/indexes/query_index"

    val queryDescriptors = sc.binaryRecords(queryDescriptorsFile, 32)

    val results = search.joinKnn(2)(queryDescriptors)
    println(results.mkString("\n"))

//    def encode(n: BigInt): BigInt = n ^ (n >> 1)
//
//    val grayCodes: RDD[BigInt] = descriptors.map(bytes => encode(BigInt(bytes)))
//
//    println("grayCodes", grayCodes.count())
//
//    val sortedDescriptors: RDD[Array[Byte]] =
//      descriptors.zip(grayCodes).sortBy(el => el._2).keys
//
//    val strings: RDD[String] = sortedDescriptors.map(a => BigInt(a).toString(2))

    sc.stop()
  }

}
