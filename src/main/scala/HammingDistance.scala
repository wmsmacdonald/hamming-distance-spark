import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf
import org.apache.spark.rdd.RDD


object HammingDistance {

  def main(args: Array[String]) {
    val conf = new SparkConf().setAppName("HammingDistance").setMaster("local[8]")
    val sc = new SparkContext(conf)

    val descriptorsFile = "/home/bill/projects/image-feature-search/indexes/full_index"
    val valuesFile = "/home/bill/projects/image-feature-search/indexes/values"

    val descriptors = sc.binaryRecords(descriptorsFile, 32)
    val values = sc.textFile(valuesFile).collect.map(_.split(","))

    val names = values.map(_(0)).toList
    val lastIndexes = values.map(_(1).toInt).toList
    val ranges = Partitioned(lastIndexes)
    val getItemNum = Partitioned.getItemNum(ranges)(_)
    def getName(i: Int): String = names(getItemNum(i).get)

    val search = new LinearSearch()

    search.train(descriptors)

    val queryDescriptorsFile = "/home/bill/projects/image-feature-search/indexes/query_descriptors"

    val queryDescriptors = sc.binaryRecords(queryDescriptorsFile, 32)

    val indexResults: Array[Array[(Long, Int)]] = search.joinKnn(1)(queryDescriptors)

    val matches: Array[(String, Int)] =
      indexResults.map(_.head).map { case (i, d) => (getName(i.toInt), d) }

    val correctMatches = matches.filter(m => m._1 == "frame003.jpg" || m._1 == "frame004.jpg")

    val correctDistances = correctMatches.map(_._2)

    println(correctDistances.sum / correctDistances.length)

//    def encode(n: BigInt): BigInt = n ^ (n >> 1)
//
//    val grayCodes: RDD[BigInt] = descriptors.map(bytes => encode(BigInt(bytes)))
//
//    val sortedDescriptors: RDD[Array[Byte]] =
//      descriptors.zip(grayCodes).sortBy(el => el._2).keys
//
//    val strings: RDD[String] = sortedDescriptors.map(a => BigInt(a).toString(2))

    sc.stop()
  }

}
