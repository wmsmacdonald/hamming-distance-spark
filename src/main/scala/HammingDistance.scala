import java.math.BigInteger
import java.nio.file.{Files, Paths}
import java.util.Random

import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf
import org.apache.spark.rdd.RDD


object HammingDistance {

  def main(args: Array[String]) {

    val fullDescriptorsFile = "/home/bill/projects/image-feature-search/indexes/full_index"
    val queryDescriptorsFile = "/home/bill/projects/image-feature-search/indexes/query_index"
    val namesFile = "/home/bill/projects/image-feature-search/indexes/values"

    val fullDescriptors = Files.readAllBytes(Paths.get(fullDescriptorsFile))
      .sliding(32, 32).toList
    val queryDescriptors = Files.readAllBytes(Paths.get(queryDescriptorsFile))
      .sliding(32, 32).toList

    val (names, lastIndexes): (List[String], List[String]) = scala.io.Source.fromFile(namesFile).getLines()
      .map(_.split(",")).toList.map { x => (x(0), x(1))}.unzip

    val namesArr = names.toArray

    val partitions = Partitioned(lastIndexes.map(_.toInt))
    val files: List[String] = fullDescriptors.indices.map(x => {
      namesArr(Partitioned.getItemNum(partitions)(x).get)
    }).toList


    val ts = Array("000001001010", "000001011101", "000011001100", "000101001010",
    "000101110110", "000101011101", "000101101010", "000111001100").map(BigInt(_, 2))

    val nodes = DynamicHAIndex(fullDescriptors, files, 0.02, 3)
    val t0 = System.nanoTime()

    val results = queryDescriptors.map(desc => DynamicHAIndex.knn(nodes, 50, 2, desc))
    print(results)
    println("Time: " + ((System.nanoTime() - t0) / 1e9)  + " s")
    /*
    val vectors = Array(Array())

    val grayCodes: RDD[BigInt] = descriptors.map(bytes => encode(BigInt(bytes)))

    val sortedDescriptors: RDD[Array[Byte]] =
      descriptors.zip(grayCodes).sortBy(el => el._2).keys

    val strings: RDD[String] = sortedDescriptors.map(a => BigInt(a).toString(2))
    */

  }

}
