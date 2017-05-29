import java.math.BigInteger
import java.nio.file.{Files, Paths}
import java.util.Random

import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf
import org.apache.spark.rdd.RDD


object HammingDistance {

  def main(args: Array[String]) {


    val byteArray = Files.readAllBytes(Paths.get(
      "/home/bill/projects/image-feature-search/indexes/full_index"
    ))

    val descriptors = byteArray.sliding(4, 4)


    val ts = Array("000001001010", "000001011101", "000011001100", "000101001010",
    "000101110110", "000101011101", "000101101010", "000111001100").map(BigInt(_, 2))

    val nodes = DynamicHAIndex(descriptors.toList, 3500, 1)
    println(nodes.length)
    /*
    val vectors = Array(Array())

    val grayCodes: RDD[BigInt] = descriptors.map(bytes => encode(BigInt(bytes)))

    val sortedDescriptors: RDD[Array[Byte]] =
      descriptors.zip(grayCodes).sortBy(el => el._2).keys

    val strings: RDD[String] = sortedDescriptors.map(a => BigInt(a).toString(2))
    */

  }

}
