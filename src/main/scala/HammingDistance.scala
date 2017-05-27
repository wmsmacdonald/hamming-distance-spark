import java.math.BigInteger
import java.nio.file.Files
import java.util.Random

import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf
import org.apache.spark.rdd.RDD


object HammingDistance {

  def main(args: Array[String]) {

    def bigIntegerLRS(n: BigInt, amount: Int): BigInt = {
      if (n >= 0) {
        n >> amount
      }
      else {
        // unset sign bit
        BigInt("0" + (n >> amount).toString(2).drop(1), 2)
      }
    }

    def leftPad(n: Int)(bytes: Array[Byte]): Array[Byte] = {
      Array.fill[Byte](n - bytes.length)(0.toByte) ++ bytes
    }


    def encode(n: BigInt): BigInt =  n ^ bigIntegerLRS(n, 1)

    val ts = Array("000001001010", "000001011101", "000011001100", "000101001010",
    "000101110110", "000101011101", "000101101010", "000111001100").map(BigInt(_, 2))

    val (_, grayOrdered) = ts.map(encode).zip(ts).sortBy(x => x._1).unzip

    val sequences = grayOrdered.map(_.toByteArray).map(leftPad(4)(_))


    val fLSSeqs = sequences.map(FLSSeq(_))

    DynamicHAIndex.build(fLSSeqs, 2)
    /*
    val vectors = Array(Array())

    val grayCodes: RDD[BigInt] = descriptors.map(bytes => encode(BigInt(bytes)))

    val sortedDescriptors: RDD[Array[Byte]] =
      descriptors.zip(grayCodes).sortBy(el => el._2).keys

    val strings: RDD[String] = sortedDescriptors.map(a => BigInt(a).toString(2))
    */

  }

}
