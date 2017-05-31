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
      .sliding(16, 16).toList
    val queryDescriptors = Files.readAllBytes(Paths.get(queryDescriptorsFile))
      .sliding(16, 16).toList

    val (names, lastIndexes): (List[String], List[String]) = scala.io.Source.fromFile(namesFile).getLines()
      .map(_.split(",")).toList.map { x => (x(0), x(1)) }.unzip

    val namesArr = names.toArray

    val partitions = Partitioned(lastIndexes.map(_.toInt))
/*
    val ls = new LinearSearch()
    ls.train(fullDescriptors)
    val result = ls.selectKnn(1)(queryDescriptors.head).head
    val file = names(Partitioned.getItemNum(partitions)(result._1).get)
    println(file, result._2)

    */

    val ts = Array("000001001010", "000001011101", "000011001100", "000101001010",
    "000101110110", "000101011101", "000101101010", "000111001100").map(BigInt(_, 2))

    val queryFLSSeq = FLSSeq.full(queryDescriptors.head)

    val nodes = DynamicHAIndex(fullDescriptors, fullDescriptors.indices.toList, 50, 4)
    println("done indexing")
    val t0 = System.nanoTime()
    val results =  queryDescriptors.slice(0,1).map(d =>
      DynamicHAIndex.knn(nodes, threshold = 11, k = 1, d)
    )
    println(results)

    println(s"nodes searched ${DynamicHAIndex.nodesSearched}")

    val sorted = results.flatten.groupBy(_._1).map {
      p => (names(
        Partitioned.getItemNum(partitions)(p._1.asInstanceOf[Int]).get
      ), p._2.length)
    }
    print(sorted)
    println("Time: " + ((System.nanoTime() - t0) / 1e9)  + " s")
  }
}
