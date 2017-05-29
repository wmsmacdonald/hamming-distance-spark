name := "hamming-distance-spark"

version := "1.0"

scalaVersion := "2.11.11"

libraryDependencies += "org.apache.spark" %% "spark-core" % "2.1.1"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.4.7" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")