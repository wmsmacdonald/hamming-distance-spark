import utest._
import utest.framework.{Test, Tree}

object FLSSeqTest extends TestSuite {
  val f1 = new FLSSeq(Array(1.toByte, 1.toByte), Array(1.toByte, 4.toByte))
  val f2 = new FLSSeq(Array((-1).toByte, 1.toByte), Array(1.toByte, 4.toByte))
  val f3 = new FLSSeq(Array(1.toByte, 1.toByte), Array(0.toByte, 0.toByte))
  val f4 = new FLSSeq(Array(0.toByte, 1.toByte), Array(1.toByte, 4.toByte))
  // 0x0E -> 00001110, 0xFB -> 11111011
  val f5 = new FLSSeq(Array(1.toByte, 0x0E.toByte), Array(1.toByte, 0xFE.toByte))

  // 0xEB -> 1110 1011, 0xBF -> 1011 1111
  val fs1 = FLSSeq(Array(0xEB.toByte), Array(0xBF.toByte))
  // 0xEB -> 1110 1011, 0xB1 -> 1011 0001
  val fs2 = FLSSeq(Array(0xEB.toByte), Array(0xB1.toByte))
  // 0xEB -> 1110 1011, 0xFF -> 1111 1111
  val fs3 = FLSSeq(Array(0xEB.toByte), Array(0xFF.toByte))
  // 0xEF -> 1110 1111, 0x3F -> 0011 1111
  val fs4 = FLSSeq(Array(0xEF.toByte), Array(0x3F.toByte))

  val tests: Tree[Test] = this {
    'equals {
      assert(f1 == f1)
      assert(f1 == f2)
      assert(f1 != f3)
      assert(f1 != f4)

      assert(f2 != f3)
      assert(f2 != f4)

      assert(f3 != f4)
    }
    'hashCode {
      val s1 = Set(f1)
      assert(s1.contains(f2))
      assert(!s1.contains(f3))
      assert(!s1.contains(f4))
    }
    'isEmpty {
      assert(f3.isEmpty)
      assert(f1.notEmpty)
    }
    '- {
      val expected1 = FLSSeq(Array(0xEB.toByte), Array(0x0E.toByte))
      val expected2 = FLSSeq(Array(0xEB.toByte), Array(0x4E.toByte))

      val result1 = fs1 - fs2
      val result2 = fs3 - fs2

      assert(expected1 == result1)
      assert(expected2 == result2)
    }
    '& {
      val expected1 = FLSSeq(Array(0xFF.toByte), Array(0x31.toByte))

      val r1 = fs2 & fs4

      assert(expected1 == r1)
    }
    'distance {
      val result1 = f1.distance(f2)
      val result2 = f1.distance(f4)
      val result3 = f1.distance(f5)
      val result4 = f5.distance(f1)

      assert(result1 == 0)
      assert(result2 == 1)
      assert(result3 == 1)
      assert(result4 == 1)
    }
  }
}
