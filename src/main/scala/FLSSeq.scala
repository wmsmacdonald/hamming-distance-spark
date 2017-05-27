import Function.tupled

object FLSSeq {
  def extract(fLSSeqs: Array[FLSSeq]): FLSSeq = {
    val sequences: Array[Array[Byte]] = fLSSeqs.map(_.sequence)
    val transposed: Array[Array[Byte]] = sequences.transpose
    val sequence: Array[Byte] = transposed.map(
      seq => seq.reduce((b1, b2) => (b1 & b2).toByte)
    )
    if (transposed(0).length == 1) {
      FLSSeq(sequence)
    }
    else {
      val mask: Array[Byte] = transposed.map { seq =>
        val groups = seq.sliding(2, 1).toList
        val r = groups.map({ bytes =>
          (bytes(0) ^ bytes(1)).toByte
        }).reduce((b1, b2) => (b1 | b2).toByte)
        (~r).toByte
      }
      FLSSeq(sequence, mask)
    }
  }

  def opposite(original: FLSSeq, other: FLSSeq): FLSSeq = {
    val mask = other.mask.map(m => (~m).toByte)
    FLSSeq(original.sequence, mask)
  }

  def distance(fLSSeq1: FLSSeq, fLSSeq2: FLSSeq): Int =
    fLSSeq1.sequence.zip(fLSSeq2.sequence).map(tupled(_ ^ _)).zip(fLSSeq1.mask)
      .map(tupled(_ & _)).zip(fLSSeq2.mask).map(tupled(_ & _)).map(Integer.bitCount).sum
}

case class FLSSeq(sequence: Array[Byte],
                  mask: Array[Byte] = Array.fill(32)(0.toByte)) {

  def equals(other: FLSSeq): Boolean = {
    this.mask.deep == other.mask.deep && FLSSeq.distance(this, other) == 0
  }

  def isEmpty: Boolean = mask.map((b: Byte) => Integer.bitCount(b)).sum == 0

  override def toString: String =
    s"FLSSeq(sequence: [${sequence.mkString(",")}], mask: [${mask.mkString(",")}])"
}


