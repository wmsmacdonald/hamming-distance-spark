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
}

case class FLSSeq(sequence: Array[Byte],
                  mask: Array[Byte] = Array.fill(32)(0.toByte)) {

  def equals(other: FLSSeq): Boolean =
    this.mask.deep == other.mask.deep && this.distance(other) == 0

  override def hashCode(): Int =
    (BigInt(this.sequence) + BigInt(this.mask)).intValue().hashCode()


  def isEmpty: Boolean = mask.map((b: Byte) => Integer.bitCount(b)).sum == 0

  def opposite(other: FLSSeq): FLSSeq = {
    val mask = other.mask.map(m => (~m).toByte)
    FLSSeq(this.sequence, mask)
  }

  def distance(other: FLSSeq): Int =
    this.sequence.zip(other.sequence).map(tupled(_ ^ _)).zip(this.mask)
      .map(tupled(_ & _)).zip(other.mask).map(tupled(_ & _)).map(Integer.bitCount).sum

  override def toString: String =
    s"FLSSeq(seq: ${BigInt(sequence).toString(2)}, mask: ${BigInt(mask).toString(2)})"
}


