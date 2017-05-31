import Function.tupled


// mask: bit is 0 -> wildcard, bit is 1 -> value defined in sequence
// bits in sequence covered by mask have no effect
case class FLSSeq(sequence: Array[Byte], mask: Array[Byte]) {
  require(sequence.length == mask.length)

  override def equals(other: Any): Boolean = {
    val otherFLSSeq = other.asInstanceOf[FLSSeq]
    val d = this.distance(otherFLSSeq)
    this.mask.deep == otherFLSSeq.mask.deep && d == 0
  }

  // TODO make hash function better distributed
  // cuts off most significant bytes to make int
  override def hashCode(): Int =
    (BigInt(this.sequence) & BigInt(this.mask)).intValue()


  def isEmpty: Boolean = mask.map((b: Byte) => Integer.bitCount(b)).sum == 0
  def notEmpty: Boolean = !isEmpty

  // sets additional wildcard bits from other so that there is no overlap
  def -(other: FLSSeq): FLSSeq = {
    val mask: Array[Byte] = other.mask.map(m => ~m).zip(this.mask)
      .map(tupled(_ & _)).map(_.toByte)
    FLSSeq(this.sequence, mask)
  }

  // masks combine, non-masked values are ORed
  def &(other: FLSSeq): FLSSeq = {
    val sequence = this.mask.zip(other.mask).map(tupled(_ & _)).map(_.toByte)
    val mask = this.mask.zip(other.mask).map(tupled(_ & _)).map(_.toByte)
    FLSSeq(sequence, mask)
  }

  def distance(other: FLSSeq): Int = {
    require(this.sequence.length == other.sequence.length)
    this.sequence.zip(other.sequence).map(tupled(_ ^ _)).zip(this.mask)
      .map(tupled(_ & _)).zip(other.mask).map(tupled(_ & _)).map(_ & 0xFF)
      .map(Integer.bitCount).sum
  }

  override def toString: String = {
    val string = byteArrayToString(mask).zip(byteArrayToString(sequence)).map {
      case (m, s) => if (m == '0') '.' else s
    }.mkString("")
    s"FLSSeq($string)"
  }

  def byteArrayToString(bytes: Array[Byte]): String = {
    def leftPad(n: Int)(str: String): String =
      ("0" * (n - str.length)) + str

    bytes.map(_ & 0xFF).map(Integer.toBinaryString).map(leftPad(8)).mkString("")
  }
}


object FLSSeq {

  def full(sequence: Array[Byte]): FLSSeq =
    FLSSeq(sequence, Array.fill[Byte](sequence.length)(-1))

  def full(length: Int): FLSSeq =
    FLSSeq(Array.fill[Byte](length)(-1), Array.fill[Byte](length)(-1))

  def extract(fLSSeqs: List[FLSSeq]): FLSSeq = {
    val sequences: List[Array[Byte]] = fLSSeqs.map(_.sequence)
    val transposedSeq: List[List[Byte]] = sequences.transpose
    val transposedMask: List[List[Byte]] = fLSSeqs.map(_.mask).transpose
    val sequence: Array[Byte] = transposedSeq.map(
      seq => seq.reduce((b1, b2) => (b1 & b2).toByte)
    ).toArray
    if (transposedSeq.head.length == 1) {
      FLSSeq(sequence, Array.fill[Byte](sequence.length)(-1))
    }
    else {
      val minMask = transposedMask.map(
        bytes => bytes.reduce((b1, b2) => (b1 & b2).toByte)
      ).toArray
      val valuesMask: Array[Byte] = transposedSeq.map { seq =>
        val groups = seq.sliding(2, 1).toList
        val r = groups.map({ bytes =>
          (bytes.head ^ bytes(1)).toByte
        }).reduce((b1, b2) => (b1 | b2).toByte)
        (~r).toByte
      }.toArray
      val mask = minMask.zip(valuesMask).map(tupled(_ & _)).map(_.toByte)
      FLSSeq(sequence, mask)
    }
  }
}
