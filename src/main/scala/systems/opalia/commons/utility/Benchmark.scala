package systems.opalia.commons.utility


object Benchmark {

  case class Measurement[R](result: R,
                            sum: Long,
                            maximum: Long,
                            minimum: Long,
                            average: Double,
                            variance: Double)

  def run[R](warmup: Int = 0,
             repeat: Int = 1,
             winner: (R, R) => R = (a: R, b: R) => b)
            (block: => R): Measurement[R] = {

    def sample: (R, Long, Long, Long, Long) = {

      val t0 = System.nanoTime()
      val result = block
      val t1 = System.nanoTime()
      val value = t1 - t0

      (result, value, value, value, value * value)
    }

    def process(repeat: Int): (R, Long, Long, Long, Long) = {
      (1 until repeat).foldLeft(sample) {
        (a, _) =>

          val b = sample

          (winner(a._1, b._1),
            a._2 + b._2,
            math.max(a._3, b._3),
            math.min(a._4, b._4),
            a._5 + b._5)
      }
    }

    if (warmup < 0)
      throw new IllegalArgumentException("Expect warmup value greater than or equal 0.")

    if (repeat < 1)
      throw new IllegalArgumentException("Expect repeat value greater than or equal 1.")

    if (warmup > 0)
      process(warmup)

    val samples = process(repeat)

    val average = samples._2.toDouble / repeat.toDouble
    val variance = samples._5 / repeat.toDouble - average * average

    Measurement(
      samples._1,
      samples._2,
      samples._3,
      samples._4,
      average,
      variance)
  }
}
