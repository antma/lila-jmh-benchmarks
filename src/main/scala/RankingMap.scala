package lila.tournament
import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

object User {
  type ID = String
}

/* https://en.wikipedia.org/wiki/Double_hashing
 * immutable interface for open addressing hash tables,
 * mapping from users to zero-index ranks
 * performance gain over Map[String, Int] due:
 * a) class doesn't need to support modification operations after building
 * b) ranks always in a range from 0 to users.length - 1, no need to boxing Ints
 * c) due special building order, it is possible answer to queries 'is user has lower rank than given'
 *    with less number of comparisions
 */
private[tournament] class RankingMap(users: Array[User.ID]) {
  private[this] val bits = {
    val l = users.length
    val k = 32 - Integer.numberOfLeadingZeros(l)
    if (1.5 * l < (1 << k)) k else k + 1
  }
  private[this] val mask = (1 << bits) - 1
  private[this] val h    = Array.fill(1 << bits)(Int.MaxValue)
  private[this] def seek(key: User.ID, rank: Int): Int = {
    val hc  = key.hashCode
    var pos = hc & mask
    //use odd steps, since step should be relative prime to hash size equal to 2^bits
    val step = ((hc >>> (bits - 1)) & mask) | 1
    while (h(pos) < rank && users(h(pos)) != key) {
      pos = (pos + step) & mask
    }
    pos
  }
  //caller should log if duplicates > 0
  final val duplicates = {
    var dups = 0
    //adding better ranked users first c)
    for (i <- 0 until users.length) {
      val pos = seek(users(i), i)
      if (h(pos) >= i) {
        h(pos) = i
      } else {
        dups += 1
      }
    }
    dups
  }
  /* betterRank stops searching after finding slot with rank not lesser than rank parameter
   * if return value is less than parameter then it is precise
   * otherwise it is just estimation
   * (guaranteed only that user's rank not less than given parameter or user couldn't be found)
   * returned None value guaranteed that user is not in ranks
   */
  final def betterRank(key: User.ID, rank: Int): Option[Int] = {
    val k = h(seek(key, rank))
    if (k < Int.MaxValue) Some(k) else None
  }
  final def get(key: User.ID): Option[Int] = betterRank(key, Int.MaxValue)
  final def size                           = users.length
}

object RandomIds {
  val users = 20000
  def nextChar(): Char = {
    val i = scala.util.Random.nextInt(62)
    if (i < 26) i + 65
    else if (i < 52) i + 71
    else i - 4
  }.toChar
  def nextString(len: Int): String = {
    val sb = new StringBuilder(len)
    for (_ <- 0 until len) sb += nextChar()
    sb.result()
  }
  def randomId(): User.ID = nextString(10)
  scala.util.Random.setSeed(12345)
  val ids = Array.fill(users)(randomId())
}

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
class ScalaMap {
  @Benchmark
  def build() = {
    import RandomIds.ids
    val b = Map.newBuilder[User.ID, Int]
    for (i <- 0 until ids.length) {
      b += ids(i) -> i
    }
    b.result()
  }
}

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
class RankMapTest {
  @Benchmark
  def build() = new RankingMap(RandomIds.ids)
}


