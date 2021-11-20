package nutcracker.util.typealigned

import org.scalatest.FunSuite

import scalaz.std.function._
import scalaz.std.string._

class BalancedComposerTest extends FunSuite {

  test("post-compose stack safety") {
    val composer = BalancedPostComposer[* => *, Int, Int](i => i)
    val f = (1 to 10000).foldLeft(composer)((c, _) => c :+ (_ + 1)).reduceRight
    val n = f(0)
    assertResult(10000)(n)
  }

  test("pre-compose stack safety") {
    val composer = BalancedPreComposer[* => *, Int, Int](i => i)
    val f = (1 to 10000).foldLeft(composer)((c, _) => ((i: Int) => i + 1) +: c).reduceLeft
    val n = f(0)
    assertResult(10000)(n)
  }

  test("appender order") {
    val builder = BalancedAppender("").append("a").append("b").append("c")
    assertResult("abc")(builder.result)
  }

  test("prepender order") {
    val builder = BalancedPrepender("").prepend("a").prepend("b").prepend("c")
    assertResult("cba")(builder.result)
  }
}
