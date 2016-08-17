package nutcracker.util

import nutcracker.{DecSet, Dom, DRef}
import org.scalatest.FunSuite
import shapeless.test.illTyped

class KMapTest extends FunSuite {
  test("KMap1_2 compilation test") {
    val m = KMap1_2[DRef, DRef.Aux, Dom.Aux]()

    val ref: DecSet.DecSetRef[Int] = DRef[DecSet[Int]](0)

    assert(m.get(ref) == None)
    assert(m.getOrElse(ref)(ref) == ref)

    val m1 = m.put(ref)(ref)
    val m2 = m1.updated(ref)(ref)((r1, r2) => r2)

    assert(m2(ref) == ref)

    val ref2: DecSet.DecSetRef[String] = DRef[DecSet[String]](0)

    illTyped("""m.put(ref)(ref2)""")
  }
}
