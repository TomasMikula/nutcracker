package nutcracker.util

import org.scalatest.funsuite.AnyFunSuite
import shapeless.test.illTyped

object KMapTest {
  final case class Foo[A]()
  final case class Bar[A, B, C]()

  trait Typeclass[A, B, C]

  implicit val tc1: Typeclass[Int, Unit, Unit] = new Typeclass[Int, Unit, Unit]{}
}

class KMapTest extends AnyFunSuite {
  import KMapTest._

  test("KMap1_2 compilation test") {
    val m = KMap1_2[Foo, Bar, Typeclass]()

    val k1: Foo[Int] = Foo()
    val v1: Bar[Int, Unit, Unit] = Bar()

    m.get(k1): Option[Bar[Int, Unit, Unit]]

    val m1 = m.put(k1)(v1)
    val m2 = m1.updated(k1)(v1)((v1, v2) => v2)

    m2(k1): Bar[Int, Unit, Unit]


    val k2: Foo[String] = Foo()

    illTyped("""m.put(k2)(v1)""")
  }
}
