package nutcracker

import nutcracker.Rel.{Rel1, Rel2, Rel3}
import org.scalatest.{Matchers, FlatSpec}
import shapeless.test.illTyped
import shapeless.{HNil, ::}

import scalaz.{IList, NonEmptyList}

class PatternTest extends FlatSpec with Matchers {

  type ISBISB = Int :: String :: Boolean :: Int :: String :: Boolean :: HNil

  object R_B extends Rel1[Boolean]
  object R_II extends Rel2[Int, Int]
  object R_IIB extends Rel3[Int, Int, Boolean]
  object R_IS extends Rel2[Int, String]

  "Disconnected pattern" should "throw an exception" in {
    a [IllegalArgumentException] should be thrownBy {
      Pattern[ISBISB].build({ case i1 :: s1 :: b1 :: i2 :: s2 :: b2 :: HNil => NonEmptyList(
        R_II(i1, i1),
        R_IS(i2, s2)
      ) })
    }
  }

  "Pattern (0, 3), (3, 4), (0, 3, 5), (5)" should "have vertex set {0, 3, 4, 5}" in {
    val p = Pattern[ISBISB].build({ case i1 :: s1 :: b1 :: i2 :: s2 :: b2 :: HNil => NonEmptyList(
      R_II(i1, i2),
      R_IS(i2, s2),
      R_IIB(i1, i2, b2),
      R_B(b2)
    ) })
    p.vertexSet should be (Set(0, 3, 4, 5))
  }

  "Trying to build ill-typed pattern" should "not typecheck" in {
    illTyped("""
      Pattern[ISBISB].build({ case i1 :: s1 :: b1 :: i2 :: s2 :: b2 :: HNil => NonEmptyList(
        R_II(i1 :: s1 :: HNil)
      ) })
    """)
  }


  object p extends Rel2[Int, Int]
  object q extends Rel2[Int, Int]
  val pat = Pattern[Int::Int::Int::Int::Int::Int::Int::Int::HNil].build({ case a::b::c::d::e::f::g::h::HNil => NonEmptyList(
    p(a, b), p(b, c), q(c, d), q(d, e), q(e, f), q(f, g), q(g, h), q(h, a)
  ) })

  "Orienting { p(a, b), p(b, c), q(c, d), q(d, e), q(e, f), q(f, g), q(g, h), q(h, a) } towards p" should "yield two results" in {
    pat.orient(p).size should be (2)
  }

  it should "preserve the set of predicates" in {
    pat.orient(p) foreach {
      case (r, rs) => (rs.toSet + r) should be (pat.relations.toSet)
    }
  }

  it should "produce results whose every prefix is connected" in {
    pat.orient(p) foreach {
      case (r, rs) => NonEmptyList(r, rs:_*).reverse.tails.init.toList.foreach {
        case NonEmptyList(h, t) => (h.vertexSet intersect t.foldLeft(Set[Int]())((acc, rel) => acc union rel.vertexSet)) should not be (Set.empty)
      }
    }
  }
}
