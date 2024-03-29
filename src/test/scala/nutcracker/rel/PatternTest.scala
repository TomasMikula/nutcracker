package nutcracker.rel

import nutcracker.Pattern
import nutcracker.Rel.{Rel1, Rel2, Rel3}
import nutcracker.util.HList.{::, HNil}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import scalaz.{IList, NonEmptyList}

class PatternTest extends AnyFlatSpec {

  type ISBISB = Int :: String :: Boolean :: Int :: String :: Boolean :: HNil

  object R_B extends Rel1[Boolean]
  object R_II extends Rel2[Int, Int]
  object R_IIB extends Rel3[Int, Int, Boolean]
  object R_IS extends Rel2[Int, String]

  "Disconnected pattern" should "throw an exception" in {
    a [IllegalArgumentException] should be thrownBy {
      Pattern
        .on[Int][String][Boolean][Int][String][Boolean]
        .build { case i1 :: s1 :: b1 :: i2 :: s2 :: b2 :: HNil =>
          NonEmptyList(
            R_II(i1, i1),
            R_IS(i2, s2)
          )
        }
    }
  }

  "Pattern (0, 3), (3, 4), (0, 3, 5), (5)" should "have vertex set {0, 3, 4, 5}" in {
    val p = Pattern
      .on[Int][String][Boolean][Int][String][Boolean]
      .build { case i1 :: s1 :: b1 :: i2 :: s2 :: b2 :: HNil =>
        NonEmptyList(
          R_II(i1, i2),
          R_IS(i2, s2),
          R_IIB(i1, i2, b2),
          R_B(b2)
        )
      }
    p.vertexSet should be (Set(0, 3, 4, 5))
  }

  "Trying to build ill-typed pattern" should "not typecheck" in {
    """
      Pattern
        .on[Int][String][Boolean][Int][String][Boolean]
        .build { case i1 :: s1 :: b1 :: i2 :: s2 :: b2 :: HNil =>
          NonEmptyList(R_II(i1 :: s1 :: HNil))
        }
    """ shouldNot typeCheck
  }


  object p extends Rel2[Int, Int]
  object q extends Rel2[Int, Int]
  val pat = Pattern
    .on[Int][Int][Int][Int][Int][Int][Int][Int]
    .build { case a::b::c::d::e::f::g::h::HNil =>
      NonEmptyList(
        p(a, b), p(b, c), q(c, d), q(d, e), q(e, f), q(f, g), q(g, h), q(h, a)
      )
    }
  val ppat = pat.orient(p)

  "Orienting { p(a, b), p(b, c), q(c, d), q(d, e), q(e, f), q(f, g), q(g, h), q(h, a) } towards p" should "yield two results" in {
    ppat.orientations.size should be (2)
  }

  it should "preserve the set of predicates" in {
    ppat.orientations foreach {
      case (r, rs) => (rs.toSet + r) should be (pat.relations.toSet)
    }
  }

  it should "produce results whose every prefix is connected" in {
    ppat.orientations foreach {
      case (r, rs) => NonEmptyList.nel(r, IList.fromList(rs)).reverse.tails.init.toList.foreach {
        case NonEmptyList(h, t) => (h.vertexSet intersect t.foldLeft(Set[Int]())((acc, rel) => acc union rel.vertexSet)) should not be (Set.empty)
      }
    }
  }

  "Two patterns with different order of relations" should "be viewed as equal" in {
    val pat1 = Pattern.on[Int][Int][Int][Int][Int][Int][Int][Int].build({ case a::b::c::d::e::f::g::h::HNil => NonEmptyList(
      p(a, b), p(b, c), q(c, d), q(d, e), q(e, f), q(f, g), q(g, h), q(h, a)
    ) })
    val pat2 = Pattern.on[Int][Int][Int][Int][Int][Int][Int][Int].build({ case a::b::c::d::e::f::g::h::HNil => NonEmptyList(
      q(g, h), q(h, a), p(a, b), q(f, g), p(b, c), q(e, f), q(c, d), q(d, e)
    ) })

    pat1 should be (pat2)
  }
}
