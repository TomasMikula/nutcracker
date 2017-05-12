package nutcracker.rel

import nutcracker.Assignment
import org.scalatest.{FlatSpec, Matchers}
import shapeless.{::, HNil}
import shapeless.nat._

class AssignmentTest extends FlatSpec with Matchers {

  type ISB = Int :: String :: Boolean :: HNil

  "An empty assignment" should "be empty" in {
    Assignment[ISB].empty.isEmpty should be (true)
  }

  it should "not be complete" in {
    Assignment[ISB].empty.getIfComplete should be (None)
  }

  it should "match any valuation" in {
    Assignment[ISB].empty.matches(0::"foo"::false::HNil) should be (true)
    Assignment[ISB].empty.matchesAt(_1 :: HNil)("bar" :: HNil) should be (true)
  }


  "A complete assignment" should "be complete" in {
    Assignment[ISB].from(Option(1) :: Option("foo") :: Option(true) :: HNil).getIfComplete should be (Some(1 :: "foo" :: true :: HNil))
  }

  it should "not be empty" in {
    Assignment[ISB].from(Option(1) :: Option("foo") :: Option(true) :: HNil).isEmpty should be (false)
  }

  it should "match its full value" in {
    Assignment[ISB].from(Option(1) :: Option("foo") :: Option(true) :: HNil).matches(1 :: "foo" :: true :: HNil) should be (true)
  }

  it should "match any sub-value" in {
    Assignment[ISB].from(Option(1) :: Option("foo") :: Option(true) :: HNil).matchesAt(_2 :: _0 :: _0 :: HNil)(true :: 1 :: 1 :: HNil) should be (true)
  }

  it should "not match an incompatible value or sub-value" in {
    Assignment[ISB].from(Option(1) :: Option("foo") :: Option(true) :: HNil).matches(1 :: "foo" :: false :: HNil) should be (false)
    Assignment[ISB].from(Option(1) :: Option("foo") :: Option(true) :: HNil).matchesAt(_1 :: HNil)("Foo" :: HNil) should be (false)
  }


  "(1, 'foo', _)" should "be extensible by (1, _, false)" in {
    val asg0 = Assignment[ISB].empty.set(_0)(1).set(_1)("foo")
    val asg1 = asg0.extend(_0 :: _2 :: HNil)(1 :: false :: HNil)
    asg1.isDefined should be (true)
    asg1.get.getIfComplete should be (Some(1 :: "foo" :: false :: HNil))
  }

}
