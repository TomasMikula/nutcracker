package nutcracker.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import shapeless.{HNil, ::, HList}
import shapeless.nat._

class ChooseTest extends AnyFlatSpec {

  type Pointer[L <: HList, A] = nutcracker.util.HListPtr.Aux[L, _, A]

  type ISB = Int :: String :: Boolean :: HNil

  "Choose creation examples" should "compile" in {
    (_2 :: _0 :: _0 :: _1 :: Choose[ISB]): Choose[ISB,  Boolean :: Int :: Int :: String :: HNil]
    ()
  }

  "Choose instance" should " be applicable to an appropriate HList" in {
    (_2 :: _0 :: _0 :: _1 :: Choose[ISB]).apply(3 :: "foo" :: true :: HNil) should be (true :: 3 :: 3 :: "foo" :: HNil)
  }
}
