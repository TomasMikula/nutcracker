package nutcracker.util

import scala.language.existentials

import org.scalatest.FlatSpec
import shapeless.Nat._
import shapeless.test.illTyped
import shapeless.{HList, HNil, ::}

class PtrTest extends FlatSpec {

  type Pointer[L <: HList, A] = nutcracker.util.Ptr.Aux[L, _, A]

  type ISB = Int :: String :: Boolean :: HNil

  "Ptr creation examples" should "compile" in {
    Ptr[ISB, _0]: Pointer[ISB, Int]
    Ptr[ISB, _1]: Pointer[ISB, String]
    Ptr[ISB, _2]: Pointer[ISB, Boolean]

    Ptr(_0): Pointer[ISB, Int]
    Ptr(_1): Pointer[ISB, String]
    Ptr(_2): Pointer[ISB, Boolean]
  }

  "Ill-typed examples" should "not compile" in {
    illTyped("""Ptr[ISB, _0]: Pointer[ISB, Boolean]""")
    illTyped("""Ptr[ISB, _1]: Pointer[ISB, Int]""")
    illTyped("""Ptr[ISB, _2]: Pointer[ISB, String]""")

    illTyped("""Ptr(_0): Pointer[ISB, String]""")
    illTyped("""Ptr(_1): Pointer[ISB, Boolean]""")
    illTyped("""Ptr(_2): Pointer[ISB, Int]""")
  }
}
