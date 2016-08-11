package nutcracker.util

import scala.language.existentials

import org.scalatest.FunSuite
import shapeless.Nat._
import shapeless.test.illTyped
import shapeless.{HList, HNil, ::}

class PtrTest extends FunSuite {

  type Pointer[L <: HList, A] = nutcracker.util.Ptr.Aux[L, _, A]

  type ISB = Int :: String :: Boolean :: HNil

  test("Ptr creation examples should compile") {
    Ptr[ISB, _0]: Pointer[ISB, Int]
    Ptr[ISB, _1]: Pointer[ISB, String]
    Ptr[ISB, _2]: Pointer[ISB, Boolean]

    Ptr(_0): Pointer[ISB, Int]
    Ptr(_1): Pointer[ISB, String]
    Ptr(_2): Pointer[ISB, Boolean]
    ()
  }

  private def eval(testFun: => Unit): Unit = testFun

  eval { illTyped("""Ptr[ISB, _0]: Pointer[ISB, Boolean]""") }
  eval { illTyped("""Ptr[ISB, _1]: Pointer[ISB, Int]""") }
  eval { illTyped("""Ptr[ISB, _2]: Pointer[ISB, String]""") }

  eval { illTyped("""Ptr(_0): Pointer[ISB, String]""") }
  eval { illTyped("""Ptr(_1): Pointer[ISB, Boolean]""") }
  eval { illTyped("""Ptr(_2): Pointer[ISB, Int]""") }

}
