package nutcracker.util

import scala.language.existentials

import org.scalatest.funsuite.AnyFunSuite
import shapeless.Nat._
import shapeless.test.illTyped
import shapeless.{HList, HNil, ::}

class HListPtrTest extends AnyFunSuite {

  type Pointer[L <: HList, A] = nutcracker.util.HListPtr.Aux[L, _, A]

  type ISB = Int :: String :: Boolean :: HNil

  test("Ptr creation examples should compile") {
    HListPtr[ISB, _0]: Pointer[ISB, Int]
    HListPtr[ISB, _1]: Pointer[ISB, String]
    HListPtr[ISB, _2]: Pointer[ISB, Boolean]

    HListPtr(_0): Pointer[ISB, Int]
    HListPtr(_1): Pointer[ISB, String]
    HListPtr(_2): Pointer[ISB, Boolean]
    ()
  }

  private def eval(testFun: => Unit): Unit = testFun

  eval { illTyped("""ListPtr[ISB, _0]: Pointer[ISB, Boolean]""") }
  eval { illTyped("""ListPtr[ISB, _1]: Pointer[ISB, Int]""") }
  eval { illTyped("""ListPtr[ISB, _2]: Pointer[ISB, String]""") }

  eval { illTyped("""ListPtr(_0): Pointer[ISB, String]""") }
  eval { illTyped("""ListPtr(_1): Pointer[ISB, Boolean]""") }
  eval { illTyped("""ListPtr(_2): Pointer[ISB, Int]""") }

}
