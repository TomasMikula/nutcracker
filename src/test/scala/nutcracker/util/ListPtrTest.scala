package nutcracker.util

import scala.language.existentials

import org.scalatest.FunSuite
import shapeless.Nat._
import shapeless.test.illTyped
import shapeless.{HList, HNil, ::}

class ListPtrTest extends FunSuite {

  type Pointer[L <: HList, A] = nutcracker.util.ListPtr.Aux[L, _, A]

  type ISB = Int :: String :: Boolean :: HNil

  test("Ptr creation examples should compile") {
    ListPtr[ISB, _0]: Pointer[ISB, Int]
    ListPtr[ISB, _1]: Pointer[ISB, String]
    ListPtr[ISB, _2]: Pointer[ISB, Boolean]

    ListPtr(_0): Pointer[ISB, Int]
    ListPtr(_1): Pointer[ISB, String]
    ListPtr(_2): Pointer[ISB, Boolean]
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
