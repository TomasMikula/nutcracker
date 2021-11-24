package nutcracker.util

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import nutcracker.util.HList.{HNil, ::}
import nutcracker.util.Nat._

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

  test("ill-typed expressions should not typecheck") {
    """HListPtr[ISB, _0]: Pointer[ISB, Boolean]""" shouldNot typeCheck
    """HListPtr[ISB, _1]: Pointer[ISB, Int]""" shouldNot typeCheck
    """HListPtr[ISB, _2]: Pointer[ISB, String]""" shouldNot typeCheck
    """HListPtr(_0): Pointer[ISB, String]""" shouldNot typeCheck
    """HListPtr(_1): Pointer[ISB, Boolean]""" shouldNot typeCheck
    """HListPtr(_2): Pointer[ISB, Int]""" shouldNot typeCheck
  }
}
