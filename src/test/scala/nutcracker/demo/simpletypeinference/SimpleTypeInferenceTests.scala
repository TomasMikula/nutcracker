package nutcracker.demo.simpletypeinference

import nutcracker.demo.simpletypeinference.SimpleTypeInference.reconstructTypes
import nutcracker.demo.simpletypeinference.ast.Fun
import nutcracker.demo.simpletypeinference.types._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Inside

class SimpleTypeInferenceTests extends AnyFunSuite with Inside {

  def eitherBimap[A, B, C, D](f: Fun[A, C], g: Fun[B, D]): Fun[Either[A, B], Either[C, D]] =
    Fun.either(
      f > Fun.injectL,
      g > Fun.injectR,
    )

  type InfiniteList[A] = Fix[(A, *)]
  object InfiniteList {
    def map[A, B](f: Fun[A, B]): Fun[InfiniteList[A], InfiniteList[B]] =
      Fun.rec { map =>
        Fun.unfix[(A, *)] > Fun.par(f, map)> Fun.fix[(B, *)]
      }
  }

  type ListF[A, X] = Either[Unit, (A, X)]
  type List[A] = Fix[ListF[A, *]]
  object List {
    def map[A, B](f: Fun[A, B]): Fun[List[A], List[B]] =
      Fun.rec { map =>
        Fun.unfix[ListF[A, *]] > Fun.either(
          Fun.injectL[Unit, (B, List[B])],
          Fun.par(f, map) > Fun.injectR[Unit, (B, List[B])]
        ) > Fun.fix[ListF[B, *]]
      }
  }

  def infiniteListType(elemType: Type): Type =
    FixType(ProductType(elemType))

  def listType(elemType: Type): Type =
    FixType(
      SumType(UnitType()) ∘ ProductType(elemType)
    )

  test("infer types of eitherBimap(intToString, intToString)") {
    val (tIn, tOut) = reconstructTypes[Either[Int, Int], Either[String, String]](eitherBimap(Fun.intToString, Fun.intToString))

    assert(tIn  == SumType(IntType(), IntType()))
    assert(tOut == SumType(StringType(), StringType()))
  }

  test("infer types of InfiniteList.map(intToString)") {
    val (tIn, tOut) = reconstructTypes[InfiniteList[Int], InfiniteList[String]](InfiniteList.map(Fun.intToString))

    assert(tIn  == infiniteListType(IntType()))
    assert(tOut == infiniteListType(StringType()))
  }

  test("infer types of List.map(intToString)") {
    val (tIn, tOut) = reconstructTypes[List[Int], List[String]](List.map(Fun.intToString))

    inside(tIn) {
      case FixType(ComposedConstructors(TypeApp1(SumTypeT(), TypeVar(aliases1)), TypeApp1(ProductTypeT(), IntType()))) =>
        // note that the Nil case is inferred as a type var  ^^^^^^^^^^^^^^^^^^^^^  instead of Unit, since List.map really is polymorphic in it
        inside(tOut) {
          case FixType(ComposedConstructors(TypeApp1(SumTypeT(), TypeVar(aliases2)), TypeApp1(ProductTypeT(), StringType()))) =>
            assert(aliases1 == aliases2)
        }
    }
  }
}
