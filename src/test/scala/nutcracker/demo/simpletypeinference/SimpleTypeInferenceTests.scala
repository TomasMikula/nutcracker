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
  object ListF {
    val typ: TypeFun[● × ●, ●] =
      TypeFun.pair > TypeFun.introFst(Typ.unit) > TypeFun.sum
  }

  type List[A] = Fix[ListF[A, *]]
  object List {
    val typ: TypeFun[●, ●] =
      TypeFun.pfix(ListF.typ)

    def map[A, B](f: Fun[A, B]): Fun[List[A], List[B]] =
      Fun.rec { map =>
        Fun.unfix[ListF[A, *]] > Fun.either(
          Fun.injectL[Unit, (B, List[B])],
          Fun.par(f, map) > Fun.injectR[Unit, (B, List[B])]
        ) > Fun.fix[ListF[B, *]]
      }
  }

  def infiniteListType(elemType: Typ): Typ =
    Typ.fix(TypeFun.pair1(elemType))

  def infiniteListType(elemType: Type): Type =
    FixType(ProductType(elemType))

  def listType(elemType: Typ): Typ =
    Typ.fix(
      TypeFun.sum1(Typ.unit) ∘ TypeFun.pair1(elemType)
    )

  def listType(elemType: Type): Type =
    FixType(
      SumType(UnitType()) ∘ ProductType(elemType)
    )

  test("infer types of eitherBimap(intToString, intToString)") {
    val (tIn, tOut) = reconstructTypes[Either[Int, Int], Either[String, String]](eitherBimap(Fun.intToString, Fun.intToString))

    assert(tIn  == Typ.sum(Typ.int, Typ.int))
    assert(tOut == Typ.sum(Typ.string, Typ.string))
  }

  test("infer types of InfiniteList.map(intToString)") {
    val (tIn, tOut) = reconstructTypes[InfiniteList[Int], InfiniteList[String]](InfiniteList.map(Fun.intToString))

    assert(tIn  == infiniteListType(Typ.int))
    assert(tOut == infiniteListType(Typ.string))
  }

  test("infer types of List.map(intToString)") {
    val (tIn, tOut) = reconstructTypes[List[Int], List[String]](List.map(Fun.intToString))

    ???
    // inside(tIn) {
    //   case FixType(Composed1(TypeApp1(SumTypeT(), TypeVar(aliases1)), TypeApp1(ProductTypeT(), IntType()))) =>
    //     // note that the Nil case is inferred as  ^^^^^^^^^^^^^^^^^^  instead of Unit, since List.map really is polymorphic in it
    //     inside(tOut) {
    //       case FixType(Composed1(TypeApp1(SumTypeT(), TypeVar(aliases2)), TypeApp1(ProductTypeT(), StringType()))) =>
    //         assert(aliases1 == aliases2)
    //     }
    // }
  }

  test("infer types of List.map(List.map(intToString))") {
    val (tIn, tOut) = reconstructTypes[List[List[Int]], List[List[String]]](List.map(List.map(Fun.intToString)))

    ???
    // inside(tIn) {
    //   case FixType(Composed1(TypeApp1(SumTypeT(), TypeVar(aliases1)), TypeApp1(ProductTypeT(), FixType(Composed1(TypeApp1(SumTypeT(), TypeVar(aliases2)), TypeApp1(ProductTypeT(), IntTypeT())))))) =>
    //     inside(tOut) {
    //       case FixType(Composed1(TypeApp1(SumTypeT(), TypeVar(aliases3)), TypeApp1(ProductTypeT(), FixType(Composed1(TypeApp1(SumTypeT(), TypeVar(aliases4)), TypeApp1(ProductTypeT(), StringTypeT())))))) =>
    //         assert(aliases1 == aliases3)
    //         assert(aliases2 == aliases4)
    //     }
    // }
  }

  test("infer types of countNils") {
    val countNils: Fun[Fix[List], Int] =
      Fun.rec { countNils =>
        Fun.unfix[List] > Fun.unfix[ListF[Fix[List], *]] > Fun.either(
          Fun.constInt(1),
          Fun.par(
            countNils,
            Fun.fix[List] > countNils,
          ) > Fun.addInts
        )
      }

    val (tIn, tOut) = reconstructTypes[Fix[List], Int](countNils)

    assert(tIn == IntType())
  }
}
