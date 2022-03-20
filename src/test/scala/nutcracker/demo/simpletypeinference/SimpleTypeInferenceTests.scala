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
    def map[A, B](f: Fun[A, B]): Fun[InfiniteList[A], InfiniteList[B]] = {
      given TypeTag[A] = TypeTag.ofTypeParam[A]
      given TypeTag[B] = TypeTag.ofTypeParam[B]

      Fun.rec { map =>
        Fun.unfix[(A, *)] > Fun.par(f, map)> Fun.fix[(B, *)]
      }
    }
  }

  type ListF[A, X] = Either[Unit, (A, X)]
  object ListF {
    given typeTag: TypeTag[ListF] =
      TypeTag.compose2(TypeTag[Either[Unit, *]], TypeTag.pair)

    given typeTag[A](using TypeTag[A]): TypeTag[ListF[A, *]] =
      TypeTag.compose(TypeTag[Either[Unit, *]], TypeTag[(A, *)])
  }

  type List[A] = Fix[ListF[A, *]]
  object List {
    given TypeTag[List] =
      TypeTag.pfix[ListF](using ListF.typeTag)

    def tpe: TpeFun[●, ●] =
      TypeTag.toTpeFun[List](summon[TypeTag[List]])

    def map[A, B](f: Fun[A, B]): Fun[List[A], List[B]] = {
      given TypeTag[A] = TypeTag.ofTypeParam[A]
      given TypeTag[B] = TypeTag.ofTypeParam[B]

      Fun.rec { map =>
        Fun.unfix[ListF[A, *]](using ListF.typeTag[A]) > Fun.either(
          Fun.injectL[Unit, (B, List[B])],
          Fun.par(f, map) > Fun.injectR[Unit, (B, List[B])]
        ) > Fun.fix[ListF[B, *]](using ListF.typeTag[B])
      }
    }
  }

  def infiniteListType(elemType: Tpe): Tpe =
    Tpe.fix(TpeFun.pair1(elemType))

  def listType(elemType: Tpe): Tpe =
    Tpe.fix(
      TpeFun.sum1(Tpe.unit) ∘ TpeFun.pair1(elemType)
    )

  test("infer types of eitherBimap(intToString, intToString)") {
    val (tIn, tOut) = reconstructTypes[Either[Int, Int], Either[String, String]](eitherBimap(Fun.intToString, Fun.intToString))

    assert(tIn  == Tpe.sum(Tpe.int, Tpe.int))
    assert(tOut == Tpe.sum(Tpe.string, Tpe.string))
  }

  test("infer types of InfiniteList.map(intToString)") {
    val (tIn, tOut) = reconstructTypes[InfiniteList[Int], InfiniteList[String]](InfiniteList.map(Fun.intToString))

    assert(tIn  == infiniteListType(Tpe.int))
    assert(tOut == infiniteListType(Tpe.string))
  }

  test("infer types of List.map(intToString)") {
    val (tIn, tOut) = reconstructTypes[List[Int], List[String]](List.map(Fun.intToString))

    assert(tIn == listType(Tpe.int))
    assert(tOut == listType(Tpe.string))
  }

  test("infer types of List.map(List.map(intToString))") {
    val (tIn, tOut) = reconstructTypes[List[List[Int]], List[List[String]]](List.map(List.map(Fun.intToString)))

    assert(tIn == listType(listType(Tpe.int)))
    assert(tOut == listType(listType(Tpe.string)))
  }

  test("infer types of countNils") {
    import List.{given TypeTag[List]}

    val countNils: Fun[Fix[List], Int] =
      Fun.rec { countNils =>
        Fun.unfix[List] > Fun.unfix[ListF[Fix[List], *]](using ListF.typeTag[Fix[List]]) > Fun.either(
          Fun.constInt(1),
          Fun.par(
            countNils,
            Fun.fix[List] > countNils,
          ) > Fun.addInts
        )
      }

    val (tIn, tOut) = reconstructTypes[Fix[List], Int](countNils)

    assert(tIn == Tpe.fix(List.tpe))
  }
}
