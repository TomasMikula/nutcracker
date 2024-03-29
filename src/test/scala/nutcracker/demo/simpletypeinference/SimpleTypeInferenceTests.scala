package nutcracker.demo.simpletypeinference

import nutcracker.demo.simpletypeinference.SimpleTypeInference.reconstructTypes
import nutcracker.demo.simpletypeinference.ast.Fun
import nutcracker.demo.simpletypeinference.kinds._
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
    def tpe(elemType: Type): Type =
      Type.fix(TypeFun.pair1(elemType))

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

    def tpe: TypeFun[●, ●] =
      TypeTag.toTypeFun[List](summon[TypeTag[List]])

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

  type NonEmptyTreeF[A, X] = Either[A, (X, X)]
  object NonEmptyTreeF {
    given typeTag: TypeTag[NonEmptyTreeF] =
      TypeTag.composeSnd(TypeTag.sum, TypeTag.diag)

    given typeTag[A](using a: TypeTag[A]): TypeTag[NonEmptyTreeF[A, *]] =
      TypeTag.appFst(typeTag, a)
  }

  type NonEmptyTree[A] = Fix[NonEmptyTreeF[A, *]]
  object NonEmptyTree {
    given typeTag: TypeTag[NonEmptyTree] =
      TypeTag.pfix[NonEmptyTreeF](using NonEmptyTreeF.typeTag)

    def tpe: TypeFun[●, ●] =
      TypeTag.toTypeFun(typeTag)

    def map[A, B](f: Fun[A, B]): Fun[NonEmptyTree[A], NonEmptyTree[B]] = {
      given TypeTag[A] = TypeTag.ofTypeParam[A]
      given TypeTag[B] = TypeTag.ofTypeParam[B]

      Fun.rec { map =>
        Fun.unfix[NonEmptyTreeF[A, *]](using NonEmptyTreeF.typeTag[A]) > Fun.either(
          f > Fun.injectL,
          Fun.par(map, map) > Fun.injectR,
        ) > Fun.fix[NonEmptyTreeF[B, *]](using NonEmptyTreeF.typeTag[B])
      }
    }
  }

  type Tree[A] = Either[Unit, NonEmptyTree[A]]
  object Tree {
    given typeTag: TypeTag[Tree] =
      TypeTag.compose(
        TypeTag.sum1[Unit],
        NonEmptyTree.typeTag,
      )

    def tpe: TypeFun[●, ●] =
      TypeTag.toTypeFun[Tree](typeTag)

    def map[A, B](f: Fun[A, B]): Fun[Tree[A], Tree[B]] = {
      Fun.either(
        Fun.injectL[Unit, NonEmptyTree[B]],
        NonEmptyTree.map(f) > Fun.injectR[Unit, NonEmptyTree[B]],
      )
    }
  }

  trait Bifunctor[F[_, _]] {
    def lmap[A, B, C](f: Fun[A, C]): Fun[F[A, B], F[C, B]]
    def rmap[A, B, D](g: Fun[B, D]): Fun[F[A, B], F[A, D]]

    def bimap[A, B, C, D](f: Fun[A, C], g: Fun[B, D]): Fun[F[A, B], F[C, D]] =
      lmap(f) > rmap(g)
  }

  object Bifunctor {
    given Bifunctor[Tuple2] with {
      override def lmap[A, B, C](f: Fun[A, C]): Fun[(A, B), (C, B)] =
        Fun.par(f, Fun.id[B])

      override def rmap[A, B, D](g: Fun[B, D]): Fun[(A, B), (A, D)] =
        Fun.par(Fun.id[A], g)
    }
  }

  type Const[A, B] = A
  object Const {
    given bifunctorConst: Bifunctor[Const] with {
      override def lmap[A, B, C](f: Fun[A, C]): Fun[Const[A, B], Const[C, B]] =
        f

      override def rmap[A, B, D](g: Fun[B, D]): Fun[Const[A, B], Const[A, D]] =
        Fun.id[A]
    }
  }

  type Swap[F[_, _], A, B] = F[B, A]
  object Swap {
    given bifunctorSwap[F[_, _]](using F: Bifunctor[F]): Bifunctor[Swap[F, *, *]] with {
      override def lmap[A, B, C](f: Fun[A, C]): Fun[Swap[F, A, B], Swap[F, C, B]] =
        F.rmap(f)

      override def rmap[A, B, D](g: Fun[B, D]): Fun[Swap[F, A, B], Swap[F, A, D]] =
        F.lmap(g)
    }
  }

  test("infer types of eitherBimap(intToString, intToString)") {
    val (tIn, tOut) = reconstructTypes[Either[Int, Int], Either[String, String]](eitherBimap(Fun.intToString, Fun.intToString))

    assert(tIn  == Type.sum(Type.int, Type.int))
    assert(tOut == Type.sum(Type.string, Type.string))
  }

  test("infer types of InfiniteList.map(intToString)") {
    val (tIn, tOut) = reconstructTypes[InfiniteList[Int], InfiniteList[String]](InfiniteList.map(Fun.intToString))

    assert(tIn  == InfiniteList.tpe(Type.int))
    assert(tOut == InfiniteList.tpe(Type.string))
  }

  test("infer types of List.map(intToString)") {
    val (tIn, tOut) = reconstructTypes[List[Int], List[String]](List.map(Fun.intToString))

    assert(tIn == List.tpe(Type.int))
    assert(tOut == List.tpe(Type.string))
  }

  test("infer types of List.map(List.map(intToString))") {
    val (tIn, tOut) = reconstructTypes[List[List[Int]], List[List[String]]](List.map(List.map(Fun.intToString)))

    assert(tIn == List.tpe(List.tpe(Type.int)))
    assert(tOut == List.tpe(List.tpe(Type.string)))
  }

  test("infer types of nested Fix types: countNils") {
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

    assert(tIn == Type.fix(List.tpe))
  }

  test("infer types of NonEmptyTree.map(intToString)") {
    val (tIn, tOut) =
      reconstructTypes[NonEmptyTree[Int], NonEmptyTree[String]](
        NonEmptyTree.map(Fun.intToString)
      )

    assert(tIn == NonEmptyTree.tpe(Type.int))
    assert(tOut == NonEmptyTree.tpe(Type.string))
  }

  test("infer types of Bifunctor[[x, y] =>> (Const[x, y], (y, x))].bimap") {
    type F[X, Y] = (Const[X, Y], (Y, X))

    object F {
      def lmap[A, B, C](f: Fun[A, C]): Fun[F[A, B], F[C, B]] =
        Fun.par(
          Const.bifunctorConst.lmap(f),
          Swap.bifunctorSwap[(*, *)].lmap(f),
        )

      def rmap[A, B, D](g: Fun[B, D]): Fun[F[A, B], F[A, D]] =
        Fun.par(
          Const.bifunctorConst.rmap(g),
          Swap.bifunctorSwap[(*, *)].rmap(g)
        )

      def bimap[A, B, C, D](f: Fun[A, C], g: Fun[B, D]): Fun[F[A, B], F[C, D]] =
        lmap(f) > rmap(g)

      val tpe: TypeFun[● × ●, ●] =
        TypeFun(
          Routing.par(
            Routing.dup[●],
            Routing.dup[●],
          ) > Routing.ixi > Routing.par(
            Routing.elimSnd,
            Routing.swap,
          ): Routing[● × ●, ● × (● × ●)],
          TypeExpr.composeSnd(TypeExpr.pair, TypeExpr.pair),
        )

      def tpeAt(a: Type, b: Type): Type =
        TypeFun.appFst(tpe, TypeFun.fromExpr(a)).apply(b)
    }

    val f: Fun[F[Unit, Int], F[Int, String]] =
      F.bimap(Fun.constInt(0), Fun.intToString)

    val (tIn, tOut) = reconstructTypes(f)

    assert(tIn == F.tpeAt(Type.unit, Type.int))
    assert(tOut == F.tpeAt(Type.int, Type.string))
  }
}
