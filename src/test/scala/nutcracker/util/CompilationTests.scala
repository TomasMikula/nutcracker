package nutcracker.util

import scala.language.higherKinds
import scalaz.Lens

object CompilationTests {

  def f(): Unit = {
    import CoproductK._
    import KPair._

    trait FooL[K[_], A]
    trait BarL[K[_], A]
    trait BazL[K[_], A]

    trait FooS[K[_]]
    trait BarS[K[_]]
    trait BazS[K[_]]

    type FooBarBazL[K[_], A] = (FooL :+: BarL :++: BazL)#Out[K, A]
    type FooBarBazS[K[_]]    = (FooS :*: BarS :**: BazS)#Out[K]

    def fooStep[S[_[_]]](implicit lens: LensK[S, FooS]): Step[FooL, S] = ???
    def barStep[S[_[_]]](implicit lens: LensK[S, BarS]): Step[BarL, S] = ???
    def bazStep[S[_[_]]](implicit lens: LensK[S, BazS]): Step[BazL, S] = ???

    def fooIntr[S[_[_]]](implicit lens: LensK[S, FooS]): StateInterpreter[FooL, S] = ???
    def barIntr[S[_[_]]](implicit lens: LensK[S, BarS]): StateInterpreter[BarL, S] = ???
    def bazIntr[S[_[_]]](implicit lens: LensK[S, BazS]): StateInterpreter[BazL, S] = ???

    fooStep[FooBarBazS] :+: barStep[FooBarBazS] :+: bazStep[FooBarBazS]
    fooStep[FooBarBazS] :+: barStep[FooBarBazS] :+: bazIntr[FooBarBazS]
    fooStep[FooBarBazS] :+: barIntr[FooBarBazS] :+: bazStep[FooBarBazS]
    fooStep[FooBarBazS] :+: barIntr[FooBarBazS] :+: bazIntr[FooBarBazS]
    fooIntr[FooBarBazS] :+: barStep[FooBarBazS] :+: bazStep[FooBarBazS]
    fooIntr[FooBarBazS] :+: barStep[FooBarBazS] :+: bazIntr[FooBarBazS]
    fooIntr[FooBarBazS] :+: barIntr[FooBarBazS] :+: bazStep[FooBarBazS]
    fooIntr[FooBarBazS] :+: barIntr[FooBarBazS] :+: bazIntr[FooBarBazS]

    trait QuuxL[X, K[_], A]
    trait QuuxS[X, K[_]]

    type QuxL[K[_], A] = QuuxL[Int, K, A]
    type QuxS[K[_]]    = QuuxS[Int, K]

    type FooBarQuxL[K[_], A] = (FooL :+: BarL :++: QuxL)#Out[K, A]
    type FooBarQuxS[K[_]]    = (FooS :*: BarS :**: QuxS)#Out[K]

    def quxStep[S[_[_]]](implicit lens: LensK[S, QuxS]): Step[QuxL, S] = ???
    def quxIntr[S[_[_]]](implicit lens: LensK[S, QuxS]): StateInterpreter[QuxL, S] = ???

    fooStep[FooBarQuxS] :+: barStep[FooBarQuxS] :+: quxStep[FooBarQuxS]
    fooStep[FooBarQuxS] :+: barStep[FooBarQuxS] :+: quxIntr[FooBarQuxS]
    fooStep[FooBarQuxS] :+: barIntr[FooBarQuxS] :+: quxStep[FooBarQuxS]
    fooStep[FooBarQuxS] :+: barIntr[FooBarQuxS] :+: quxIntr[FooBarQuxS]
    fooIntr[FooBarQuxS] :+: barStep[FooBarQuxS] :+: quxStep[FooBarQuxS]
    fooIntr[FooBarQuxS] :+: barStep[FooBarQuxS] :+: quxIntr[FooBarQuxS]
    fooIntr[FooBarQuxS] :+: barIntr[FooBarQuxS] :+: quxStep[FooBarQuxS]
    fooIntr[FooBarQuxS] :+: barIntr[FooBarQuxS] :+: quxIntr[FooBarQuxS]

    implicitly[InjectK[FooL, FooBarQuxL]]
    implicitly[InjectK[BarL, FooBarQuxL]]
    implicitly[InjectK[QuxL, FooBarQuxL]]

    implicitly[Lens[FooBarQuxS[List], FooS[List]]]
    implicitly[Lens[FooBarQuxS[List], BarS[List]]]
    implicitly[Lens[FooBarQuxS[List], QuxS[List]]]

    implicitly[`Forall{(* -> *) -> *}`[λ[K[_] => Lens[FooBarQuxS[K], FooS[K]]]]]
    implicitly[`Forall{(* -> *) -> *}`[λ[K[_] => Lens[FooBarQuxS[K], BarS[K]]]]]
    implicitly[`Forall{(* -> *) -> *}`[λ[K[_] => Lens[FooBarQuxS[K], QuxS[K]]]]]

    ()
  }
}
