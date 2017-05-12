package nutcracker.util

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

    def fooIntr[K[_], S](implicit lens: Lens[S, FooS[K]]): StateInterpreter[K, FooL[K, ?], S] = ???
    def barIntr[K[_], S](implicit lens: Lens[S, BarS[K]]): StateInterpreter[K, BarL[K, ?], S] = ???
    def bazIntr[K[_], S](implicit lens: Lens[S, BazS[K]]): StateInterpreter[K, BazL[K, ?], S] = ???

    type Prg1[A] = FreeK[FooBarBazL, A]

    fooIntr[Prg1, FooBarBazS[Prg1]] :+: barIntr[Prg1, FooBarBazS[Prg1]] :+: bazIntr[Prg1, FooBarBazS[Prg1]]

    trait QuuxL[X, K[_], A]
    trait QuuxS[X, K[_]]

    type QuxL[K[_], A] = QuuxL[Int, K, A]
    type QuxS[K[_]]    = QuuxS[Int, K]

    type FooBarQuxL[K[_], A] = (FooL :+: BarL :++: QuxL)#Out[K, A]
    type FooBarQuxS[K[_]]    = (FooS :*: BarS :**: QuxS)#Out[K]

    type Prg2[A] = FreeK[FooBarQuxL, A]

    def quxIntr[K[_], S](implicit lens: Lens[S, QuxS[K]]): StateInterpreter[K, QuxL[K, ?], S] = ???

    fooIntr[Prg2, FooBarQuxS[Prg2]] :+: barIntr[Prg2, FooBarQuxS[Prg2]] :+: quxIntr[Prg2, FooBarQuxS[Prg2]]

    implicitly[Inject[FooL[Prg2, ?], FooBarQuxL[Prg2, ?]]]
    implicitly[Inject[BarL[Prg2, ?], FooBarQuxL[Prg2, ?]]]
    implicitly[Inject[QuxL[Prg2, ?], FooBarQuxL[Prg2, ?]]]

    implicitly[Lens[FooBarQuxS[List], FooS[List]]]
    implicitly[Lens[FooBarQuxS[List], BarS[List]]]
    implicitly[Lens[FooBarQuxS[List], QuxS[List]]]

    implicitly[LensK[FooBarQuxS, FooS]]
    implicitly[LensK[FooBarQuxS, BarS]]
    implicitly[LensK[FooBarQuxS, QuxS]]

    ()
  }
}
