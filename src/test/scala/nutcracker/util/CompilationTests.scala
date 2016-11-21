package nutcracker.util

import scala.language.higherKinds
import monocle.Lens
import scalaz.{Lens => Lenz}

object CompilationTests {

  def f(): Unit = {
    import CoproductK._
    import KList._

    trait FooL[K[_], A]
    trait BarL[K[_], A]
    trait BazL[K[_], A]

    trait FooS[K]
    trait BarS[K]
    trait BazS[K]

    type FooBarBazL[K[_], A] = (FooL :+: BarL :++: BazL)#Out[K, A]
    type FooBarBazS[K]       = (FooS :*: BarS :**: BazS)#Out[K]

    val fooStep: Step[FooL, FooS] = ???
    val barStep: Step[BarL, BarS] = ???
    val bazStep: Step[BazL, BazS] = ???

    val fooIntr: StateInterpreter[FooL, FooS] = ???
    val barIntr: StateInterpreter[BarL, BarS] = ???
    val bazIntr: StateInterpreter[BazL, BazS] = ???

    fooStep :&: barStep :&&: bazStep
    fooStep :&: barStep :&&: bazIntr
    fooStep :&: barIntr :&&: bazStep
    fooStep :&: barIntr :&&: bazIntr
    fooIntr :&: barStep :&&: bazStep
    fooIntr :&: barStep :&&: bazIntr
    fooIntr :&: barIntr :&&: bazStep
    fooIntr :&: barIntr :&&: bazIntr

    trait QuuxL[X, K[_], A]
    trait QuuxS[X, K]

    type QuxL[K[_], A] = QuuxL[Int, K, A]
    type QuxS[K] = QuuxS[Int, K]

    type FooBarQuxL[K[_], A] = (FooL :+: BarL :++: QuxL)#Out[K, A]
    type FooBarQuxS[K]       = (FooS :*: BarS :**: QuxS)#Out[K]

    val quxStep: Step[QuxL, QuxS] = ???
    val quxIntr: StateInterpreter[QuxL, QuxS] = ???

    fooStep :&: barStep :&&: quxStep
    fooStep :&: barStep :&&: quxIntr
    fooStep :&: barIntr :&&: quxStep
    fooStep :&: barIntr :&&: quxIntr
    fooIntr :&: barStep :&&: quxStep
    fooIntr :&: barStep :&&: quxIntr
    fooIntr :&: barIntr :&&: quxStep
    fooIntr :&: barIntr :&&: quxIntr

    implicitly[InjectK[FooL, FooBarQuxL]]
    implicitly[InjectK[BarL, FooBarQuxL]]
    implicitly[InjectK[QuxL, FooBarQuxL]]

    implicitly[Lens[FooBarQuxS[Int], FooS[Int]]]
    implicitly[Lens[FooBarQuxS[Int], BarS[Int]]]
    implicitly[Lens[FooBarQuxS[Int], QuxS[Int]]]

    implicitly[Lenz[FooBarQuxS[Int], FooS[Int]]]
    implicitly[Lenz[FooBarQuxS[Int], BarS[Int]]]
    implicitly[Lenz[FooBarQuxS[Int], QuxS[Int]]]

    implicitly[`Forall{* -> *}`[λ[K => Lenz[FooBarQuxS[K], FooS[K]]]]]
    implicitly[`Forall{* -> *}`[λ[K => Lenz[FooBarQuxS[K], BarS[K]]]]]
    implicitly[`Forall{* -> *}`[λ[K => Lenz[FooBarQuxS[K], QuxS[K]]]]]

    ()
  }
}
