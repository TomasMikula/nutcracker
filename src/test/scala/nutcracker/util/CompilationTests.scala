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
    trait QuuxS[X, K[_]]

    type QuxL[K[_], A] = QuuxL[Int, K, A]
    type QuxS[K[_]]    = QuuxS[Int, K]

    type FooBarQuxL[K[_], A] = (FooL :+: BarL :++: QuxL)#Out[K, A]
    type FooBarQuxS[K[_]]    = (FooS :*: BarS :**: QuxS)#Out[K]

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

    implicitly[Lens[FooBarQuxS[List], FooS[List]]]
    implicitly[Lens[FooBarQuxS[List], BarS[List]]]
    implicitly[Lens[FooBarQuxS[List], QuxS[List]]]

    implicitly[`Forall{(* -> *) -> *}`[λ[K[_] => Lens[FooBarQuxS[K], FooS[K]]]]]
    implicitly[`Forall{(* -> *) -> *}`[λ[K[_] => Lens[FooBarQuxS[K], BarS[K]]]]]
    implicitly[`Forall{(* -> *) -> *}`[λ[K[_] => Lens[FooBarQuxS[K], QuxS[K]]]]]

    ()
  }
}
