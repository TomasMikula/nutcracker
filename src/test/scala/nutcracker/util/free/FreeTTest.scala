package nutcracker.util.free

import scala.language.higherKinds
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll

import scalaz.{Applicative, ApplicativePlus, Bind, BindRec, Equal, Foldable, Functor, Monad, MonadPlus, MonadTrans, NaturalTransformation, Plus, Traverse, \/, ~>}
import scalaz.std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.syntax.monad._

class FreeTTest extends TestSuite {
  type FreeTList[M[_], A] = FreeT[List, M, A]
  type FreeTListOption[A] = FreeTList[Option, A]

  implicit def freeTListOptionArb[A](implicit A: Arbitrary[A]): Arbitrary[FreeTListOption[A]] =
    Arbitrary(freeTGen[List, Option, A](
      Gen.choose(0, 2).flatMap(Gen.listOfN(_, freeTListOptionArb[A].arbitrary))
    ))

  val headOpt = λ[List ~> Option](_.headOption)
  implicit def freeTListOptionEq[A](implicit A: Equal[A]): Equal[FreeTListOption[A]] = new Equal[FreeTListOption[A]] {
    def equal(a: FreeTListOption[A], b: FreeTListOption[A]) =
      Equal[Option[A]].equal(a.foldMap(headOpt), b.foldMap(headOpt))
  }

  def freeTGen[F[_], G[_], A](g: Gen[F[FreeT[F, G, A]]])(implicit F: Functor[F], G: Applicative[G], A: Arbitrary[A]): Gen[FreeT[F, G, A]] =
    Gen.frequency(
      (1, Functor[Arbitrary].map(A)(FreeT.point[F, G, A](_)).arbitrary),
      (1, Functor[Arbitrary].map(Arbitrary(g))(FreeT.liftF[F, G, FreeT[F, G, A]](_).flatMap(x => x)).arbitrary)
    )

  object headOption extends (List ~> Option) {
    def apply[A](l: List[A]): Option[A] = l.headOption
  }

  "ListOption" should {
    checkAll(monadPlus.laws[FreeTListOption])
    checkAll(traverse.laws[FreeTListOption])
    checkAll(monadTrans.laws[FreeTList, Option])

    test("not stack overflow with 50k binds") {
      val expected = Applicative[FreeTListOption].point(())
      val result =
        BindRec[FreeTListOption].tailrecM(0)(i =>
          if (i < 50000)
            Applicative[FreeTListOption].point(\/.left[Int, Unit](i + 1))
          else
            Applicative[FreeTListOption].point(\/.right[Int, Unit](()))
        )

      Equal[FreeTListOption[Unit]].equal(expected, result)
    }

    test("not stack overflow with 50k left-associated binds") {
      val expected = Applicative[FreeTListOption].point(())
      val result =
        (0 until 50000).foldLeft(Applicative[FreeTListOption].point(()))(
          (fu, i) => fu.flatMap(u => Applicative[FreeTListOption].point(u))
        )

      Equal[FreeTListOption[Unit]].equal(expected, result)
    }

    test("not stack overflow with bind followed by 50k maps") {
      val expected = Applicative[FreeTListOption].point(())
      val result =
        (0 until 50000).foldLeft(().point[FreeTListOption].flatMap(u => u.point[FreeTListOption]))(
          (fu, i) => fu.map(u => u)
        )

      Equal[FreeTListOption[Unit]].equal(expected, result)
    }

    "hoist" ! forAll { a: FreeTListOption[Int] =>
      val b = a.hoist(NaturalTransformation.refl)
      Equal[FreeTListOption[Int]].equal(a, b)
    }

    test("hoist stack-safety") {
      val a = (0 until 50000).foldLeft(Applicative[FreeTListOption].point(()))(
        (fu, i) => fu.flatMap(u => Applicative[FreeTListOption].point(u))
      )

      val b = a.hoist(NaturalTransformation.refl) // used to overflow
    }

    "interpret" ! forAll { a: FreeTListOption[Int] =>
      val b = a.interpret(NaturalTransformation.refl)
      Equal[FreeTListOption[Int]].equal(a, b)
    }

    test("interpret stack-safety") {
      val a = (0 until 50000).foldLeft(Applicative[FreeTListOption].point(()))(
        (fu, i) => fu.flatMap(u => Applicative[FreeTListOption].point(u))
      )

      val b = a.interpret(NaturalTransformation.refl) // used to overflow
    }
  }

  test("#1156: equals should not return true for obviously unequal instances") {
    val a = FreeT.point[List, Option, Int](1).flatMap(x => FreeT.point(2))
    val b = FreeT.point[List, Option, Int](3).flatMap(x => FreeT.point(4))
    a != b
  }

  object instances {
    def bind[S[_]: Functor, F[_]: Applicative] = Bind[FreeT[S, F, ?]]
    def foldable[S[_]: Foldable: Functor, F[_]: Foldable: Applicative: BindRec] = Foldable[FreeT[S, F, ?]]
    def traverse[S[_]: Traverse, F[_]: Traverse: Applicative: BindRec] = Traverse[FreeT[S, F, ?]]
    def monad[S[_]: Functor, F[_]: Applicative] = Monad[FreeT[S, F, ?]]
    def plus[S[_]: Functor, F[_]: Applicative: BindRec: Plus] = Plus[FreeT[S, F, ?]]
    def monadPlus[S[_]: Functor, F[_]: ApplicativePlus: BindRec] = MonadPlus[FreeT[S, F, ?]]
    def monadTrans[S[_]: Functor] = MonadTrans[FreeT[S, ?[_], ?]]

    // checking absence of ambiguity
    def functor[S[_]: Traverse, F[_]: Traverse: Applicative: BindRec] = Functor[FreeT[S, F, ?]]
    def foldable[S[_]: Traverse, F[_]: Traverse: Applicative: BindRec] = Foldable[FreeT[S, F, ?]]
    def monad[S[_]: Functor, F[_]: ApplicativePlus: BindRec] = Monad[FreeT[S, F, ?]]
    def plus[S[_]: Functor, F[_]: ApplicativePlus: BindRec] = Plus[FreeT[S, F, ?]]
  }
}
