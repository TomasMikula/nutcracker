package nutcracker.util

import nutcracker.util.typealigned.{BoundedAPair}
import scala.language.higherKinds
import scalaz.{Applicative, Forall, PlusEmpty}
import shapeless._
import shapeless.poly.~>

trait Mapped[L <: HList, F[_]] extends Serializable { self =>
  import shapeless.poly.~>>

  type Out <: HList

  def extract(l: Out, f: F ~> Id): L
  def remap[G[_]](l: Out, f: F ~> G): BoundedAPair[HList, Id, Mapped.Aux[L, G, ?]]
  def toList(l: Out): List[F[_]]
  def toList[A](l: Out, f: F ~>> A): List[A]

  final def prepend[H]: Mapped.Aux[H :: L, F, F[H] :: Out] =
    new Mapped[H :: L, F] {
      type Out = F[H] :: self.Out
      def extract(l: Out, f: F ~> Id): H :: L = f(l.head) :: self.extract(l.tail, f)
      def remap[G[_]](l: Out, f: F ~> G): BoundedAPair[HList, Id, Mapped.Aux[H :: L, G, ?]] = {
        val tRes = self.remap(l.tail, f)
        BoundedAPair.of[HList, Id, Mapped.Aux[H :: L, G, ?]](f(l.head) :: tRes._1, tRes._2.prepend[H])
      }
      def toList(l: Out): List[F[_]] = l.head :: self.toList(l.tail)
      def toList[A](l: Out, f: F ~>> A): List[A] = f(l.head) :: self.toList(l.tail, f)
    }
}

object Mapped {
  import shapeless.poly.~>>

  type Aux[L <: HList, F[_], Out0 <: HList] = Mapped[L, F] { type Out = Out0 }

  def apply[L <: HList, F[_]](implicit mapped: Mapped[L, F]): Aux[L, F, mapped.Out] = mapped

  def pure[L <: HList, F[_]](l: L)(implicit F: Applicative[F]): BoundedAPair[HList, Id, Mapped.Aux[L, F, ?]] = {
    l match {
      case HNil => BoundedAPair.of[HList, Id, Mapped.Aux[L, F, ?]](HNil, hnilMapped[F].asInstanceOf[Mapped.Aux[L, F, HNil]])
      case shapeless.::(h, t) =>
        val ev = pure(t)(F)
        BoundedAPair.of[HList, Id, Mapped.Aux[L, F, ?]](F.point(h) :: ev._1, ev._2.prepend.asInstanceOf)
    }
  }

  def empty[L <: HList, F[_]](implicit F: PlusEmpty[F], L: MappedListBuilder[L]): BoundedAPair[HList, Id, Mapped.Aux[L, F, ?]] =
    L.supply(new Forall[F] { def apply[A]: F[A] = F.empty[A] })

  implicit def hnilMapped[F[_]]: Aux[HNil, F, HNil] = new Mapped[HNil, F] {
    type Out = HNil
    def extract(l: HNil, f: F ~> Id): HNil = HNil
    def remap[G[_]](l: HNil, f: F ~> G): BoundedAPair[HList, Id, Mapped.Aux[HNil, G, ?]] =
      BoundedAPair.of[HList, Id, Mapped.Aux[HNil, G, ?]](HNil, implicitly[Mapped.Aux[HNil, G, HNil]])
    def toList(l: HNil): List[F[_]] = Nil
    def toList[A](l: HNil, f: F ~>> A): List[A] = Nil
  }

  implicit def hconsMapped[H, T <: HList, F[_], OutM <: HList](implicit mt: Mapped.Aux[T, F, OutM]): Aux[H :: T, F, F[H] :: OutM] =
    mt.prepend[H]
}

trait MappedListBuilder[L <: HList] {
  def map[F[_]](l: L)(f: Id ~> F): BoundedAPair[HList, Id, Mapped.Aux[L, F, ?]]
  def supply[F[_]](f: Forall[F]): BoundedAPair[HList, Id, Mapped.Aux[L, F, ?]]
}

object MappedListBuilder {
  implicit def hnilBuilder: MappedListBuilder[HNil] = new MappedListBuilder[HNil] {
    def map[F[_]](l: HNil)(f: Id ~> F): BoundedAPair[HList, Id, Mapped.Aux[HNil, F, ?]] =
      BoundedAPair.of[HList, Id, Mapped.Aux[HNil, F, ?]](HNil, Mapped.hnilMapped[F])

    def supply[F[_]](f: Forall[F]): BoundedAPair[HList, Id, Mapped.Aux[HNil, F, ?]] =
      BoundedAPair.of[HList, Id, Mapped.Aux[HNil, F, ?]](HNil, Mapped.hnilMapped[F])
  }

  implicit def hconsBuilder[H, T <: HList](implicit T: MappedListBuilder[T]): MappedListBuilder[H :: T] = new MappedListBuilder[H :: T] {
    def map[F[_]](l: H :: T)(f: Id ~> F): BoundedAPair[HList, Id, Mapped.Aux[H :: T, F, ?]] = {
      val t = T.map(l.tail)(f)
      BoundedAPair.of[HList, Id, Mapped.Aux[H :: T, F, ?]](f(l.head) :: t._1, t._2.prepend[H])
    }

    def supply[F[_]](f: Forall[F]): BoundedAPair[HList, Id, Mapped.Aux[H :: T, F, ?]] = {
      val t = T.supply[F](f)
      BoundedAPair.of[HList, Id, Mapped.Aux[H :: T, F, ?]](f[H] :: t._1, t._2.prepend[H])
    }
  }
}