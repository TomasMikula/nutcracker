package nutcracker.util

import nutcracker.util.HList.{::, HNil}
import nutcracker.util.typealigned.BoundedAPair
import scalaz.{Applicative, Const, Forall, PlusEmpty, ~>}
import scalaz.Id.Id

trait Mapped[L <: HList, F[_]] extends Serializable { self =>
  import Mapped.~>>

  type Out <: HList

  def extract(l: Out, f: F ~> Id): L
  def remap[G[_]](l: Out, f: F ~> G): BoundedAPair[HList, Id, ({ type out[m <: HList] = Mapped.Aux[L, G, m] })#out]
  def toList(l: Out): List[Exists[F]]
  def toList[A](l: Out, f: F ~>> A): List[A]

  final def prepend[H]: Mapped.Aux[H :: L, F, F[H] :: Out] =
    new Mapped[H :: L, F] {
      type Out = F[H] :: self.Out
      def extract(l: Out, f: F ~> Id): H :: L = f(l.head) :: self.extract(l.tail, f)
      def remap[G[_]](l: Out, f: F ~> G): BoundedAPair[HList, Id, ({ type out[m <: HList] = Mapped.Aux[H :: L, G, m] })#out] = {
        val tRes = self.remap(l.tail, f)
        BoundedAPair.of[HList, Id, ({ type out[m <: HList] = Mapped.Aux[H :: L, G, m] })#out](f(l.head) :: tRes._1, tRes._2.prepend[H])
      }
      def toList(l: Out): List[Exists[F]] = Exists(l.head) :: self.toList(l.tail)
      def toList[A](l: Out, f: F ~>> A): List[A] = f(l.head).getConst :: self.toList(l.tail, f)
    }
}

object Mapped {
  type ~>>[G[_], B] = G ~> Const[B, *]

  type Aux[L <: HList, F[_], Out0 <: HList] = Mapped[L, F] { type Out = Out0 }

  def apply[L <: HList, F[_]](implicit mapped: Mapped[L, F]): Aux[L, F, mapped.Out] = mapped

  def pure[L <: HList, F[_]](l: L)(implicit F: Applicative[F]): BoundedAPair[HList, Id, ({ type out[m <: HList] = Mapped.Aux[L, F, m] })#out] = {
    l match {
      case HNil => BoundedAPair.of[HList, Id, ({ type out[m <: HList] = Mapped.Aux[L, F, m] })#out](HNil, hnilMapped[F].asInstanceOf[Mapped.Aux[L, F, HNil]])
      case h :: t =>
        val ev = pure(t)(F)
        BoundedAPair[HList, Id, ({ type out[m <: HList] = Mapped.Aux[L, F, m] })#out, HList](F.point(h) :: ev._1, ev._2.prepend.asInstanceOf[Mapped.Aux[L, F, HList]])
    }
  }

  def empty[L <: HList, F[_]](implicit F: PlusEmpty[F], L: MappedListBuilder[L]): BoundedAPair[HList, Id, ({ type out[m <: HList] = Mapped.Aux[L, F, m] })#out] =
    L.supply(new Forall[F] { def apply[A]: F[A] = F.empty[A] })

  implicit def hnilMapped[F[_]]: Aux[HNil, F, HNil] = new Mapped[HNil, F] {
    type Out = HNil
    def extract(l: HNil, f: F ~> Id): HNil = HNil
    def remap[G[_]](l: HNil, f: F ~> G): BoundedAPair[HList, Id, ({ type out[m <: HList] = Mapped.Aux[HNil, G, m] })#out] =
      BoundedAPair.of[HList, Id, ({ type out[m <: HList] = Mapped.Aux[HNil, G, m] })#out](HNil, hnilMapped[G])
    def toList(l: HNil): List[Exists[F]] = Nil
    def toList[A](l: HNil, f: F ~>> A): List[A] = Nil
  }

  implicit def hconsMapped[H, T <: HList, F[_], OutM <: HList](implicit mt: Mapped.Aux[T, F, OutM]): Aux[H :: T, F, F[H] :: OutM] =
    mt.prepend[H]
}

trait MappedListBuilder[L <: HList] {
  def map[F[_]](l: L)(f: Id ~> F): BoundedAPair[HList, Id, ({ type out[m <: HList] = Mapped.Aux[L, F, m] })#out]
  def supply[F[_]](f: Forall[F]): BoundedAPair[HList, Id, ({ type out[m <: HList] = Mapped.Aux[L, F, m] })#out]
}

object MappedListBuilder {
  implicit def hnilBuilder: MappedListBuilder[HNil] = new MappedListBuilder[HNil] {
    def map[F[_]](l: HNil)(f: Id ~> F): BoundedAPair[HList, Id, ({ type out[m <: HList] = Mapped.Aux[HNil, F, m] })#out] =
      BoundedAPair.of[HList, Id, ({ type out[m <: HList] = Mapped.Aux[HNil, F, m] })#out](HNil, Mapped.hnilMapped[F])

    def supply[F[_]](f: Forall[F]): BoundedAPair[HList, Id, ({ type out[m <: HList] = Mapped.Aux[HNil, F, m] })#out] =
      BoundedAPair.of[HList, Id, ({ type out[m <: HList] = Mapped.Aux[HNil, F, m] })#out](HNil, Mapped.hnilMapped[F])
  }

  implicit def hconsBuilder[H, T <: HList](implicit T: MappedListBuilder[T]): MappedListBuilder[H :: T] = new MappedListBuilder[H :: T] {
    def map[F[_]](l: H :: T)(f: Id ~> F): BoundedAPair[HList, Id, ({ type out[m <: HList] = Mapped.Aux[H :: T, F, m] })#out] = {
      val t = T.map(l.tail)(f)
      BoundedAPair.of[HList, Id, ({ type out[m <: HList] = Mapped.Aux[H :: T, F, m] })#out](f(l.head) :: t._1, t._2.prepend[H])
    }

    def supply[F[_]](f: Forall[F]): BoundedAPair[HList, Id, ({ type out[m <: HList] = Mapped.Aux[H :: T, F, m] })#out] = {
      val t = T.supply[F](f)
      BoundedAPair.of[HList, Id, ({ type out[m <: HList] = Mapped.Aux[H :: T, F, m] })#out](f[H] :: t._1, t._2.prepend[H])
    }
  }
}