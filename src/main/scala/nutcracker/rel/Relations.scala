package nutcracker.rel

import scala.language.higherKinds
import algebra.Order
import nutcracker.rel.RelDB.PartiallyAssignedPattern
import nutcracker.util.{ContU, Mapped, MappedListBuilder, SummonHList}
import scalaz.Bind
import scalaz.syntax.bind._
import shapeless.{HList, HNil, ::}

trait Relations[M[_]] {
  def relateImpl[L <: HList, OrderL <: HList](rel: Rel[L], values: L)(implicit m: Mapped.Aux[L, Order, OrderL], os: SummonHList[OrderL]): M[Unit]
  def onPatternMatch[V <: HList](p: Pattern[V], a: Assignment[V])(h: V => M[Unit]): M[Unit]

  def establishImpl[L <: HList, C <: HList, OrderL <: HList](rel: Rel[L], values: C, recipe: Recipe[L, C, M])(implicit L: MappedListBuilder[L], m: Mapped.Aux[rel.Projection, Order, OrderL], os: SummonHList[OrderL]): ContU[M, L]

  def establish[L <: HList](rel: Rel[L])(implicit m: Mapped[rel.Projection, Order]): EstablishSyntaxHelper[L, rel.Projection, m.Out] =
    new EstablishSyntaxHelper[L, rel.Projection, m.Out](rel)(m)

  def relate[L <: HList](rel: Rel[L])(implicit m: Mapped[L, Order]): RelateSyntaxHelper[L, m.Out] =
    new RelateSyntaxHelper[L, m.Out](rel)(m)

  def constrain[L <: HList](con: Constraint[L, M])(implicit m: Mapped[L, Order]): ConstrainSyntaxHelper[L, m.Out] =
    new ConstrainSyntaxHelper[L, m.Out](con)(m)

  final def onPatternMatch[V <: HList](p: Pattern[V])(h: V => M[Unit]): M[Unit] =
    onPatternMatch(p, p.emptyAssignment)(h)

  final def onPatternMatch[V <: HList](p: PartiallyAssignedPattern[V])(h: V => M[Unit]): M[Unit] =
    onPatternMatch(p.pattern, p.assignment)(h)


  final class RelateSyntaxHelper[L <: HList, OS <: HList](rel: Rel[L])(implicit m: Mapped.Aux[L, Order, OS]) {
    def values(vals: L)(implicit os: SummonHList[OS]): M[Unit] = relateImpl[L, OS](rel, vals)
  }

  final class ConstrainSyntaxHelper[L <: HList, OS <: HList](con: Constraint[L, M])(implicit m: Mapped.Aux[L, Order, OS]) {
    def values(vals: L)(implicit os: SummonHList[OS], M: Bind[M]): M[Unit] =
      relateImpl(con.rel, vals) >> con.setup(vals)
  }

  final class EstablishSyntaxHelper[L <: HList, L0 >: L <: HList, OS <: HList](rel: Rel.Aux[L, L0])(implicit m: Mapped.Aux[L0, Order, OS]) {
    def matching[C <: HList](values: C): EstablishMatchingSyntaxHelper[L, L0, C, OS] =
      new EstablishMatchingSyntaxHelper[L, L0, C, OS](rel, values)(m)

    def matching1[A](a: A): EstablishMatchingSyntaxHelper[L, L0, A :: HNil, OS] =
      new EstablishMatchingSyntaxHelper[L, L0, A :: HNil, OS](rel, a :: HNil)(m)
    def matching2[A, B](a: A, b: B): EstablishMatchingSyntaxHelper[L, L0, A :: B :: HNil, OS] =
      new EstablishMatchingSyntaxHelper[L, L0, A :: B :: HNil, OS](rel, a :: b :: HNil)(m)
    def matching3[A, B, C](a: A, b: B, c: C): EstablishMatchingSyntaxHelper[L, L0, A :: B :: C :: HNil, OS] =
      new EstablishMatchingSyntaxHelper[L, L0, A :: B :: C :: HNil, OS](rel, a :: b :: c :: HNil)(m)
    def matching4[A, B, C, D](a: A, b: B, c: C, d: D): EstablishMatchingSyntaxHelper[L, L0, A :: B :: C :: D :: HNil, OS] =
      new EstablishMatchingSyntaxHelper[L, L0, A :: B :: C :: D :: HNil, OS](rel, a :: b :: c :: d :: HNil)(m)
    def matching5[A, B, C, D, E](a: A, b: B, c: C, d: D, e: E): EstablishMatchingSyntaxHelper[L, L0, A :: B :: C :: D :: E :: HNil, OS] =
      new EstablishMatchingSyntaxHelper[L, L0, A :: B :: C :: D :: E :: HNil, OS](rel, a :: b :: c :: d :: e :: HNil)(m)
    def matching6[A, B, C, D, E, F](a: A, b: B, c: C, d: D, e: E, f: F): EstablishMatchingSyntaxHelper[L, L0, A :: B :: C :: D :: E :: F :: HNil, OS] =
      new EstablishMatchingSyntaxHelper[L, L0, A :: B :: C :: D :: E :: F :: HNil, OS](rel, a :: b :: c :: d :: e :: f :: HNil)(m)
  }

  final class EstablishMatchingSyntaxHelper[L <: HList, L0 >: L <: HList, C <: HList, OS <: HList](rel: Rel.Aux[L, L0], values: C)(implicit m: Mapped.Aux[L0, Order, OS]) {
    def by(recipe: Recipe[L, C, M])(implicit L: MappedListBuilder[L], os: SummonHList[OS]): ContU[M, L] =
      establishImpl[L, C, OS](rel, values, recipe)
  }
}

object Relations {
  def apply[M[_]](implicit ev: Relations[M]): Relations[M] = ev
}