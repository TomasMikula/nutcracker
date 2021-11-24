package nutcracker

import nutcracker.util.{ChooseByPtrs, HList}
import nutcracker.util.HList.{::, HNil}

/** N-ary relation (where N is determined by `L`).
  * Loosely speaking, it represents the signature of a database table,
  * i.e. the datatypes of the columns.
  *
  * An implementation `MyRel[A, B] extends Rel[F[A, B]]` must ensure that
  *  - if `(rel1: MyRel[T, U]) == (rel2: MyRel[V, W])`, i.e. two values of `MyRel` with different types are equal, then
  *    - for all `row1: F[T, U]`, `row2: F[V, W]`
  *      - `row1 == row2` ⇒ `F[T, U] =:= F[V, W]`
  *
  * In other words, the table signature can be somewhat parametric,
  * as long as the actual row values cannot be mistakenly thought
  * to be of a different type.
  *
  * Moreover, queries have to specify values of enough columns, so
  * that the types of the remaining columns are uniquely determined
  * by the types of the given columns.
  *
  * @tparam L constituents of this relation.
  */
trait Rel[L <: HList] {

  type Projection >: L <: HList

  def apply[V <: HList, Ptrs <: HList](
    ptrs: Ptrs)(implicit
    ch: ChooseByPtrs[V, L, Ptrs]
  ): RelChoice[V, L] =
    RelChoice(this, ch(ptrs))

  def apply[V <: HList, P1]                    (p1: P1)                                        (implicit ch: ChooseByPtrs[V, L, P1 :: HNil]):                               RelChoice[V, L] = apply(p1 :: HNil)
  def apply[V <: HList, P1, P2]                (p1: P1, p2: P2)                                (implicit ch: ChooseByPtrs[V, L, P1 :: P2 :: HNil]):                         RelChoice[V, L] = apply(p1 :: p2 :: HNil)
  def apply[V <: HList, P1, P2, P3]            (p1: P1, p2: P2, p3: P3)                        (implicit ch: ChooseByPtrs[V, L, P1 :: P2 :: P3 :: HNil]):                   RelChoice[V, L] = apply(p1 :: p2 :: p3 :: HNil)
  def apply[V <: HList, P1, P2, P3, P4]        (p1: P1, p2: P2, p3: P3, p4: P4)                (implicit ch: ChooseByPtrs[V, L, P1 :: P2 :: P3 :: P4 :: HNil]):             RelChoice[V, L] = apply(p1 :: p2 :: p3 :: p4 :: HNil)
  def apply[V <: HList, P1, P2, P3, P4, P5]    (p1: P1, p2: P2, p3: P3, p4: P4, p5: P5)        (implicit ch: ChooseByPtrs[V, L, P1 :: P2 :: P3 :: P4 :: P5 :: HNil]):       RelChoice[V, L] = apply(p1 :: p2 :: p3 :: p4 :: p5 :: HNil)
  def apply[V <: HList, P1, P2, P3, P4, P5, P6](p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6)(implicit ch: ChooseByPtrs[V, L, P1 :: P2 :: P3 :: P4 :: P5 :: P6 :: HNil]): RelChoice[V, L] = apply(p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: HNil)
}

object Rel {
  type Aux[L <: HList, L0 >: L <: HList] = Rel[L] { type Projection = L0 }

  type Rel1[A1] = Rel[A1 :: HNil]
  type Rel2[A1, A2] = Rel[A1 :: A2 :: HNil]
  type Rel3[A1, A2, A3] = Rel[A1 :: A2 :: A3 :: HNil]
  type Rel4[A1, A2, A3, A4] = Rel[A1 :: A2 :: A3 :: A4 :: HNil]
  type Rel5[A1, A2, A3, A4, A5] = Rel[A1 :: A2 :: A3 :: A4 :: A5 :: HNil]
  type Rel6[A1, A2, A3, A4, A5, A6] = Rel[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: HNil]
}