package nutcracker.theories

import nutcracker.{Fire, Sleep}
import nutcracker.{DomRef, ProblemDescription}
import nutcracker.ProblemDescription._
import nutcracker.theories.bool.BoolDomain._

package object bool {

  type Ref = DomRef[Boolean, BoolDomain]

  def and(x: Ref, y: Ref): ProblemDescription[Ref] = {
    variable[Boolean]() >>= { res =>
      partialVarTrigger(x)({
        case MustBeFalse => Fire(set(res, false))
        case MustBeTrue => Fire(y <=> res)
      }) >>
      partialVarTrigger(y)({
        case MustBeFalse => Fire(set(res, false))
        case MustBeTrue => Fire(x <=> res)
      }) >>
      partialVarTrigger(res)({
        case MustBeTrue => Fire(set(x, true) >> set(y, true))
      }) >>
      Pure(res)
    }
  }

  def or(x: Ref*): ProblemDescription[Ref] = {
    variable[Boolean]() >>= { res =>
      def watch(i: Int, j: Int): ProblemDescription[Unit] = {
        require(i < j)
        if(j < 0) {
          // all variables have been set to false,
          // thus the result must be false
          set(res, false)
        } else if(i < 0) {
          // all but one variable set to false,
          // the result is equal to the remaining one
          x(j) <=> res
        } else {
          selectionTrigger2(x(i), x(j))((di, dj) => {
            if (di == MustBeTrue || dj == MustBeTrue) {
              // found a variable set to true,
              // thus the result must be true
              Fire(set(res, true))
            } else if (di == MustBeFalse) {
              // pick next variable to watch instead of x(i)
              Fire(watch(i-1, j))
            } else if (dj == MustBeFalse) {
              // pick next variable to watch instead of x(j)
              Fire(watch(i-1, i))
            } else {
              Sleep
            }
          })
        }
      }
      watch(x.size - 2, x.size - 1) >> Pure(res)
    }
  }

  def neg(x: Ref): ProblemDescription[Ref] = {
    variable[Boolean]() >>= { res =>
      partialVarTrigger(x)({
        case MustBeTrue => Fire(set(res, false))
        case MustBeFalse => Fire(set(res, true))
      }) >>
      partialVarTrigger(res)({
        case MustBeTrue => Fire(set(x, false))
        case MustBeFalse => Fire(set(x, true))
      }) >>
      Pure(res)
    }
  }

  def neg(x: ProblemDescription[Ref]): ProblemDescription[Ref] = x >>= { neg(_) }

  def not(x: Ref): ProblemDescription[Unit] =
    set(x, false)

  def imp(x: Ref, y: Ref): ProblemDescription[Unit] = {
    partialVarTrigger(x)({
      case MustBeTrue => Fire(set(y, true))
    }) >>
    partialVarTrigger(y)({
      case MustBeFalse => Fire(set(x, false))
    })
  }

  def atLeastOneTrue(x: Ref*): ProblemDescription[Unit] = {
    presume(or(x: _*))
  }

  def presume(x: Ref): ProblemDescription[Unit] = {
    set(x, true)
  }

  def presume(x: ProblemDescription[Ref]): ProblemDescription[Unit] = {
    x >>= { set(_, true) }
  }

  implicit class BoolOps(self: Ref) {

    def ===(that: Ref): ProblemDescription[Unit] = {
      (self >>= { set(that, _) }) >>
        (that >>= { set(self, _) })
    }

    def =!=(that: Ref): ProblemDescription[Unit] = {
      (self >>= { x => set(that, !x) }) >>
        (that >>= { x => set(self, !x) })
    }

    def =?=(that: Ref): ProblemDescription[Ref] = {
      variable[Boolean]() >>= { res =>
        (res >>= { if(_) (self === that) else (self =!= that) }) >>
          (self >>= { if(_) (res === that) else (res  =!= that) }) >>
          (that >>= { if(_) (res === self) else (res  =!= self) }) >>
          Pure(res)
      }
    }

    def =?=(that: ProblemDescription[Ref]): ProblemDescription[Ref] =
      that >>= { self =?= _ }

    def ∨(that: Ref): ProblemDescription[Ref] = or(self, that)
    def ∨(that: ProblemDescription[Ref]): ProblemDescription[Ref] =
      that >>= { self ∨ _ }

    def ∧(that: Ref): ProblemDescription[Ref] = and(self, that)
    def ∧(that: ProblemDescription[Ref]): ProblemDescription[Ref] =
      that >>= { self ∧ _ }
  }

  implicit class BoolOps1(self: ProblemDescription[Ref]) {
    def ===(that: Ref): ProblemDescription[Unit] = self >>= { _ === that }
    def =!=(that: Ref): ProblemDescription[Unit] = self >>= { _ =!= that }
    def =?=(that: Ref): ProblemDescription[Ref] = self >>= { _ =?= that }
    def =?=(that: ProblemDescription[Ref]): ProblemDescription[Ref] = self >>= { _ =?= that }
    def ∨(that: Ref): ProblemDescription[Ref] = self >>= { _ ∨ that }
    def ∨(that: ProblemDescription[Ref]): ProblemDescription[Ref] = self >>= { _ ∨ that }
    def ∧(that: Ref): ProblemDescription[Ref] = self >>= { _ ∧ that }
    def ∧(that: ProblemDescription[Ref]): ProblemDescription[Ref] = self >>= { _ ∧ that }
  }
}