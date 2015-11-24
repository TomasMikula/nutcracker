package nutcracker.theories

import nutcracker.Triggers.{Fire, Sleep, Discard}
import nutcracker.{Sel, PureDomRef, ProblemDescription}
import nutcracker.ProblemDescription._
import nutcracker.theories.bool.BoolDomain._

package object bool {

  type Ref = PureDomRef[Boolean, BoolDomain]

  def and(x: Ref, y: Ref): ProblemDescription[Ref] = {
    variable[Boolean]() >>= { res =>
      partialVarTrigger(x)({
        case MustBeFalse => set(res, false)
        case MustBeTrue => y <=> res
      }) >>
      partialVarTrigger(y)({
        case MustBeFalse => set(res, false)
        case MustBeTrue => x <=> res
      }) >>
      partialVarTrigger(res)({
        case MustBeTrue => set(x, true) >> set(y, true)
      }) >>
      Pure(res)
    }
  }

  def or(x: Ref, y: Ref): ProblemDescription[Ref] = {
    variable[Boolean]() >>= { res =>
      partialVarTrigger(x)({
        case MustBeTrue => set(res, true)
        case MustBeFalse => y <=> res
      }) >>
      partialVarTrigger(y)({
        case MustBeTrue => set(res, true)
        case MustBeFalse => x <=> res
      }) >>
      partialVarTrigger(res)({
        case MustBeFalse => set(x, false) >> set(y, false)
      }) >>
      Pure(res)
    }
  }

  def not(x: Ref): ProblemDescription[Ref] = {
    variable[Boolean]() >>= { res =>
      partialVarTrigger(x)({
        case MustBeTrue => set(res, false)
        case MustBeFalse => set(res, true)
      }) >>
      partialVarTrigger(res)({
        case MustBeTrue => set(x, false)
        case MustBeFalse => set(x, true)
      }) >>
      Pure(res)
    }
  }

  def imp(x: Ref, y: Ref): ProblemDescription[Unit] = {
    partialVarTrigger(x)({
      case MustBeTrue => set(y, true)
    }) >>
    partialVarTrigger(y)({
      case MustBeFalse => set(x, false)
    })
  }

  def atLeastOneTrue(x: Ref*): ProblemDescription[Unit] = {
    require(x.size >= 1)
    def watch(i: Int, j: Int): ProblemDescription[Unit] = {
      require(i < j)
      require(j >= 0)
      if (i < 0) {
        set(x(j), true)
      } else {
        selectionTrigger2(x(i), x(j))((di, dj) => {
          if (di == MustBeTrue || dj == MustBeTrue) {
            // constraint satisfied, we are done
            Discard
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
    watch(x.size - 2, x.size - 1)
  }
}
