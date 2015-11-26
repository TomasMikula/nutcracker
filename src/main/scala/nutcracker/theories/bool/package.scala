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
    or(x: _*) >>= { set(_, true) }
  }
}
