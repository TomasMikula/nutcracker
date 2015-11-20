package nutcracker.theories

import nutcracker.PureDomRef
import nutcracker.ProblemDescription
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

  def atLeastOne(x: Ref*): ProblemDescription[Unit] = {
    // XXX the problem with watching just 1 variable is that if it is the last one satisfiable,
    // it will not be immediately set to true (unless it is x(0)).
    require(x.size >= 1)
    def atLeastOne(x: Seq[Ref], i: Int): ProblemDescription[Unit] = {
      if(i == 0) set(x(0), true)
      else partialVarTrigger(x(i))({
        case MustBeFalse => atLeastOne(x, i-1)
      })
    }
    atLeastOne(x, x.size - 1)
  }
}
