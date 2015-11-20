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
        case MustBeFalse => Intersect(res, MustBeFalse)
        case MustBeTrue => y <=> res
      }) >>
      partialVarTrigger(y)({
        case MustBeFalse => Intersect(res, MustBeFalse)
        case MustBeTrue => x <=> res
      }) >>
      partialVarTrigger(res)({
        case MustBeTrue => Intersect(x, MustBeTrue) >> Intersect(y, MustBeTrue)
      }) >>
      Pure(res)
    }
  }

  def or(x: Ref, y: Ref): ProblemDescription[Ref] = {
    variable[Boolean]() >>= { res =>
      partialVarTrigger(x)({
        case MustBeTrue => Intersect(res, MustBeTrue)
        case MustBeFalse => y <=> res
      }) >>
      partialVarTrigger(y)({
        case MustBeTrue => Intersect(res, MustBeTrue)
        case MustBeFalse => x <=> res
      }) >>
      partialVarTrigger(res)({
        case MustBeFalse => Intersect(x, MustBeFalse) >> Intersect(y, MustBeFalse)
      }) >>
      Pure(res)
    }
  }

  def not(x: Ref): ProblemDescription[Ref] = {
    variable[Boolean]() >>= { res =>
      partialVarTrigger(x)({
        case MustBeTrue => Intersect(res, MustBeFalse)
        case MustBeFalse => Intersect(res, MustBeTrue)
      }) >>
      partialVarTrigger(res)({
        case MustBeTrue => Intersect(x, MustBeFalse)
        case MustBeFalse => Intersect(x, MustBeTrue)
      }) >>
      Pure(res)
    }
  }

  def imp(x: Ref, y: Ref): ProblemDescription[Unit] = {
    partialVarTrigger(x)({
      case MustBeTrue => Intersect(y, MustBeTrue)
    }) >>
    partialVarTrigger(y)({
      case MustBeFalse => Intersect(x, MustBeFalse)
    })
  }
}
