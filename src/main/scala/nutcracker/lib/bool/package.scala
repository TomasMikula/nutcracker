package nutcracker.lib

import scala.language.higherKinds
import nutcracker.util.{FreeK, InjectK}
import nutcracker._
import nutcracker.PropagationLang._
import nutcracker.Trigger._
import nutcracker.lib.bool.BoolDomain._

import scalaz.\/

package object bool {

  type BoolDom = Dom [BoolDomain, Meet[BoolDomain] \/ Diff[BoolDomain], Unit]
  type BoolRef = DRef[BoolDomain, Meet[BoolDomain] \/ Diff[BoolDomain], Unit]

  def and(x: BoolRef, y: BoolRef): FreeK[PropagationLang, BoolRef] = {
    variable[Boolean]() >>= { res =>
      whenResolvedF(x)({ (r: Boolean) =>
        if (r) y <=> res
        else set(res, false)
      }) >>
      whenResolvedF(y)({ (r: Boolean) =>
        if (r) x <=> res
        else set(res, false)
      }) >>
      whenResolvedF(res)({ (r: Boolean) =>
        if (r) set(x, true) >> set(y, true)
        else FreeK.Pure(())
      }) >>
      FreeK.Pure(res)
    }
  }

  def or(x: BoolRef*): FreeK[PropagationLang, BoolRef] = {
    variable[Boolean]() >>= { res =>
      def watch(i: Int, j: Int): FreeK[PropagationLang, Unit] = {
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
          selTrigger2F(x(i), x(j))((di, dj) => {
            if (di == MustBeTrue || dj == MustBeTrue) {
              // found a variable set to true,
              // thus the result must be true
              fire(set(res, true))
            } else if (di == MustBeFalse) {
              // pick next variable to watch instead of x(i)
              fire(watch(i-1, j))
            } else if (dj == MustBeFalse) {
              // pick next variable to watch instead of x(j)
              fire(watch(i-1, i))
            } else {
              sleep
            }
          })
        }
      }
      watch(x.size - 2, x.size - 1) >> FreeK.Pure(res)
    }
  }

  def neg(x: BoolRef): FreeK[PropagationLang, BoolRef] = {
    variable[Boolean]() >>= { res =>
      whenResolvedF(x)({ (r: Boolean) =>
        if (r) set(res, false)
        else set(res, true)
      }) >>
        whenResolvedF(res)({ (r: Boolean) =>
        if (r) set(x, false)
        else set(x, true)
      }) >>
      FreeK.Pure(res)
    }
  }

  def neg[F[_[_], _]](x: FreeK[F, BoolRef])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, BoolRef] = x >>= { neg(_) }

  def not(x: BoolRef): FreeK[PropagationLang, Unit] =
    set(x, false)

  def imp(x: BoolRef, y: BoolRef): FreeK[PropagationLang, Unit] = {
    whenResolvedF(x)({ (r: Boolean) =>
      if (r) set(y, true)
      else FreeK.Pure[PropagationLang, Unit](())
    }) >>
    whenResolvedF(y)({ (r: Boolean) =>
      if (r) FreeK.Pure(())
      else set(x, false)
    })
  }

  def atLeastOneTrue(x: BoolRef*): FreeK[PropagationLang, Unit] = {
    presume(or(x: _*))
  }

  def presume(x: BoolRef): FreeK[PropagationLang, Unit] = {
    set(x, true)
  }

  def presume[F[_[_], _]](x: FreeK[F, BoolRef])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] = {
    x >>= { set(_, true) }
  }

  implicit class BoolOps(self: BoolRef) {

    def ===(that: BoolRef): FreeK[PropagationLang, Unit] = {
      (self >>= { (x: Boolean) => set(that, x) }) >>
      (that >>= { (x: Boolean) => set(self, x) })
    }

    def =!=(that: BoolRef): FreeK[PropagationLang, Unit] = {
      (self >>= { (x: Boolean) => set(that, !x) }) >>
      (that >>= { (x: Boolean) => set(self, !x) })
    }

    def =?=(that: BoolRef): FreeK[PropagationLang, BoolRef] = {
      variable[Boolean]() >>= { res =>
        (res  >>= { (x: Boolean) => if(x) (self === that) else (self =!= that) }) >>
        (self >>= { (x: Boolean) => if(x) (res  === that) else (res  =!= that) }) >>
        (that >>= { (x: Boolean) => if(x) (res  === self) else (res  =!= self) }) >>
        FreeK.Pure(res)
      }
    }

    def =?=[F[_[_], _]](that: FreeK[F, BoolRef])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, BoolRef] =
      that >>= { self =?= _ }

    def ∨(that: BoolRef): FreeK[PropagationLang, BoolRef] = or(self, that)
    def ∨[F[_[_], _]](that: FreeK[F, BoolRef])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, BoolRef] =
      that >>= { self ∨ _ }

    def ∧(that: BoolRef): FreeK[PropagationLang, BoolRef] = and(self, that)
    def ∧[F[_[_], _]](that: FreeK[F, BoolRef])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, BoolRef] =
      that >>= { self ∧ _ }
  }

  implicit class BoolOps1[F[_[_], _]](self: FreeK[F, BoolRef]) {
    def ===(that: BoolRef)(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] = self >>= { _ === that }
    def =!=(that: BoolRef)(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] = self >>= { _ =!= that }
    def =?=(that: BoolRef)(implicit inj: InjectK[PropagationLang, F]): FreeK[F, BoolRef] = self >>= { _ =?= that }
    def =?=(that: FreeK[F, BoolRef])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, BoolRef] = self >>= { _ =?= that }
    def ∨(that: BoolRef)(implicit inj: InjectK[PropagationLang, F]): FreeK[F, BoolRef] = self >>= { _ ∨ that }
    def ∨(that: FreeK[F, BoolRef])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, BoolRef] = self >>= { _ ∨ that }
    def ∧(that: BoolRef)(implicit inj: InjectK[PropagationLang, F]): FreeK[F, BoolRef] = self >>= { _ ∧ that }
    def ∧(that: FreeK[F, BoolRef])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, BoolRef] = self >>= { _ ∧ that }
  }
}