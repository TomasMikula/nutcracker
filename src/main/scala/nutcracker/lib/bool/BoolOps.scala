package nutcracker.lib.bool

import scala.language.higherKinds

import nutcracker.{FinalVars, Propagation}
import nutcracker.Trigger._
import nutcracker.lib.bool.BoolDomain._

import scalaz.{Bind, Monad}
import scalaz.syntax.bind._

class BoolOps[M[_]](implicit P: Propagation[M]) {
  val V = FinalVars[M]

  import P._

  def and(x: BoolRef, y: BoolRef)(implicit M: Monad[M]): M[BoolRef] = {
    import V._
    variable[Boolean]() >>= { res =>
      whenFinal(x).exec({ r =>
        if (r) y <=> res
        else set(res, false)
      }) >>
        whenFinal(y).exec({ r =>
          if (r) x <=> res
          else set(res, false)
        }) >>
        whenFinal(res).exec({ r =>
          if (r) set(x, true) >> set(y, true)
          else M.pure(())
        }) >>
        M.pure(res)
    }
  }

  def or(x: BoolRef*)(implicit M: Monad[M]): M[BoolRef] = {
    import V._
    variable[Boolean]() >>= { res =>
      def watch(i: Int, j: Int): M[Unit] = {
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
          selTrigger2(x(i), x(j))((di, dj) => {
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
      watch(x.size - 2, x.size - 1) >> M.pure(res)
    }
  }

  def neg(x: BoolRef)(implicit M: Monad[M]): M[BoolRef] = {
    import V._
    variable[Boolean]() >>= { res =>
      whenFinal(x).exec({ r =>
        if (r) set(res, false)
        else set(res, true)
      }) >>
        whenFinal(res).exec({ r =>
          if (r) set(x, false)
          else set(x, true)
        }) >>
        M.pure(res)
    }
  }

  def neg(x: M[BoolRef])(implicit M: Monad[M]): M[BoolRef] = x >>= { neg(_) }

  def not(x: BoolRef): M[Unit] = {
    import V._
    set(x, false)
  }

  def imp(x: BoolRef, y: BoolRef)(implicit M: Monad[M]): M[Unit] = {
    import V._
    whenFinal(x).exec({ r =>
      if (r) set(y, true)
      else M.pure(())
    }) >>
      whenFinal(y).exec({ r =>
        if (r) M.pure(())
        else set(x, false)
      })
  }

  def atLeastOneTrue(x: BoolRef*)(implicit M: Monad[M]): M[Unit] = {
    presume(or(x: _*))
  }

  def presume(x: BoolRef): M[Unit] = {
    import V._
    set(x, true)
  }

  def presume(x: M[BoolRef])(implicit B: Bind[M]): M[Unit] = {
    import V._
    x >>= { set(_, true) }
  }

  implicit class BoolRefOps(self: BoolRef) {
    import V._

    def ===(that: BoolRef)(implicit B: Bind[M]): M[Unit] = {
      (self >>= { (x: Boolean) => set(that, x) }) >>
        (that >>= { (x: Boolean) => set(self, x) })
    }

    def =!=(that: BoolRef)(implicit B: Bind[M]): M[Unit] = {
      (self >>= { (x: Boolean) => set(that, !x) }) >>
        (that >>= { (x: Boolean) => set(self, !x) })
    }

    def =?=(that: BoolRef)(implicit M: Monad[M]): M[BoolRef] = {
      variable[Boolean]() >>= { res =>
        (res  >>= { (x: Boolean) => if(x) (self === that) else (self =!= that) }) >>
          (self >>= { (x: Boolean) => if(x) (res  === that) else (res  =!= that) }) >>
          (that >>= { (x: Boolean) => if(x) (res  === self) else (res  =!= self) }) >>
          M.pure(res)
      }
    }

    def =?=(that: M[BoolRef])(implicit M: Monad[M]): M[BoolRef] =
      that >>= { self =?= _ }

    def ∨(that: BoolRef)(implicit M: Monad[M]): M[BoolRef] =
      or(self, that)
    def ∨(that: M[BoolRef])(implicit M: Monad[M]): M[BoolRef] =
      that >>= { self ∨ _ }

    def ∧(that: BoolRef)(implicit M: Monad[M]): M[BoolRef] = and(self, that)
    def ∧(that: M[BoolRef])(implicit M: Monad[M]): M[BoolRef] =
      that >>= { self ∧ _ }
  }

  implicit class BoolRefOps1(self: M[BoolRef]) {
    def ===(that: BoolRef)(implicit B: Bind[M]): M[Unit] = self >>= { _ === that }
    def =!=(that: BoolRef)(implicit B: Bind[M]): M[Unit] = self >>= { _ =!= that }
    def =?=(that: BoolRef)(implicit M: Monad[M]): M[BoolRef] = self >>= { _ =?= that }
    def =?=(that: M[BoolRef])(implicit M: Monad[M]): M[BoolRef] = self >>= { _ =?= that }
    def ∨(that: BoolRef)(implicit M: Monad[M]): M[BoolRef] = self >>= { _ ∨ that }
    def ∨(that: M[BoolRef])(implicit M: Monad[M]): M[BoolRef] = self >>= { _ ∨ that }
    def ∧(that: BoolRef)(implicit M: Monad[M]): M[BoolRef] = self >>= { _ ∧ that }
    def ∧(that: M[BoolRef])(implicit M: Monad[M]): M[BoolRef] = self >>= { _ ∧ that }
  }

}

object BoolOps {
  def apply[M[_]: Propagation]: BoolOps[M] = new BoolOps()
}