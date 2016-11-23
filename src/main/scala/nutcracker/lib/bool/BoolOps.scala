package nutcracker.lib.bool

import scala.language.higherKinds

import nutcracker.{FinalVars, Propagation}
import nutcracker.Trigger._
import nutcracker.lib.bool.Bool._
import nutcracker.ops._

import scalaz.{Bind, Monad}
import scalaz.syntax.bind._

class BoolOps[M[_], Ref[_]](implicit P: Propagation[M, Ref]) {
  val V = FinalVars[M, Ref]

  import P._

  def and(x: Ref[Bool], y: Ref[Bool])(implicit M: Monad[M]): M[Ref[Bool]] = {
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

  def or(x: Ref[Bool]*)(implicit M: Monad[M]): M[Ref[Bool]] = {
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

  def neg(x: Ref[Bool])(implicit M: Monad[M]): M[Ref[Bool]] = {
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

  def negM(x: M[Ref[Bool]])(implicit M: Monad[M]): M[Ref[Bool]] = x >>= { neg(_) }

  def not(x: Ref[Bool]): M[Unit] = {
    import V._
    set(x, false)
  }

  def imp(x: Ref[Bool], y: Ref[Bool])(implicit M: Monad[M]): M[Unit] = {
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

  def atLeastOneTrue(x: Ref[Bool]*)(implicit M: Monad[M]): M[Unit] = {
    presume(or(x: _*))
  }

  def presume(x: Ref[Bool]): M[Unit] = {
    import V._
    set(x, true)
  }

  def presume(x: M[Ref[Bool]])(implicit B: Bind[M]): M[Unit] = {
    import V._
    x >>= { set(_, true) }
  }

  implicit class BoolRefOps(self: Ref[Bool]) {
    import V._

    def ===(that: Ref[Bool])(implicit B: Bind[M]): M[Unit] = {
      (self >>= { (x: Boolean) => set(that, x) }) >>
        (that >>= { (x: Boolean) => set(self, x) })
    }

    def =!=(that: Ref[Bool])(implicit B: Bind[M]): M[Unit] = {
      (self >>= { (x: Boolean) => set(that, !x) }) >>
        (that >>= { (x: Boolean) => set(self, !x) })
    }

    def =?=(that: Ref[Bool])(implicit M: Monad[M]): M[Ref[Bool]] = {
      variable[Boolean]() >>= { res =>
        (res  >>= { (x: Boolean) => if(x) (self === that) else (self =!= that) }) >>
          (self >>= { (x: Boolean) => if(x) (res  === that) else (res  =!= that) }) >>
          (that >>= { (x: Boolean) => if(x) (res  === self) else (res  =!= self) }) >>
          M.pure(res)
      }
    }

    def =??=(that: M[Ref[Bool]])(implicit M: Monad[M]): M[Ref[Bool]] =
      that >>= { self =?= _ }

    def |(that: Ref[Bool])(implicit M: Monad[M]): M[Ref[Bool]] =
      or(self, that)
    def ||(that: M[Ref[Bool]])(implicit M: Monad[M]): M[Ref[Bool]] =
      that >>= { self | _ }

    def &(that: Ref[Bool])(implicit M: Monad[M]): M[Ref[Bool]] = and(self, that)
    def &&(that: M[Ref[Bool]])(implicit M: Monad[M]): M[Ref[Bool]] =
      that >>= { self & _ }
  }

  implicit class BoolRefOps1(self: M[Ref[Bool]]) {
    def ===(that: Ref[Bool])(implicit B: Bind[M]): M[Unit] = self >>= { _ === that }
    def =!=(that: Ref[Bool])(implicit B: Bind[M]): M[Unit] = self >>= { _ =!= that }
    def =?=(that: Ref[Bool])(implicit M: Monad[M]): M[Ref[Bool]] = self >>= { _ =?= that }
    def =??=(that: M[Ref[Bool]])(implicit M: Monad[M]): M[Ref[Bool]] = self >>= { _ =??= that }
    def |(that: Ref[Bool])(implicit M: Monad[M]): M[Ref[Bool]] = self >>= { _ | that }
    def ||(that: M[Ref[Bool]])(implicit M: Monad[M]): M[Ref[Bool]] = self >>= { _ || that }
    def &(that: Ref[Bool])(implicit M: Monad[M]): M[Ref[Bool]] = self >>= { _ & that }
    def &&(that: M[Ref[Bool]])(implicit M: Monad[M]): M[Ref[Bool]] = self >>= { _ && that }
  }

}

object BoolOps {
  def apply[M[_], Ref[_]](implicit P: Propagation[M, Ref]): BoolOps[M, Ref] = new BoolOps()
}