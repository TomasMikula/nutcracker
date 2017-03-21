package nutcracker.lib.bool

import scala.language.higherKinds
import nutcracker.BranchingPropagation
import nutcracker.lib.bool.Bool._
import nutcracker.ops._

import scalaz.{Applicative, Apply, Bind, Monad}
import scalaz.syntax.bind._

class BoolOps[M[_], Ref[_]](implicit P: BranchingPropagation[M, Ref]) {
  import P._

  def and(x: Ref[Bool], y: Ref[Bool])(implicit M: Monad[M]): M[Ref[Bool]] = {
    newVar[Bool] >>= { res =>
      x._whenFinal[M]({ r =>
        if (r) y <=> res
        else res.set(false)
      }) >>
        y._whenFinal({ r =>
          if (r) x <=> res
          else res.set(false)
        }) >>
        res.whenFinal({ r =>
          if (r) x.set(true) >> y.set(true)
          else M.pure(())
        }) >>
        M.pure(res)
    }
  }

  def or(x: Ref[Bool]*)(implicit M: Monad[M]): M[Ref[Bool]] = {
    newVar[Bool] >>= { res =>
      def watch(i: Int, j: Int): M[_] = {
        require(i < j)
        if(j < 0) {
          // all variables have been set to false,
          // thus the result must be false
          res.set(false)
        } else if(i < 0) {
          // all but one variable set to false,
          // the result is equal to the remaining one
          x(j) <=> res
        } else {
          P.propagation._selThreshold2(x(i), x(j))((di, dj) => {
            if (di == MustBeTrue || dj == MustBeTrue) {
              // found a variable set to true,
              // thus the result must be true
              Some(res.set[M, Boolean](true))
            } else if (di == MustBeFalse) {
              // pick next variable to watch instead of x(i)
              Some(watch(i-1, j))
            } else if (dj == MustBeFalse) {
              // pick next variable to watch instead of x(j)
              Some(watch(i-1, i))
            } else {
              None
            }
          })
        }
      }
      watch(x.size - 2, x.size - 1) >> M.pure(res)
    }
  }

  def neg(x: Ref[Bool])(implicit M: Monad[M]): M[Ref[Bool]] = {
    newVar[Bool] >>= { res =>
      x.whenFinal({ r =>
        if (r) res.set(false)
        else res.set(true)
      }) *>
      res.whenFinal({ r =>
        if (r) x.set(false)
        else x.set(true)
      }) *>
      M.pure(res)
    }
  }

  def negM(x: M[Ref[Bool]])(implicit M: Monad[M]): M[Ref[Bool]] =
    x >>= { neg(_) }

  def not(x: Ref[Bool]): M[Unit] =
    x.set(false)

  def imp(x: Ref[Bool], y: Ref[Bool])(implicit M: Applicative[M]): M[Unit] = {
    x.whenFinal({ r =>
      if (r) y.set(true)
      else M.pure(())
    }) *>
    y.whenFinal_({ r =>
      if (r) M.pure(())
      else x.set(false)
    })
  }

  def atLeastOneTrue(x: Ref[Bool]*)(implicit M: Monad[M]): M[Unit] =
    presume(or(x: _*))

  def presume(x: Ref[Bool]): M[Unit] =
    x.set(true)

  def presume(x: M[Ref[Bool]])(implicit B: Bind[M]): M[Unit] =
    x >>= { _.set(true) }

  implicit class BoolRefOps(self: Ref[Bool]) {

    def ===(that: Ref[Bool])(implicit M: Apply[M]): M[Unit] = {
      (self whenFinal_ { (x: Boolean) => that.set(x) }) *>
      (that whenFinal_ { (x: Boolean) => self.set(x) })
    }

    def =?=(that: Ref[Bool])(implicit M: Monad[M]): M[Ref[Bool]] = {
      newVar[Bool] >>= { res =>
        (res  whenFinal { (x: Boolean) => if(x) (self === that) else (self =!= that) }) *>
        (self whenFinal { (x: Boolean) => if(x) (res  === that) else (res  =!= that) }) *>
        (that whenFinal { (x: Boolean) => if(x) (res  === self) else (res  =!= self) }) *>
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
  def apply[M[_], Ref[_]](implicit P: BranchingPropagation[M, Ref]): BoolOps[M, Ref] = new BoolOps()
}