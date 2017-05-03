package nutcracker.lib.bool

import scala.language.higherKinds
import nutcracker.BranchingPropagation
import nutcracker.lib.bool.Bool._
import nutcracker.ops._

import scalaz.{Applicative, Apply, Bind, Monad}
import scalaz.syntax.bind._

class BoolOps[M[_], Var[_], Val[_]](implicit BP: BranchingPropagation[M, Var, Val]) {
  import BP._
  val P = BP.propagation
  import P._

  def and(x: Var[Bool], y: Var[Bool])(implicit M: Monad[M]): M[Var[Bool]] = for {
    res <- newVar[Bool]
    _ <- x._whenFinal({ r =>
      if (r) y <=> res
      else res.set(false)
    })
    _ <- y._whenFinal({ r =>
      if (r) x <=> res
      else res.set(false)
    })
    _ <- res._whenFinal({ r =>
      if (r) x.set(true) >> y.set(true)
      else M.pure(())
    })
  } yield res

  def or(x: Var[Bool]*)(implicit M: Monad[M]): M[Var[Bool]] = {
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
          _selThreshold2(x(i), x(j))((di, dj) => {
            if (di == MustBeTrue || dj == MustBeTrue) {
              // found a variable set to true,
              // thus the result must be true
              Some(res.set(true))
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

  def neg(x: Var[Bool])(implicit M: Monad[M]): M[Var[Bool]] = for {
    res <- newVar[Bool]
    _ <- x.whenFinal({ r =>
      if (r) res.set(false)
      else res.set(true)
    })
    _ <- res.whenFinal({ r =>
      if (r) x.set(false)
      else x.set(true)
    })
  } yield res

  def negM(x: M[Var[Bool]])(implicit M: Monad[M]): M[Var[Bool]] =
    x >>= { neg(_) }

  def not(x: Var[Bool]): M[Unit] =
    x.set(false)

  def imp(x: Var[Bool], y: Var[Bool])(implicit M: Applicative[M]): M[Unit] = {
    x.whenFinal({ r =>
      if (r) y.set(true)
      else M.pure(())
    }) *>
    y.whenFinal_({ r =>
      if (r) M.pure(())
      else x.set(false)
    })
  }

  def atLeastOneTrue(x: Var[Bool]*)(implicit M: Monad[M]): M[Unit] =
    presume(or(x: _*))

  def presume(x: Var[Bool]): M[Unit] =
    x.set(true)

  def presume(x: M[Var[Bool]])(implicit B: Bind[M]): M[Unit] =
    x >>= { _.set(true) }

  implicit class BoolRefOps(self: Var[Bool]) {

    def ===(that: Var[Bool])(implicit M: Apply[M]): M[Unit] = {
      (self whenFinal_ { (x: Boolean) => that.set(x) }) *>
      (that whenFinal_ { (x: Boolean) => self.set(x) })
    }

    def =?=(that: Var[Bool])(implicit M: Monad[M]): M[Var[Bool]] = for {
      res <- newVar[Bool]
      _ <- res  whenFinal { (x: Boolean) => if(x) (self === that) else (self =!= that) }
      _ <- self whenFinal { (x: Boolean) => if(x) (res  === that) else (res  =!= that) }
      _ <- that whenFinal { (x: Boolean) => if(x) (res  === self) else (res  =!= self) }
    } yield res

    def =??=(that: M[Var[Bool]])(implicit M: Monad[M]): M[Var[Bool]] =
      that >>= { self =?= _ }

    def |(that: Var[Bool])(implicit M: Monad[M]): M[Var[Bool]] =
      or(self, that)
    def ||(that: M[Var[Bool]])(implicit M: Monad[M]): M[Var[Bool]] =
      that >>= { self | _ }

    def &(that: Var[Bool])(implicit M: Monad[M]): M[Var[Bool]] = and(self, that)
    def &&(that: M[Var[Bool]])(implicit M: Monad[M]): M[Var[Bool]] =
      that >>= { self & _ }
  }

  implicit class BoolRefOps1(self: M[Var[Bool]]) {
    def ===(that: Var[Bool])(implicit B: Bind[M]): M[Unit] = self >>= { _ === that }
    def =!=(that: Var[Bool])(implicit B: Bind[M]): M[Unit] = self >>= { _ =!= that }
    def =?=(that: Var[Bool])(implicit M: Monad[M]): M[Var[Bool]] = self >>= { _ =?= that }
    def =??=(that: M[Var[Bool]])(implicit M: Monad[M]): M[Var[Bool]] = self >>= { _ =??= that }
    def |(that: Var[Bool])(implicit M: Monad[M]): M[Var[Bool]] = self >>= { _ | that }
    def ||(that: M[Var[Bool]])(implicit M: Monad[M]): M[Var[Bool]] = self >>= { _ || that }
    def &(that: Var[Bool])(implicit M: Monad[M]): M[Var[Bool]] = self >>= { _ & that }
    def &&(that: M[Var[Bool]])(implicit M: Monad[M]): M[Var[Bool]] = self >>= { _ && that }
  }

}

object BoolOps {
  def apply[M[_], Var[_], Val[_]](implicit P: BranchingPropagation[M, Var, Val]): BoolOps[M, Var, Val] = new BoolOps()
}