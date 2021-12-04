package nutcracker.data.bool

import nutcracker.{BranchingPropagation, Dom, Subscription, UpdateResult}
import nutcracker.data.bool.Bool._
import nutcracker.ops.Ops._
import scalaz.{Applicative, Apply, Bind, Monad, Traverse}
import scalaz.std.list._
import scalaz.syntax.apply._
import scalaz.syntax.bind0._

class BoolOps[M[_], Var[_], Val[_]](implicit BP: BranchingPropagation[M, Var, Val]) {
  import BP._
  val P = BP.propagation
  import P._

  def and(x: Var[Bool], y: Var[Bool])(implicit M: Monad[M]): M[Var[Bool]] = for {
    res <- newVar[Bool]
    _ <- x.whenFinal({ r =>
      if (r) (y <=> res).void
      else res.set(false)
    })
    _ <- y.whenFinal({ r =>
      if (r) (x <=> res).void
      else res.set(false)
    })
    _ <- res._whenFinal({ r =>
      if (r) x.set(true) >> y.set(true)
      else M.pure(())
    })
  } yield res

  def or(x: Var[Bool], y: Var[Bool])(implicit M: Monad[M]): M[Var[Bool]] = for {
    res <- newVar[Bool]
    _ <- x._whenFinal({ r =>
      if (r) res.set(true)
      else (y <=> res).void
    })
    _ <- y._whenFinal({ r =>
      if (r) res.set(true)
      else (x <=> res).void
    })
    _ <- res._whenFinal({ r =>
      if (r) M.pure(())
      else x.set(false) >> y.set(false)
    })
  } yield res

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

  def atLeastOneTrue(x: Var[Bool]*)(implicit M: Monad[M]): M[Unit] = {
    require(x.size > 0)

    def watch(i: Int, wr: Var[Watched]): M[Subscription[M]] =
      observe(x(i)).threshold(_ match {
        case MustBeTrue => Some(update(wr).by(Watched.Satisfied))
        case MustBeFalse => Some(update(wr).by(Watched.Failed(i)))
        case Contradiction => Some(M.point(())) // already un-sat, no need to propagate contradiction
        case Anything => None
      })

    val n = x.size
    if (n == 1)
      x(0).set(true)
    else for {
      wr <- newCell(Watched(n - 2, n - 1))
      _ <- observe(wr).by(_ => sleep(untilRight((w: Watched, δ: Watched.Delta) => δ match {
        case Watched.ToWatch(is) => Left(Traverse[List].traverse_(is)(watch(_, wr)))
        case Watched.ToSatisfy(i) => Right(x(i).set(true))
        case Watched.DoNothing => Right(M.point(()))
      })))
      _ <- watch(n - 1, wr)
      _ <- watch(n - 2, wr)
    } yield ()
  }

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

// Auxiliary domain for implementing Two Watched Variables
private[bool] sealed abstract class Watched

private[bool] object Watched {
  case class Watching(i: Int, j: Int) extends Watched
  case object Done extends Watched

  def apply(i: Int, j: Int): Watched = {
    require(i < j)
    Watching(i, j)
  }

  sealed abstract class Update
  case class Failed(i: Int) extends Update
  case object Satisfied extends Update

  sealed abstract class Delta
  case class ToWatch(is: List[Int]) extends Delta
  case class ToSatisfy(i: Int) extends Delta
  case object DoNothing extends Delta

  implicit val domInstance: Dom.Aux[Watched, Update, Delta] = new Dom[Watched] {
    type Update = Watched.Update
    type Delta = Watched.Delta

    override def update[D0 <: Watched](d: D0, u: Update): UpdateResult[Watched, IDelta, D0] = d match {
      case Watching(i, j) => u match {
        case Satisfied => UpdateResult(Done, DoNothing)
        case Failed(k) =>
          assert(k == i || k == j)
          val l = i + j - k
          if(i == 0 || j == 0) UpdateResult(Done, ToSatisfy(l))
          else {
            val k1 = math.min(i, j) - 1
            UpdateResult(Watching(k1, l), ToWatch(k1 :: Nil))
          }
      }
      case Done => UpdateResult()
    }

    override def appendDeltas(d1: Delta, d2: Delta): Watched.Delta = (d1, d2) match {
      case (DoNothing, _) => DoNothing
      case (_, DoNothing) => DoNothing
      case (s @ ToSatisfy(_), _) => s
      case (_, s @ ToSatisfy(_)) => s
      case (ToWatch(is), ToWatch(js)) => ToWatch(is ++ js)
    }

    override def isFailed(d: Watched): Boolean = false
  }
}
