package nutcracker.util.free

import nutcracker.util.free.Interpreter.AlwaysClean

import scala.language.higherKinds

import nutcracker.util.free.PromiseLang._
import scala.collection.immutable.LongMap
import scalaz._
import scalaz.syntax.applicative._

final class PromiseStore[K[_]](
    nextId: Long,
    promises: LongMap[Option[_]],
    continuations: LongMap[List[_ => K[Unit]]]) {

  def promise[A]: (PromiseStore[K], Promised[A]) = ???
  def complete[A](p: Promised[A], a: A): (PromiseStore[K], K[Unit]) = ???
  def addOnComplete[A](p: Promised[A], f: A => K[Unit]): (PromiseStore[K], K[Unit]) = ???
}

object PromiseStore {

  def interpreter: Interpreter[PromiseLang, PromiseStore, AlwaysClean] =
    new Interpreter[PromiseLang, PromiseStore, AlwaysClean] {

      def step[K[_]: Applicative, A](p: PromiseLang[K, A])(s: PromiseStore[K]): (PromiseStore[K], AlwaysClean[K], K[A]) = {
        p match {
          case Promise() => // A =:= Promised[X] forSome { type X }
            val promised: (PromiseStore[K], A) = s.promise
            (promised._1, (), promised._2.point[K])
          case Complete(pr, x) => // A =:= Unit, (pr, x): (Promised[X], X) forSome { type X }
            val completed: (PromiseStore[K], K[Unit]) = s.complete(pr, x)
            (completed._1, (), completed._2)
          case OnComplete(pr, f) => // A =:= Unit, (pr, f): (Promised[X], X => K[Unit]) forSome { type X }
            val onCompleteAdded: (PromiseStore[K], K[Unit]) = s.addOnComplete(pr, f)
            (onCompleteAdded._1, (), onCompleteAdded._2)
        }
      }

      def uncons[K[_]: Applicative](w: AlwaysClean[K])(s: PromiseStore[K]) = None
    }
}
