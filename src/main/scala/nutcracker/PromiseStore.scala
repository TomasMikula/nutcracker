package nutcracker

import nutcracker.PromiseLang._
import nutcracker.util.free.Interpreter
import nutcracker.util.free.Interpreter.{CleanInterpreter, AlwaysClean}

import scala.collection.immutable.LongMap
import scala.language.higherKinds
import scalaz._
import scalaz.std.list._
import scalaz.syntax.applicative._

final case class PromiseStore[K[_]](
    nextId: Long,
    promises: LongMap[Option[_]],
    continuations: LongMap[List[_ => K[Unit]]]) {

  def promise[A]: (PromiseStore[K], Promised[A]) =
    (copy(nextId = nextId + 1, promises = promises + ((nextId, Option.empty))), Promised(nextId))

  def complete[A](p: Promised[A], a: A)(implicit K: Applicative[K]): (PromiseStore[K], K[Unit]) = {
    val conts = continuations.getOrElse(p.id, Nil).asInstanceOf[List[A => K[Unit]]] map { _(a) }
    val cont = Foldable[List].sequence_(conts)
    (copy(promises = promises + ((p.id, Some(a))), continuations = continuations - p.id), cont)
  }

  def addOnComplete[A](p: Promised[A], f: A => K[Unit])(implicit K: Applicative[K]): (PromiseStore[K], K[Unit]) =
    promises.get(p.id).asInstanceOf[Option[A]] match {
      case Some(a) => (this, f(a))
      case None => (copy(continuations = continuations + ((p.id, f :: continuations.getOrElse(p.id, Nil)))), K.pure(()))
    }

  def apply[A](pr: Promised[A]): Option[A] = promises(pr.id).asInstanceOf[Option[A]]
}

object PromiseStore {

  def empty[K[_]] = new PromiseStore[K](0L, LongMap(), LongMap())

  implicit def interpreter: Interpreter[PromiseLang, PromiseStore, AlwaysClean] =
    new CleanInterpreter[PromiseLang, PromiseStore] {

      def step0[K[_]: Applicative, A](p: PromiseLang[K, A])(s: PromiseStore[K]): (PromiseStore[K], K[A]) = {
        p match {
          case Promise() => // A =:= Promised[X] forSome { type X }
            val promised: (PromiseStore[K], A) = s.promise
            (promised._1, promised._2.point[K])
          case Complete(pr, x) => // A =:= Unit, (pr, x): (Promised[X], X) forSome { type X }
            s.complete(pr, x)
          case OnComplete(pr, f) => // A =:= Unit, (pr, f): (Promised[X], X => K[Unit]) forSome { type X }
            s.addOnComplete(pr, f)
        }
      }

    }
}
