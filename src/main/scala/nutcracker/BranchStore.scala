package nutcracker

import nutcracker.BranchLang._
import nutcracker.util.free.Interpreter
import nutcracker.util.free.Interpreter.{CleanInterpreter, AlwaysClean}

import scala.language.higherKinds
import scalaz.{Applicative, \/-, -\/, \/}
import scalaz.syntax.applicative._

class BranchStore[F[_], K[_]](val branches: List[F[K[Unit]]]) extends AnyVal {

  private def addBranching(b: F[K[Unit]]) = new BranchStore(b::branches)
}

object BranchStore {

  def empty[F[_], K[_]] = new BranchStore[F, K](List())

  implicit def interpreter[F[_]]: Interpreter[BranchLang[F, ?[_], ?], BranchStore[F, ?[_]], AlwaysClean] =
    new CleanInterpreter[BranchLang[F, ?[_], ?], BranchStore[F, ?[_]]] {

      def step0[K[_]: Applicative, A](p: BranchLang[F, K, A])(s: BranchStore[F, K]): (BranchStore[F, K], K[A]) = {
        p match {
          case AddBranching(b) => (s.addBranching(b), ().point[K])
        }
      }

    }
}