package nutcracker

import nutcracker.BranchLang.{AddBranching}
import nutcracker.util.free.Interpreter
import nutcracker.util.free.Interpreter.AlwaysClean

import scala.language.higherKinds
import scalaz.{Applicative, \/-, -\/, \/}
import scalaz.syntax.applicative._

class BranchStore[F[_], K[_]](val branches: List[F[K[Unit]]]) extends AnyVal {

  private def addBranching(b: F[K[Unit]]) = new BranchStore(b::branches)
}

object BranchStore {

  def empty[F[_], K[_]] = new BranchStore[F, K](List())

  implicit def interpreter[F[_]]: Interpreter[BranchLang[F, ?[_], ?], BranchStore[F, ?[_]], AlwaysClean] =
    new Interpreter[BranchLang[F, ?[_], ?], BranchStore[F, ?[_]], AlwaysClean] {

      def step[K[_]: Applicative, A](p: BranchLang[F, K, A])(s: BranchStore[F, K]): (BranchStore[F, K], AlwaysClean[K], K[A]) = {
        p match {
          case AddBranching(b) => (s.addBranching(b), (), ().point[K])
        }
      }

      def uncons[K[_]: Applicative](w: AlwaysClean[K])(s: BranchStore[F, K]) = None
    }
}