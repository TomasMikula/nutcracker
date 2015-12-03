package nutcracker

import nutcracker.BranchLang.{AddBranching, BranchT}
import nutcracker.util.free.Interpreter
import nutcracker.util.free.Interpreter.AlwaysClean

import scala.language.higherKinds
import scalaz.{Applicative, \/-, -\/, \/}
import scalaz.syntax.applicative._

class BranchStore[F[_], K[_]](private val branches: List[BranchT[F, K, Unit]]) extends AnyVal {

  private def addBranching(b: BranchT[F, K, Unit]) = new BranchStore(b::branches)
}

object BranchStore {
  def interpreter[F[_]]: Interpreter[BranchLang[F, ?[_], ?], BranchStore[F, ?[_]], AlwaysClean] =
    new Interpreter[BranchLang[F, ?[_], ?], BranchStore[F, ?[_]], AlwaysClean] {

      def step[K[_]: Applicative, A](p: BranchLang[F, K, A])(s: BranchStore[F, K]): (BranchStore[F, K], AlwaysClean[K], K[A]) = {
        p match {
          case AddBranching(b) => (s.addBranching(b), (), ().point[K])
        }
      }

      def uncons[K[_]: Applicative](w: AlwaysClean[K])(s: BranchStore[F, K]) = None
    }
}