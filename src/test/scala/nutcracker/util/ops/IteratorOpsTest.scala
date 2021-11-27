package nutcracker.util.ops

import nutcracker.util.ops.Ops._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import scala.annotation.tailrec
import scalaz.Semigroup

class IteratorOpsTest extends AnyFunSuite {

  private sealed trait Tree {
    def depth: Int = this match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + math.max(l.depth, r.depth)
    }
    def toList: List[Int] = {
      @tailrec def go(init: List[Tree], acc: List[Int]): List[Int] = init match {
        case t :: ts => t match {
          case Branch(l, r) => go(r :: l :: ts, acc)
          case Leaf(x) => go(ts, x :: acc)
        }
        case Nil => acc
      }

      go(List(this), Nil)
    }
  }
  private case class Leaf(value: Int) extends Tree
  private case class Branch(l: Tree, r: Tree) extends Tree

  private object Tree {
    implicit val treeSemigroup: Semigroup[Tree] = new Semigroup[Tree] {
      override def append(f1: Tree, f2: => Tree) = Branch(f1, f2)
    }
  }

  test("balanced reduction") {
    val n = 1000000
    val it = Iterator.range(0, n).map[Tree](Leaf)
    val res = it.balancedReduce.get

    def log2ceil(x: Int): Int = math.ceil(math.log(x.toDouble) / math.log(2)).toInt

    // max depth is logarithmic
    res.depth should be <= 1 + log2ceil(n)

    // order preservation
    res.toList should be (List.range(0, n))
  }
}
