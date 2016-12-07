package nutcracker.util

import nutcracker.util.DeepShow.Desc
import org.scalatest.FunSuite

import scalaz.{Free, NaturalTransformation}
import scalaz.Id.Id

class DeepShowTest extends FunSuite {

  implicit val idShowK: ShowK[Id] = new ShowK[Id] {
    def shows[A](fa: Id[A]): String = System.identityHashCode(fa).formatted("%x")
  }

  test("stack safety") {
    val l = (1 to 10000).toList

    implicit val ds: DeepShow[List[Int], Id] = new DeepShow[List[Int], Id] {
      def show(is: List[Int]): Free[Desc[Id, ?], String] = is match {
        case Nil => Free.pure("")
        case i :: Nil => Free.pure(i.toString)
        case i :: is => Free.liftF(Desc[Id, List[Int]](is)(this)) map { tail => s"$i, $tail" }
      }
    }

    val res = ds.deepShow(l)(NaturalTransformation.refl[Id])()
    val expected = l.mkString(", ")

    assertResult(expected)(res)
  }

  test("cycles") {
    class Lst(val i: Int, var tail: Lst = null)
    val l = new Lst(1)
    l.tail = l

    implicit val ds: DeepShow[Lst, Id] = new DeepShow[Lst, Id] {
      def show(is: Lst): Free[Desc[Id, ?], String] =
        Free.liftF(Desc[Id, Lst](is.tail)(this)) map { tail => s"${is.i}, $tail" }
    }

    val s = ds.deepShow(l)(NaturalTransformation.refl[Id])(
      decorateReferenced = (str, ref) => str,
      decorateReference = ref => "@"
    )

    val expected = "1, 1, @"

    assertResult(expected)(s)
  }
}
