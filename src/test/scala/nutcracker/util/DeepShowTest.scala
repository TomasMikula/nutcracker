package nutcracker.util

import nutcracker.util.FreeObjectOutput.Decoration
import org.scalatest.funsuite.AnyFunSuite

import scalaz.{NaturalTransformation, ~>}
import scalaz.Id.Id
import scalaz.syntax.monad._

class DeepShowTest extends AnyFunSuite {

  val idShowK: ShowK[Id] = new ShowK[Id] {
    def shows[A](fa: Id[A]): String = System.identityHashCode(fa).formatted("%x")
  }

  val listShow: DeepShow[List[Int], Id] = new DeepShow.FromSerialize[List[Int], Id] {
    def serialize[M[_]](is: List[Int])(implicit ev: MonadObjectOutput[M, String, Id]): M[Unit] =
      is match {
        case Nil => ev.empty
        case i :: Nil => ev.write(i.toString)
        case i :: is => ev.write(i.toString) >> ev.write(",") >> ev.writeObject(is)(this)
      }
  }

  test("stack safety") {
    val l = (1 to 10000).toList

    val res = listShow.free(l).showAutoLabeled(NaturalTransformation.refl[Id], idShowK)()
    val expected = l.mkString(",")

    assertResult(expected)(res)
  }

  test("cycles") {
    class Lst(val i: Int, var tail: Lst = null)
    val l = new Lst(1)
    l.tail = l

    implicit val ds: DeepShow[Lst, Id] = new DeepShow.FromSerialize[Lst, Id] {
      def serialize[M[_]](is: Lst)(implicit ev: MonadObjectOutput[M, String, Id]): M[Unit] =
        ev.write(is.i.toString + ",") >> ev.writeObject(is.tail)(this)
    }

    val s = ds.free(l).showAutoLabeled(NaturalTransformation.refl[Id], idShowK)(
      decorateReferenced = λ[Id ~> λ[α => Decoration[String]]](ref => Decoration("", "")),
      decorateReference = ref => "@"
    )

    val expected = "1,1,@"

    assertResult(expected)(s)
  }

  test("tree") {
    val l = List(1, 2, 3, 4)

    val res1 = listShow.free(l).printTree(NaturalTransformation.refl[Id], idShowK, lineLimit = 1, tab = "  ", newLine = "\n")()
    val expected1 = """1
                     |,
                     |  2
                     |  ,
                     |    3
                     |    ,
                     |      4""".stripMargin
    assertResult(expected1)(res1)

    val res2 = listShow.free(l).printTree(NaturalTransformation.refl[Id], idShowK, lineLimit = 4, tab = "  ", newLine = "\n")()
    val expected2 = """1
                      |,
                      |  2
                      |  ,
                      |    3,4""".stripMargin
    assertResult(expected2)(res2)
  }
}
