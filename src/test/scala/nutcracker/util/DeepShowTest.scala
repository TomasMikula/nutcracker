package nutcracker.util

import nutcracker.util.FreeObjectOutput.Decoration
import org.scalatest.FunSuite

import scalaz.{NaturalTransformation, ~>}
import scalaz.Id.Id

class DeepShowTest extends FunSuite {

  val idShowK: ShowK[Id] = new ShowK[Id] {
    def shows[A](fa: Id[A]): String = System.identityHashCode(fa).formatted("%x")
  }

  val listShow: DeepShow[List[Int], Id] = new DeepShow.FromFree[List[Int], Id] {
    def free(is: List[Int]): Desc[Id] = is match {
      case Nil => Desc.done("")
      case i :: Nil => Desc.done(i.toString)
      case i :: is => Desc.done[Id](i.toString) ++ Desc.done(",") ++ Desc.ref[Id, List[Int]](is)(this)
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

    implicit val ds: DeepShow[Lst, Id] = new DeepShow.FromFree[Lst, Id] {
      def free(is: Lst): Desc[Id] =
        Desc.done(is.i.toString + ",") ++ Desc.ref[Id, Lst](is.tail)(this)
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
