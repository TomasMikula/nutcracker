package nutcracker.util

import org.scalatest.FunSuite

import scalaz.Id.Id
import scalaz.{NaturalTransformation}

class DeepEqualTest extends FunSuite {

  val listEqual: DeepEqual[List[Int], List[Int], Id, Id] = DeepEqual.listInstance[Id, Id, Int, Int]
  val setEqual: DeepEqual[Set[Int], Set[Int], Id, Id] = DeepEqual.setInstance[Id, Id, Int, Int]

  test("stack safety") {
    val l = (1 to 10000).toList

    val res = listEqual.deepEqual(l, l)(NaturalTransformation.refl[Id], NaturalTransformation.refl[Id])

    assert(res)
  }

  test("cycles") {
    class Lst(val i: Int, var tail: Lst = null)
    val l = new Lst(1)
    l.tail = l

    implicit val de: DeepEqual[Lst, Lst, Id, Id] = new DeepEqual[Lst, Lst, Id, Id] {
      def equal(l1: Lst, l2: Lst) =
        IsEqual[Id, Id, Int, Int](l1.i, l2.i) && IsEqual.refs[Id, Id, Lst, Lst](l1.tail, l2.tail)(this)
    }

    val res = de.deepEqual(l, l)(NaturalTransformation.refl[Id], NaturalTransformation.refl[Id])

    assert(res)
  }

  test("set") {
    val s1 = Set(1, 2, 3, 4)
    val s2 = Set(4, 3, 2, 1)

    val res = setEqual.deepEqual(s1, s2)(NaturalTransformation.refl[Id], NaturalTransformation.refl[Id])

    assert(res)
  }
}
