package nutcracker.util.free

import org.scalacheck.{Prop, Properties}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers

trait TestSuite extends AnyFunSuite with Checkers {

  private var context: String = ""

  protected def checkAll(props: Properties): Unit =
    for ((name, prop) <- props.properties) {
      test(context + ":" + name) { check(prop) }
    }


  implicit class StringOps(s: String) {
    def should[A](a: => Any): Unit = {
      val saved = context
      context = s; try a finally context = saved; ()
    }

    def ![A](a: => A)(implicit ev: (A) => Prop): Unit =
      test(context + ":" + s) { check(ev(a)) }
//
//    def in[A](a: => A)(implicit ev: (A) => Prop): Unit = property(context + ":" + s) = Prop { prms =>
//      ev(a).apply(prms) // TODO sort out the laziness / implicit conversions properly
//    }
  }
}