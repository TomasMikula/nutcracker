package nutcracker.util

import scalaz.{-\/, BindRec, Comonad, Monad, \/, \/-}

case class Id[A](value: A)

object Id {
  implicit val id: Monad[Id] with BindRec[Id] with Comonad[Id] =
    new Monad[Id] with BindRec[Id] with Comonad[Id] {
      @scala.annotation.tailrec
      def tailrecM[A, B](a: A)(f: A => Id[A \/ B]): Id[B] =
        f(a).value match {
          case -\/(a) => tailrecM(a)(f)
          case \/-(b) => Id(b)
        }

      def copoint[A](a: Id[A]): A =
        a.value

      def point[A](a: => A): Id[A] =
        Id(a)

      def cobind[A, B](fa: Id[A])(f: Id[A] => B): Id[B] =
        Id(f(fa))

      def bind[A, B](fa: Id[A])(f: A => Id[B]): Id[B] =
        f(fa.value)
    }

  /** Reference comparison of the wrapped values.
    * For value types might return `false` even for equal values,
    * since they must be boxed before reference comparison can take place.
    */
  implicit val referenceEquality: HEqualK[Id] =
    new HEqualK[Id] {
      def hEqualK[A, B](fa: Id[A], fb: Id[B]): Boolean = {
        val a: A = fa.value
        val b: B = fb.value
        a.asInstanceOf[AnyRef] eq b.asInstanceOf[AnyRef]
      }
    }
}
