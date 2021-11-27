package nutcracker.util

sealed trait Nat {
  type N <: Nat
}

object Nat {
  object _0 extends Nat {
    type N = _0.type
  }
  type _0 = _0.N

  case class Succ[N0 <: Nat](n: N0) extends Nat {
    type N = Succ[n.N]
  }

  val _1: Succ[_0] = Succ(_0)
  type _1 = _1.N

  val _2: Succ[_1] = Succ(_1)
  type _2 = _2.N

  sealed trait ToInt[N <: Nat] {
    def apply(): Int
  }

  object ToInt {
    implicit val zeroToInt: ToInt[_0] =
      new ToInt[_0] {
        override def apply(): Int = 0
      }

    implicit def succToInt[N <: Nat](implicit nToInt: ToInt[N]): ToInt[Succ[N]] =
      new ToInt[Succ[N]] {
        override def apply(): Int = 1 + nToInt()
      }
  }
}
