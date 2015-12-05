import scala.language.higherKinds

import algebra.lattice.GenBool
import nutcracker.ProblemDescription._

package object nutcracker {
  import acyclic.pkg

  type Assessor[S, U] = S => Assessment[U]
  type Advancer[S, U, F[_], K[_]] = (S, U) => F[(S, K[Unit])]

  type AssessAdvance[S, F[_], K[_]] = S => Assessment[F[(S, K[Unit])]]

  def allDifferent[A, D: Domain[A, ?] : GenBool](doms: DomRef[A, D]*): ProblemDescription[Unit] = {
    val n = doms.size
    concat((0 until n) map { i => whenResolved(doms(i)){ a =>
      concat((0 until i) map { j => remove(doms(j))(a) }) >>
      concat((i+1 until n) map { j => remove(doms(j))(a) }) }
    })
  }
}
