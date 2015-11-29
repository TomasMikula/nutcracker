import algebra.lattice.GenBool
import nutcracker.ProblemDescription._

package object nutcracker {
  import acyclic.pkg

  def allDifferent[A, D: Domain[A, ?] : GenBool](doms: PureDomRef[A, D]*): ProblemDescription[Unit] = {
    val n = doms.size
    concat((0 until n) map { i => whenResolved(doms(i)){ a =>
      concat((0 until i) map { j => remove(doms(j))(a) }) >>
      concat((i+1 until n) map { j => remove(doms(j))(a) }) }
    })
  }

  private def concat(pds: Iterable[ProblemDescription[Unit]]): ProblemDescription[Unit] =
    pds.foldLeft[ProblemDescription[Unit]](Pure(())) { _ >> _ }

  private def remove[A, D: Domain[A, ?] : GenBool](ref: PureDomRef[A, D])(a: A): ProblemDescription[Unit] = {
    val d = Domain[A, D].singleton(a)
    fetch(ref) >>= { d0 => intersect(ref, GenBool[D].without(d0, d)) }
  }
}
