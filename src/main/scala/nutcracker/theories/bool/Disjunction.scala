package nutcracker.theories.bool

import shapeless.Nat
import shapeless.Sized
import nutcracker.Constraint
import nutcracker.Domain

case class Disjunction[N <: Nat](signs: Sized[Vector[Boolean], N]) extends Constraint[Sized[Vector[BoolDomain], N]] {

  def enforce(domains: Sized[Vector[BoolDomain], N]): Sized[Vector[BoolDomain], N] = {
    val D = implicitly[Domain[Boolean, BoolDomain]]
    val signedDomains = Sized.wrap[Vector[(Boolean, BoolDomain)], N](signs zip domains)
    val nSatisfiable = signedDomains count { case (sign, d) => !D.isEmpty(D.meet(d, D.singleton(sign))) }

    if(nSatisfiable == 1)
      // if there is only a single domain that can be satisfied, satisfy it
      signedDomains map { case(sign, d) => if(!D.isEmpty(D.meet(d, D.singleton(sign)))) D.singleton(sign) else d }
    else if(nSatisfiable == 0)
      // no variable satisfiable; make sure at least one is failed
      Sized.wrap(domains.updated(0, BoolDomain.Bottom))
    else
      domains
  }

}