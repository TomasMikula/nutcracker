package nutcracker

import nutcracker.util.HList

abstract class Constraint[L <: HList, K[_]](val rel: Rel[L]) {

  def setup(l: L): K[Unit]

}