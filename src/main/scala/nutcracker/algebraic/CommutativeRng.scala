package nutcracker.algebraic

import scala.language.implicitConversions

import algebra.ring.MultiplicativeCommutativeSemigroup
import algebra.ring.Rng
import simulacrum.typeclass

@typeclass(excludeParents = List("Rng", "MultiplicativeCommutativeSemigroup"))
trait CommutativeRng[A] extends Any with Rng[A] with MultiplicativeCommutativeSemigroup[A]