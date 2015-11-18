package nutcracker.algebraic

import scala.language.implicitConversions

import simulacrum.typeclass

@typeclass trait BoolRing[A] extends Any with BoolRng[A] {
  import BoolRng.ops._
  implicit def boolRing: BoolRing[A] = this

  def one: A
  def complement(a: A): A = one \ a

  def top: A = one
  def ⊤ : A = one
}