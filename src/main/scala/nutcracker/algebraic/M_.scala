package nutcracker.algebraic

import algebra.Eq
import algebra.lattice.{Bool, BoundedLattice}
import algebra.std.boolean._

import M_._

/**
  * A trivial lattice created by adjoining Top and Bottom to any type.
  */
sealed trait M_[A]

object M_ extends M_Instances0 {
  case class Bottom[A]() extends M_[A]
  case class Top[A]() extends M_[A]
  case class Just[A](value: A) extends M_[A]
}

trait M_Instances0 extends M_Instances1 {
  implicit def mBoolean: Bool[M_[Boolean]] = new M_Bool
}

trait M_Instances1 {
  implicit def lattice[A: Eq]: BoundedLattice[M_[A]] with Eq[M_[A]] = new M_Lattice[A]
}

private[algebraic] class M_Lattice[A: Eq] extends BoundedLattice[M_[A]] with Eq[M_[A]] {
  def one: M_[A] = Top()
  def zero: M_[A] = Bottom()

  def meet(a: M_[A], b: M_[A]): M_[A] = (a, b) match {
    case (Top(), y) => y
    case (x, Top()) => x
    case (Bottom(), _) => Bottom()
    case (_, Bottom()) => Bottom()
    case (Just(x), Just(y)) => if(Eq[A].eqv(x, y)) Just(x) else Bottom()
  }

  def join(a: M_[A], b: M_[A]): M_[A] = (a, b) match {
    case (Top(), _) => Top()
    case (_, Top()) => Top()
    case (Bottom(), y) => y
    case (x, Bottom()) => x
    case (Just(x), Just(y)) => if(Eq[A].eqv(x, y)) Just(x) else Top()
  }

  def eqv(a: M_[A], b: M_[A]): Boolean = (a, b) match {
    case (Top(), Top()) => true
    case (Bottom(), Bottom()) => true
    case (Just(x), Just(y)) => Eq[A].eqv(x, y)
    case _ => false
  }
}

private[algebraic] class M_Bool extends M_Lattice[Boolean] with Bool[M_[Boolean]] {
  def and(a: M_[Boolean], b: M_[Boolean]): M_[Boolean] = meet(a, b)

  def or(a: M_[Boolean], b: M_[Boolean]): M_[Boolean] = join(a, b)

  def complement(a: M_[Boolean]): M_[Boolean] = a match {
    case Top() => Bottom()
    case Bottom() => Top()
    case Just(x) => Just(!x)
  }
}