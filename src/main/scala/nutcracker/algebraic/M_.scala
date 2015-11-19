package nutcracker.algebraic

import algebra.Eq
import algebra.lattice.BoundedLattice

/**
  * A trivial lattice created by adjoining Top and Bottom to any type.
  */
sealed trait M_[A]
case class Bottom[A]() extends M_[A]
case class Top[A]() extends M_[A]
case class Just[A](value: A) extends M_[A]

object M_ {
  implicit def lattice[A: Eq]: BoundedLattice[M_[A]] with Eq[M_[A]] = new BoundedLattice[M_[A]] with Eq[M_[A]] {
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
}