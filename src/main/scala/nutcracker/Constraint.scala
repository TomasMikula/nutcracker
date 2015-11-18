package nutcracker

trait Constraint[A] {
  def enforce(a: A): A
}