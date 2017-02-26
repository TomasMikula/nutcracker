package nutcracker

trait Module {
  type Lang[K[_], A]
  type State[K[_]]

  def empty[K[_]]: State[K]
}
