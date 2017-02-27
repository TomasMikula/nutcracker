package nutcracker

trait BranchingBundle extends RefBundle with StashBundle {
  implicit def branchingApi: BranchingPropagation[Prg, Ref]
  implicit def stashRestore[K[_]]: StashRestore[State[K]]

  def assess(s: State[Prg]): Assessment[List[Prg[Unit]]]
}
