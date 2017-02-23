package nutcracker

import nutcracker.Assessment.{Done, Incomplete, Stuck}
import nutcracker.Splittable._
import nutcracker.util.KMap

import scalaz.Id.Id
import scalaz.~>

private[nutcracker] case class BranchStore[Ref[_], K[_]](
  unresolvedVars: KMap[Ref, Splittable]
) {
  def addVar[D](ref: Ref[D], ev: Splittable[D]): BranchStore[Ref, K] =
    copy(unresolvedVars = unresolvedVars.put(ref)(ev))

  def removeVar[D](ref: Ref[D]): BranchStore[Ref, K] =
    copy(unresolvedVars = unresolvedVars - ref)

  def split(fetch: Ref ~> Id)(implicit K: Propagation[K, Ref]): Assessment[List[K[Unit]]] =
    if(unresolvedVars.isEmpty) Done
    else {
      def splitDomain[D](ref: Ref[D])(implicit ev: Splittable[D]): Option[List[K[Unit]]] = {
        val d = fetch(ref)
        ev.assess(d) match {
          case Unrefined(choices) => choices() map { _ map { ui => K.updateImpl[D, ev.Update, ev.IDelta](ref)(ui)(ev) } }
          case _ => sys.error("splitDomain should be called on unresolved variables only.")
        }
      }

      unresolvedVars.toStream.map(p => splitDomain(p._1)(p._2)).collectFirst({
        case Some(branches) => Incomplete(branches)
      }).getOrElse(Stuck)
    }
}

object BranchStore {
  def apply[Ref[_], K[_]](): BranchStore[Ref, K] = BranchStore(KMap())
}