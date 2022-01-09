package nutcracker.toolkit

import nutcracker.Assessment.{Done, Incomplete, Stuck}
import nutcracker.Splittable._
import nutcracker.util.KMap
import nutcracker.{Assessment, Propagation, Splittable}
import scalaz.Id.Id
import scalaz.~>

private[nutcracker] case class BranchStore[Ref[_], K[_]](
  unresolvedVars: KMap[Ref, Splittable],
  failedVars: Set[Ref[Nothing]]
) {
  def addUnresolved[D](ref: Ref[D], ev: Splittable[D]): BranchStore[Ref, K] =
    copy[Ref, K](unresolvedVars = unresolvedVars.put(ref)(ev))

  def removeUnresolved[D](ref: Ref[D]): BranchStore[Ref, K] =
    copy[Ref, K](unresolvedVars = unresolvedVars - ref)

  def addFailed[D](ref: Ref[D]): BranchStore[Ref, K] =
    copy[Ref, K](failedVars = failedVars + ref.asInstanceOf[Ref[Nothing]])

  def split[Val[_]](fetch: Ref ~> Id)(implicit K: Propagation.Aux[K, Ref, Val]): Assessment[List[K[Unit]]] =
    if(unresolvedVars.isEmpty) Done
    else {
      def splitDomain[D](ref: Ref[D])(implicit ev: Splittable[D]): Option[List[K[Unit]]] = {
        val d = fetch(ref)
        ev.assess(d) match {
          case Unrefined(choices) => choices() map { _ map { ui => K.updateImpl[D, ev.Update, ev.Delta](ref)(ui)(ev) } }
          case _ => sys.error("splitDomain should be called on unresolved variables only.")
        }
      }

      unresolvedVars.iterator.map(p => splitDomain(p._1)(p._2)).collectFirst({
        case Some(branches) => Incomplete(branches)
      }).getOrElse(Stuck)
    }

  def hasFailedVars: Boolean = failedVars.nonEmpty
}

object BranchStore {
  def apply[Ref[_], K[_]](): BranchStore[Ref, K] =
    BranchStore[Ref, K](KMap[Ref, Splittable](), Set.empty[Ref[Nothing]])
}