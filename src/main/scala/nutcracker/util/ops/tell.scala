package nutcracker.util.ops

import scala.language.implicitConversions
import scalaz.MonadTell

object tell extends ToTellOps

trait ToTellOps {
  implicit def tellInterpolator(sc: StringContext): TellInterpolator = TellInterpolator(sc)
}

final case class TellInterpolator(sc: StringContext) extends AnyVal {
  def tell[M[_]](args: M[Unit]*)(implicit M: MonadTell[M, String]): M[Unit] = {
    if (sc.parts.length != args.length + 1)
      throw new IllegalArgumentException(
        s"wrong number of arguments (${args.length}) for interpolated string with ${sc.parts.length} parts"
      )

    val ps = sc.parts.iterator.map(s => if(s.isEmpty) M.point(()) else M.tell(s))
    val as = args.iterator
    (ps interleave as).sequence_
  }
}