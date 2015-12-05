package nutcracker.util.free

import scala.language.higherKinds
import scalaz.\/

final case class CoproductK[F[_[_], _], G[_[_], _], K[_], A](run: F[K, A] \/ G[K, A]) {

}