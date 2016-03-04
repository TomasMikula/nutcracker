package nutcracker.util

import scala.language.higherKinds

package object free {
  type ConstK[A, K[_]] = A
}
