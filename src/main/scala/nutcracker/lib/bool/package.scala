package nutcracker.lib

import nutcracker.{DRef, Diff, DomWithBottom, Join}
import scalaz.\/

package object bool {

  type BoolDom = DomWithBottom.Aux[BoolDomain, Join[BoolDomain] \/ Diff[BoolDomain], Unit]
  type BoolRef = DRef[BoolDomain]

}