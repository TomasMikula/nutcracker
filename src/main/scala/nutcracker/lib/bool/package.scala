package nutcracker.lib

import nutcracker.{Diff, DomWithBottom, Join}
import scalaz.\/

package object bool {

  type BoolDom = DomWithBottom.Aux[BoolDomain, Join[BoolDomain] \/ Diff[BoolDomain], Unit]

}