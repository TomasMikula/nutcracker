package nutcracker.toolkit

import nutcracker.rel.Relations

trait RelToolkit extends Toolkit {
  implicit val relationsApi: Relations[Prg]
}
