package nutcracker.toolkit

import nutcracker.Relations

trait RelToolkit extends Toolkit {
  implicit val relationsApi: Relations[Prg]
}
