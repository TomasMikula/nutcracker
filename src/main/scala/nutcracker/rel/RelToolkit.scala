package nutcracker.rel

import nutcracker.Toolkit

trait RelToolkit extends Toolkit {
  implicit val relationsApi: Relations[Prg]
}
