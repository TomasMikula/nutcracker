package nutcracker.demo.simpletypeinference

/** The usual type-level fixed-point. */
case class Fix[F[_]](unfix: F[Fix[F]])