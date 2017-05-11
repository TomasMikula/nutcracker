package nutcracker.toolkit

sealed trait DeferLang[D, K[_], A]

object DeferLang {
  case class Delay[D, K[_]](delay: D, k: K[Unit]) extends DeferLang[D, K, Unit]
  case class Exec[D, K[_]]() extends DeferLang[D, K, Unit]

  def defer[D, K[_]](delay: D, k: K[Unit]): DeferLang[D, K, Unit] =
    Delay(delay, k)

  def exec[D, K[_]](): DeferLang[D, K, Unit] =
    Exec()
}