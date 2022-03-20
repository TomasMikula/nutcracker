package nutcracker.demo.simpletypeinference

package object kinds {

  /** Phantom type representing the kind of types. Unicode character U+25CF */
  sealed trait ●

  /** Phantom type representing a pair of kinds. Unicode character U+00D7. */
  sealed trait ×[K, L]

  /** Phantom type representing the "unit" kind. Neutral element for [[×]]. Unicode character U+25CB. */
  sealed trait ○

  /** Phantom type representing the kinds of type functions. Internal hom in the category [[TypeFun]].
   *
   * @tparam K input kind
   * @tparam L output kind
   */
  // sealed trait ->[K, L]

  sealed trait Kind[K] {
    def testEqual[L](that: Kind[L]): Option[K =:= L] =
      (this, that) match {
        case (Kind.Unit, Kind.Unit) =>
          Some(implicitly[○ =:= ○])
        case (Kind.Type, Kind.Type) =>
          Some(implicitly[● =:= ●])
        case (Kind.Prod(a, b), Kind.Prod(x, y)) =>
          (a testEqual x, b testEqual y) match {
            case (Some(ax), Some(by)) =>
              Some(implicitly[K =:= K].asInstanceOf[K =:= L])
            case _ =>
              None
          }
        case _ =>
          None
      }
  }

  object Kind {
    case object Unit extends Kind[○] {
      override def toString = "○"
    }
    case object Type extends Kind[●] {
      override def toString = "●"
    }
    case class Prod[K, L](k: Kind[K], l: Kind[L]) extends Kind[K × L] {
      override def toString = s"($k × $l)"
    }

    given Kind[○] = Unit
    given Kind[●] = Type
    given [K, L](using k: Kind[K], l: Kind[L]): Kind[K × L] = Prod(k, l)
    given [K](using k: OutputKind[K]): Kind[K] = k.kind

    def fst[K, L](kl: Kind[K × L]): Kind[K] =
      kl match {
        case Prod(k, l) => k
      }

    private def impossible(msg: String): Nothing =
      throw new AssertionError(msg)
  }

  /** Witnesses that `K` is a legal output kind of type functions. */
  sealed trait OutputKind[K] {
    def kind: Kind[K] =
      this match {
        case OutputKind.Type => Kind.Type
      }
  }
  object OutputKind {
    case object Type extends OutputKind[●] {
      override def toString = "●"
    }

    given OutputKind[●] = Type
  }
}
