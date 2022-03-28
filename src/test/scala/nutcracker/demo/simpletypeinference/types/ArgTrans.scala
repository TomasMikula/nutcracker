package nutcracker.demo.simpletypeinference.types

import nutcracker.demo.simpletypeinference.kinds._

/** Transforms type arguments from kind `K` to kind `L` by transformation functions `F[x, y]`.
 *
 * @tparam K the kind before transforming (some of the) type arguments
 * @tparam L the kind after transforming (some of the) type arguments
 * @tparam F the type of transformations, parameterized by kinds
 */
sealed trait ArgTrans[F[_, _], K, L] {
  import ArgTrans._

  given inKind: Kind[K]
  given outKind: ProperKind[L]

  def fst[M](using ProperKind[K], ProperKind[M]): ArgTrans[F, K × M, L × M] =
    ArgTrans.fst(this)

  def snd[J](using ProperKind[J], ProperKind[K]): ArgTrans[F, J × K, J × L] =
    ArgTrans.snd(this)
}

object ArgTrans {
  case class Wrap[F[_, _], K, L](f: F[K, L])(using
    k: Kind[K],
    val outputKind: OutputKind[L],
  ) extends ArgTrans[F, K, L] {
    override def inKind: Kind[K] = k
    override def outKind: ProperKind[L] = outputKind.properKind
  }

  case class Fst[F[_, _], K: ProperKind, L, M: ProperKind](f: ArgTrans[F, K, L]) extends ArgTrans[F, K × M, L × M] {
    override def inKind: Kind[K × M] = (ProperKind[K] × ProperKind[M]).kind
    override def outKind: ProperKind[L × M] = f.outKind × ProperKind[M]
  }

  case class Snd[F[_, _], K: ProperKind, L: ProperKind, M](f: ArgTrans[F, L, M]) extends ArgTrans[F, K × L, K × M] {
    override def inKind: Kind[K × L] = (ProperKind[K] × ProperKind[L]).kind
    override def outKind: ProperKind[K × M] = ProperKind[K] × f.outKind

    def in1Kind: ProperKind[K] = ProperKind[K]
    def in2Kind: ProperKind[L] = ProperKind[L]
    def out2Kind: ProperKind[M] = f.outKind
  }

  def apply[F[_, _], K: Kind, L: OutputKind](f: F[K, L]): ArgTrans[F, K, L] =
    wrap(f)

  def wrap[F[_, _], K: Kind, L: OutputKind](f: F[K, L]): ArgTrans[F, K, L] =
    Wrap(f)

  def fst[F[_, _], K: ProperKind, L, M: ProperKind](f: ArgTrans[F, K, L]): ArgTrans[F, K × M, L × M] =
    Fst(f)

  def snd[F[_, _], K: ProperKind, L: ProperKind, M](f: ArgTrans[F, L, M]): ArgTrans[F, K × L, K × M] =
    Snd(f)

  def unwrap[F[_, _], K, L](f: ArgTrans[F, K, L])(using l: OutputKind[L]): F[K, L] =
    f match {
      case Wrap(f) =>
        f
      case other =>
        // TODO: prove using OutputKind[K], instead of throwing an exception
        throw new AssertionError(s"didn't expect $f to produce an OutputKind")
    }
}
