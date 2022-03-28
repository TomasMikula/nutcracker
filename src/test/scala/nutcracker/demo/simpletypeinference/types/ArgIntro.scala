package nutcracker.demo.simpletypeinference.types

import nutcracker.demo.simpletypeinference.kinds._

/** Transforms type arguments from kind `K` into `L` by introducing additional type arguments.
 *  Used as a vehicle to deliver (partial) type arguments into a type function `L ->> M`.
 *
 * @tparam K the kind before adding type arguments
 * @tparam L the kind after adding type arguments (i.e. `K` interspersed with additional type arguments)
 * @tparam TA the type of type arguments, parameterized by kind
 */
sealed trait ArgIntro[TA[_], K, L] {
  import ArgIntro._

  given inKind: Kind[K]
  given outKind: ProperKind[L]
}

object ArgIntro {
  case class WrapArg[TA[_], K](arg: TA[K])(using
    val outputKind: OutputKind[K],
  ) extends ArgIntro[TA, ○, K] {
    override def inKind: Kind[○] = summon[Kind[○]]
    override def outKind: ProperKind[K] = outputKind.properKind
  }

  case class IntroFst[TA[_], K, L: ProperKind](args: ArgIntro[TA, ○, K]) extends ArgIntro[TA, L, K × L] {
    override def inKind: Kind[L]            = ProperKind[L].kind
    override def outKind: ProperKind[K × L] = args.outKind × ProperKind[L]
  }

  case class IntroSnd[TA[_], K: ProperKind, L](args: ArgIntro[TA, ○, L]) extends ArgIntro[TA, K, K × L] {
    override def inKind: Kind[K]            = ProperKind[K].kind
    override def outKind: ProperKind[K × L] = ProperKind[K] × args.outKind
  }

  case class IntroBoth[TA[_], K, L](
    args1: ArgIntro[TA, ○, K],
    args2: ArgIntro[TA, ○, L],
  ) extends ArgIntro[TA, ○, K × L] {
    override def inKind: Kind[○] = summon[Kind[○]]
    override def outKind: ProperKind[K × L] = args1.outKind × args2.outKind
  }

  def wrapArg[TA[_], K: OutputKind](arg: TA[K]): ArgIntro[TA, ○, K] =
    WrapArg(arg)

  def introFst[TA[_], K, L: ProperKind](args: ArgIntro[TA, ○, K]): ArgIntro[TA, L, K × L] =
    IntroFst(args)

  def introSnd[TA[_], K: ProperKind, L](args: ArgIntro[TA, ○, L]): ArgIntro[TA, K, K × L] =
    IntroSnd(args)

  def introBoth[TA[_], K, L](args1: ArgIntro[TA, ○, K], args2: ArgIntro[TA, ○, L]): ArgIntro[TA, ○, K × L] =
    IntroBoth(args1, args2)

  def wrapArgFst[TA[_], K: OutputKind, L: ProperKind](arg: TA[K]): ArgIntro[TA, L, K × L] =
    introFst(wrapArg(arg))

  def wrapArgSnd[TA[_], K: ProperKind, L: OutputKind](arg: TA[L]): ArgIntro[TA, K, K × L] =
    introSnd(wrapArg(arg))

  def unwrap[TA[_], K](i: ArgIntro[TA, ○, K])(using k: OutputKind[K]): TA[K] =
    i match {
      case WrapArg(a) =>
        a
      case other =>
        // TODO: prove using OutputKind[K], instead of throwing an exception
        throw new AssertionError(s"didn't expect $i to produce an OutputKind")
    }
}
