package nutcracker.demo.simpletypeinference.types

import nutcracker.demo.simpletypeinference.kinds._

/** Introduces type arguments into `K`, obtaining `L`.
 *  Used as a vehicle to deliver type arguments into a type function `L ->> M`.
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
  case class WrapArg[TA[_], K: ProperKind](arg: TA[K]) extends ArgIntro[TA, ○, K] {
    override def inKind: Kind[○] = summon[Kind[○]]
    override def outKind: ProperKind[K] = summon[ProperKind[K]]
  }

  case class IntroFst[TA[_], K, L: ProperKind](args: ArgIntro[TA, ○, K]) extends ArgIntro[TA, L, K × L] {
    import args.outKind

    override def inKind: Kind[L] = summon[Kind[L]]
    override def outKind: ProperKind[K × L] = summon[ProperKind[K × L]]
  }

  def wrapArg[TA[_], K: ProperKind](arg: TA[K]): ArgIntro[TA, ○, K] =
    WrapArg(arg)

  def introFst[TA[_], K, L: ProperKind](args: ArgIntro[TA, ○, K]): ArgIntro[TA, L, K × L] =
    IntroFst(args)

  def wrapArgFst[TA[_], K: ProperKind, L: ProperKind](arg: TA[K]): ArgIntro[TA, L, K × L] =
    introFst(wrapArg(arg))

  def unwrap[TA[_]](i: ArgIntro[TA, ○, ●]): TA[●] =
    i match {
      case WrapArg(a) => a
    }
}
