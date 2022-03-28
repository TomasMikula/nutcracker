package nutcracker.demo.simpletypeinference.types

import nutcracker.demo.simpletypeinference.kinds._

sealed trait Routing[K, L](using
  val inKind: Kind[K],
  val outKind: Kind[L],
) {
  import Routing._

  def >[M](that: Routing[L, M]): Routing[K, M] =
    AndThen(this, that)

  def snd[X](using ProperKind[X], ProperKind[K], ProperKind[L]): Routing[X × K, X × L] =
    Routing.snd(this)

  def applyTo[TA[_], J](args: ArgIntro[TA, J, K]): ApplyRes[TA, J, ?, L] = {
    import args.inKind

    this match {
      case Id() =>
        ApplyRes(Id(), args)
      case AndThen(f, g) =>
        f.applyTo(args) match {
          case ApplyRes(f1, args1) =>
            g.applyTo(args1) match {
              case ApplyRes(g1, args2) => ApplyRes(f1 > g1, args2)
            }
        }
      case p: Par[k1, k2, l1, l2] =>
        given ProperKind[l2] = Kind.snd(p.outKind)
        args match {
          case i1: ArgIntro.IntroFst[TA, `l1`, y] =>
            ApplyRes(p.f2, ArgIntro.introFst[TA, l1, l2](p.f1.applyTo0(i1.args)))
          case other =>
            throw new NotImplementedError(s"$other")
        }
      case other =>
        throw new NotImplementedError(s"applying $other to $args")
    }
  }

  def applyTo0[TA[_]](args: ArgIntro[TA, ○, K]): ArgIntro[TA, ○, L] = {
    this match {
      case Id() => args
      case AndThen(f, g) => g.applyTo0(f.applyTo0(args))
      case Dup() => ArgIntro.introBoth(args, args)
      case other => throw new NotImplementedError(s"$other")
    }
  }

  def applyToTrans[F[_, _], J](f: ArgTrans[F, J, K]): AppTransRes[F, J, ?, L] = {
    import f.inKind

    this match {
      case Id() => AppTransRes(Id(), f)
      case other => throw new NotImplementedError(s"$other")
    }
  }
}

object Routing {
  case class AndThen[K, L, M](
    f: Routing[K, L],
    g: Routing[L, M],
  ) extends Routing[K, M](using f.inKind, g.outKind)

  case class Id[K: Kind]() extends Routing[K, K]

  case class Par[K1: ProperKind, K2: ProperKind, L1: ProperKind, L2: ProperKind](
    f1: Routing[K1, L1],
    f2: Routing[K2, L2],
  ) extends Routing[K1 × K2, L1 × L2]

  case class ElimSnd[K: ProperKind, L: ProperKind]() extends Routing[K × L, K]

  case class Dup[K]()(using val kind: OutputKind[K]) extends Routing[K, K × K]

  case class ApplyRes[TA[_], K, X, L](r: Routing[K, X], ai: ArgIntro[TA, X, L])

  case class AppTransRes[F[_, _], K, X, L](f: Routing[K, X], g: ArgTrans[F, X, L])

  def id[K: Kind]: Routing[K, K] =
    Id()

  def par[K1: ProperKind, K2: ProperKind, L1: ProperKind, L2: ProperKind](
    f1: Routing[K1, L1],
    f2: Routing[K2, L2],
  ): Routing[K1 × K2, L1 × L2] =
    Par(f1, f2)

  def snd[K: ProperKind, L: ProperKind, M: ProperKind](f: Routing[L, M]): Routing[K × L, K × M] =
    par(Id(), f)

  def elimSnd[K: ProperKind, L: ProperKind]: Routing[K × L, K] =
    ElimSnd()

  def dup[K: OutputKind]: Routing[K, K × K] =
    Dup()

  def proveId[K](r: Routing[○, K]): K =:= ○ =
    r match {
      case Id() => implicitly[K =:= ○]
      case other => throw new NotImplementedError(s"$other")
    }
}
