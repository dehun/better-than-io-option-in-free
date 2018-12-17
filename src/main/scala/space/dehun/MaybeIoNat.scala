package space.dehun

import cats.data.OptionT
import cats.effect.IO
import cats.~>

class MaybeIoNat extends ~>[MaybeAction, OptionT[IO, ?]] {
  override def apply[A](fa: MaybeAction[A]): OptionT[IO, A] = fa match {
    case MaybeAction.Just(value) => OptionT.some[IO](value)
    case MaybeAction.Nothing() => OptionT.none[IO, A]
  }
}
