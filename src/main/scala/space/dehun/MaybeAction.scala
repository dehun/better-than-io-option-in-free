package space.dehun

import cats.InjectK
import cats.free.Free

sealed trait MaybeAction[A]

object MaybeAction {
  case class Just[A](value:A) extends MaybeAction[A]
  case class Nothing[A]() extends MaybeAction[A]
}

object Maybe {
  def just[A](value:A):Free[MaybeAction, A] = Free.liftF(MaybeAction.Just[A](value))
  def nothing[A]:Free[MaybeAction, A] = Free.liftF(MaybeAction.Nothing[A]())
  def lift[F[_], A](value:Free[F, Option[A]])(implicit maybeInjector:InjectK[MaybeAction, F]):Free[F, A] = value.flatMap {
    case None => Free.liftF(maybeInjector.inj(MaybeAction.Nothing[A]()))
    case Some(v) => Free.liftF(maybeInjector.inj(MaybeAction.Just[A](v)))
  }
}