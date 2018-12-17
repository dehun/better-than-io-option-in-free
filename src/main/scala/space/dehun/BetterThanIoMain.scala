package space.dehun

import cats.~>
import cats._
import cats.data.EitherT
import cats.implicits._
import cats.effect.{Effect, IO}
import cats.free.Free
import cats.Inject._
import cats.arrow.FunctionK
import cats.data._

object BetterThanIoMain extends App {
  type Level0[A] = EitherK[LogAction, NetAction, A]
  type AppStack[A] = EitherK[DbAction, Level0, A]

  type LogAndDb[A] = EitherK[LogAction, DbAction, A]
  def foo(x:Int):Free[LogAndDb, User] = for {
    user <- Db.queryUser(x.toString).inject[LogAndDb]
    _ <- Log.logMsg(s"got user ${user}").inject[LogAndDb]
    _ <- Db.storeUser(user.copy(age=user.age + 1)).inject[LogAndDb]
  } yield user

  type LogAndNet[A] = EitherK[LogAction, NetAction, A]
  def bar(user:User):Free[LogAndNet, Int] = for {
    _ <- Log.logMsg("lets notify user change!").inject[LogAndNet]
    _ <- Net.notifyUserChange(user).inject[LogAndNet]
  } yield user.age

  type MaybeAndAppStack[A] = EitherK[MaybeAction, AppStack, A]
  def foo2(x:Int):Free[MaybeAndAppStack, User] = for {
    user <- Db.queryUser(x.toString).inject[MaybeAndAppStack]
    _ <- Log.logMsg(s"got user ${user}").inject[MaybeAndAppStack]
    _ <- Maybe.nothing[Unit].inject[MaybeAndAppStack]
    _ <- Db.storeUser(user.copy(age=user.age + 1)).inject[MaybeAndAppStack]
  } yield user

  def foo3(x:Int):Free[MaybeAndAppStack, User] = for {
    user <- Maybe.lift(Db.maybeQueryUser(x.toString).inject[MaybeAndAppStack])
    _ <- Log.logMsg(s"got user ${user}").inject[MaybeAndAppStack]
    _ <- Db.storeUser(user.copy(age=user.age + 1)).inject[MaybeAndAppStack]
  } yield user

  override def main(args: Array[String]): Unit = {
    lazy val logIoNat = new LogIoNat().andThen[OptionT[IO, ?]](OptionT.liftK)
    lazy val dbIoNat = new DbIoNat().andThen[OptionT[IO, ?]](OptionT.liftK)
    lazy val netIoNat = new NetIoNat().andThen[OptionT[IO, ?]](OptionT.liftK)
    lazy val maybeIoNat = new MaybeIoNat()

    lazy val level0Nat = logIoNat.or(netIoNat)
    lazy val appStackNat = dbIoNat.or[Level0](level0Nat)

    implicit val logAndDbIntoAppStack = new InjectK[LogAndDb, MaybeAndAppStack] {
      override def inj: ~>[LogAndDb, MaybeAndAppStack] = new ~>[LogAndDb, MaybeAndAppStack] {
        override def apply[A](fa: LogAndDb[A]): MaybeAndAppStack[A] = fa match {
          case EitherK(Left(act)) => InjectK[LogAction, MaybeAndAppStack].inj(act)
          case EitherK(Right(act)) => InjectK[DbAction, MaybeAndAppStack].inj(act)
        }
      }

      override def prj: ~>[MaybeAndAppStack, λ[α => Option[LogAndDb[α]]]] = ??? // not used
    }

    //val appStackNatOptionT = appStackNat.andThen()

    lazy val maybeAndAppStackNat = maybeIoNat.or[AppStack](appStackNat)
    val r = (foo3(12).inject[MaybeAndAppStack] flatMap (u => bar(u).inject[MaybeAndAppStack]))
      .foldMap[OptionT[IO, ?]](maybeAndAppStackNat).value
      .unsafeRunSync()
    Console.println(s"end result: $r")
  }
}
