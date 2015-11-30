package domain.TestWeb

import domain.TestWeb.MiddleLayer._

import scalaz.{Id, ~>}

object NoEffectInterpreter extends (Request ~> Id.Id){
  override def apply[A](fa: Request[A]): Id.Id[A] = fa match {
    case NoOp(a) => a
    case Get(service) => service match {
      case GetActivities(userId) =>
        println(s"GetActivities: $userId")
        userId match {
          case 1 => Seq(Activity(1, "hoge1"), Activity(1, "foo1"))
          case 2 => Seq(Activity(2, "hoge2"), Activity(2, "foo2"))
          case _ => Seq.empty[Activity]
        }
      case GetUserName(userId) =>
        println(s"GetUserName: $userId")
        userId match {
          case 1 => "Alice"
          case 2 => "Bob"
          case _ => "Nobody"
        }
      case GetUserPhotoPath(userId) =>
        println(s"GetUserPhotoPath: $userId")
        userId match {
          case 1 => "Alice.jpg"
          case 2 => "Bob.jpg"
          case _ => "Nobody.jpg"
        }
    }
  }
}
