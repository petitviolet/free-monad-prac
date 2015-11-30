package domain.TestWeb

import scalaz.Coyoneda

object MiddleLayer {
  type UserId = Long
  type Name = String
  type PhotoPath = String

  case class User(userId: UserId, name: Name, photoPath: PhotoPath)

  case class Activity(userId: UserId, content: String)


  sealed trait WebService[+A]

  case class GetActivities(userId: UserId) extends WebService[Seq[Activity]]

  case class GetUserName(userId: UserId) extends WebService[Name]

  case class GetUserPhotoPath(userId: UserId) extends WebService[PhotoPath]


  sealed trait Request[+A]

  case class NoOp[+A](a: A) extends Request[A]

  case class Get[+A](service: WebService[A]) extends Request[A]

  type Requestable[+A] = Coyoneda[Request, A]
}
