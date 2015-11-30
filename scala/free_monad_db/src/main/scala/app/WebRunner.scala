package app

import domain.TestWeb.MiddleLayer._
import domain.TestWeb.NoEffectInterpreter
import domain.TestWeb.Request._

import scalaz.{Id, Free}

object WebRunner extends App {

  val userId = 1

  def getUser(id: UserId): Free[Requestable, User] =
    for {
      name <- get(GetUserName(id))
      photoPath <- get(GetUserPhotoPath(id))
    } yield User(id, name, photoPath)

  def sample(id: UserId): Free[Requestable, User] =
    for {
      activities: Seq[Activity] <- get(GetActivities(id))
      result <- (activities map { activity: Activity =>
        for {
          user <- getUser(activity.userId)
        } yield activity.content -> user
      }).toMap
    } yield result

  def run(id:UserId): Id.Id[User] = {
    Free.runFC(sample(id))(NoEffectInterpreter)
  }

  run(userId)
}
