package app

import domain._
import domain.ModelQuery._
import domain.model.User

import scala.collection.mutable.{Map => mMap}

/**
 * ModelQuery and User sample interpreter
 */
object UserRunner extends App {

  type Id[+A] = A

  // emulation of DB
  var dict = mMap.empty[Long, User]

  // actual behavior of Get
  def findUser(id: Long): User = {
    println(s"find: $dict")
    dict(id)
  }

  def findUsers(ids: Seq[Long]): Seq[User] = {
    dict.collect { case (id, user) if ids.contains(id) => user }.toSeq
  }

  // actual behavior of Pet
  def putUser(name: String, email: String): Unit = {
    println(s"put: $dict")
    val nextId = if (dict.isEmpty) 0 else dict.keySet.max + 1L
    dict.put(nextId, User(nextId, name, email))
  }

  // actual behavior of Delete
  def deleteUser(id: Long): Unit = {
    println(s"delete: $dict")
    dict.remove(id)
  }

  type FreeUser = FreeM[ModelQuery, User]

  // interpreter combined ModelQuery and User
  def interpreterUser(free: FreeUser): User =
    free match {
      case Free(Get(id, onResult: (User => FreeUser))) =>
        // find when Get
        val user: User = findUser(id)
        interpreterUser(onResult(user))
//      case Free(GetList(ids, onResult: (Seq[User] => Seq[FreeUser]))) =>
//        // findUser when GetList
//        val users: Seq[User] = findUsers(ids)
//        val next: Seq[FreeUser] = onResult(users)
//        next.map(user => interpreterUser(user)).head
      case Free(Put(name, email, next)) =>
        // put when Put
        putUser(name, email)
        interpreterUser(next)
      case Free(Delete(id, next)) =>
        // delete when Delete
        deleteUser(id)
        interpreterUser(next)
      // when comes Pure, it is the end of sequential functions
      case Pure(x) => x
    }

  // sample sequential functions, conducted by some interpreter not only `interpreterUser`
  def sequence(id: Long): FreeUser = {
    for {
      _ <- put("name1", "email1")
      _ <- put("name2", "email2")
//      _ <- delete(1)
      x <- get[User](id)
//      y <- getList[User](Seq(1, 2))
    } yield x
  }

  // actual behaviors of UserRunner App
  println(s"start: $dict")
  val result: User = interpreterUser(sequence(0))
  println(s"end: $dict")
  println(s"result => $result")
}
