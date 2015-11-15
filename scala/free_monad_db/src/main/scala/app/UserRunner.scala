package app

import domain._
import domain.ModelQuery._
import domain.model.User

import scala.collection.mutable.{Map => mMap}

/**
 * ModelQuery and User sample interpreter
 */
object UserRunner extends App {

  type Id[A] = A

  // emulation of DB
  var dict = mMap.empty[Long, User]

  // actual behavior of Get
  def findUser(id: Long): User = {
    println(s"find: $dict")
    dict(id)
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

  // interpreter combined ModelQuery and User
  def interpreterUser(free: FreeM[ModelQuery, User]): Id[User] =
    free match {
      case Free(Get(id: Long, onResult: (User => FreeM[ModelQuery, User]))) =>
        // find when Get
        val user = findUser(id)
        interpreterUser(onResult(user))
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
  def sequence[A](id: Long): FreeM[ModelQuery, A] = {
    for {
      _ <- put("name1", "email1")
      _ <- put("name2", "email2")
      _ <- delete(1)
      x <- get[A](id)
    } yield x
  }

  // actual behaviors of UserRunner App
  println(s"start: $dict")
  val result: Id[User] = interpreterUser(sequence(0))
  println(s"end: $dict")
  println(s"result => $result")
}
