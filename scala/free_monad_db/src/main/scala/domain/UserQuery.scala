package domain

import domain.model.User

sealed trait UserQuery[+A]

object UserQuery {

  case class Get[+A](id: Long, next: A) extends UserQuery[A]

  case class Put[+A](name: String, email: String, next: A) extends UserQuery[A]

  case class Delete[+A](id: Long, next: A) extends UserQuery[A]

  //  case class GetList[+A](ids: Seq[Long], next: A) extends UserQuery[Seq[A]]
  //
  //  case class UpdateName[+A](name: String, next: A) extends UserQuery[Boolean]

  def liftF[F[_], A](fa: F[A])(implicit f: Functor[F]): FreeM[F, A] =
    Free(f.fmap(fa) { (a: A) => Pure(a) })

  implicit val userFunctor = new Functor[UserQuery] {
    def fmap[A, B](u: UserQuery[A])(f: A => B): UserQuery[B] = u match {
      case Get(id, next) => Get(id, f(next))
      case Put(name, email, next) => Put(name, email, f(next))
      case Delete(id, next) => Delete(id, f(next))
    }
  }

  def get[A](id: Long): FreeM[Get, A] = {
//    val user: Long => FreeM[UserQuery, User] = x => Free(Get(id, Pure(User(id, s"name:$id", s"email:$id"))))
    liftF(Get(id, ()))
  }

  def put(name: String, email: String) = liftF(Put(name, email, ()))

  def delete(id: Long) = liftF(Delete(id, ()))

  val sequency: Long => FreeM[UserQuery, Unit] = id => {
    for {
      x <- get(id)
    } yield x
  }

  def interpreter(f: Long => User)(free: FreeM[UserQuery, Long]): Unit = {
    free match {
      case Free(Get(id, next)) => {
        val user = f(id)
        interpreter(f)(next)
      }
    }
  }
}
