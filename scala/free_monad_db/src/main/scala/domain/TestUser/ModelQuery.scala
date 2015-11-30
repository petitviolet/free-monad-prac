package domain.TestUser

/**
 * Base of Query
 */
sealed trait ModelQuery[+A]

/**
 * Constructor of DSLs
 */
case class Get[+A, B](id: Long, onResult: B => A) extends ModelQuery[A]

case class GetList[+A, B](ids: Seq[Long], onResult: B => Seq[A]) extends ModelQuery[Seq[A]]

case class Put[+A](name: String, email: String, next: A) extends ModelQuery[A]

case class Delete[+A](id: Long, next: A) extends ModelQuery[A]

/**
 * defined some functions of DSL
 * use these in implementation of interpreter
 */
object ModelQuery {


  //  case class GetList[+A](ids: Seq[Long], next: A) extends UserQuery[Seq[A]]
  //
  //  case class UpdateName[+A](name: String, next: A) extends UserQuery[Boolean]

  def liftF[F[+ _] : Functor, A](fa: F[A])(implicit f: Functor[F]): FreeM[F, A] =
    Free(f.fmap(fa) { a => Pure(a) })

  implicit val userFunctor = new Functor[ModelQuery] {
    def fmap[A, B](u: ModelQuery[A])(f: A => B): ModelQuery[B] = u match {
      case Get(id, next: (B => A)) => Get(id, f compose next)
//      case GetList(ids, next: (B => Seq[A])) => GetList(ids, next andThen(_.map(f)))
      case Put(name, email, next) => Put(name, email, f(next))
      case Delete(id, next) => Delete(id, f(next))
    }
  }

  def get[A](id: Long): FreeM[ModelQuery, A] = {
    liftF(Get[A, A](id, identity))
  }

  def getList[A](ids: Seq[Long]): FreeM[ModelQuery, Seq[A]] = {
    liftF(GetList[A, Seq[A]](ids, identity))
  }

  def put(name: String, email: String): FreeM[ModelQuery, Unit] = {
    liftF(Put(name, email, ()))
  }

  def delete(id: Long): FreeM[ModelQuery, Unit] = {
    liftF(Delete(id, ()))
  }
}
