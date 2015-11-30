package domain.model

import scalikejdbc._

case class User(id: Long, name: String, email: String)

//object User extends SQLSyntaxSupport[User] {
//  override def tableName: String = "users"
//  private val u = syntax("u")
//
//  private def apply(u: ResultName[User], rs:WrappedResultSet): User =
//    User(rs.long(u.id), rs.string(u.name), rs.string(u.email))
//
//  def findById(id:Long)(implicit session: DBSession = ReadOnlyAutoSession): Option[User] = withSQL {
//    select.from(User as u).where.eq(u.id, id)
//  }.single.apply
//}
