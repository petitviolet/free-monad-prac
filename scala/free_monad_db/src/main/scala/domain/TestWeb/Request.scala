package domain.TestWeb

import domain.TestWeb.MiddleLayer._

import scalaz.Free

object Request {
  def noop[A](a: A): Free[Requestable, A] = Free.liftFC(NoOp(a))

  def get[A](service: WebService[A]): Free[Requestable, A] = Free.liftFC(Get(service))
}
