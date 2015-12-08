import java.io.FileWriter

object ResultFree extends App {

  trait Functor[F[_]] {
    def fmap[A, B](m: F[A])(f: A => B): F[B]
  }

  /**
   * Free Monad
   */
  sealed trait FreeM[S[+ _], +A] {
    def flatMap[B](f: A => FreeM[S, B])(implicit s: Functor[S]): FreeM[S, B]

    def map[B](f: A => B)(implicit s: Functor[S]): FreeM[S, B] = flatMap(a => Pure(f(a)))
  }

  case class Pure[S[+ _], +A](a: A) extends FreeM[S, A] {
    def flatMap[B](f: A => FreeM[S, B])(implicit s: Functor[S]): FreeM[S, B] = f(a)
  }

  case class Free[S[+ _], +A](k: S[FreeM[S, A]]) extends FreeM[S, A] {
    def flatMap[B](f: A => FreeM[S, B])(implicit s: Functor[S]): FreeM[S, B] =
      Free(s.fmap(k)((i: FreeM[S, A]) => i.flatMap(f)))
  }

  /**
   * Type to convert to Monad by Free
   */
  sealed trait Result[+A]

  case class Success[+A](a: A) extends Result[A]

  case class Fail[+A]() extends Result[A]

  implicit val resultFunctor = new Functor[Result] {
    def fmap[A, B](a: Result[A])(f: A => B): Result[B] = a match {
      case Success(x) => Success(f(x))
      case Fail() => Fail()
    }
  }

  /**
   * interpreter for Result Free Monad
   */
  def interpreter(f: String => Unit)(free: FreeM[Result, String]): Unit = {
    free match {
      case Free(Success(r)) => f(s"Success: $r"); interpreter(f)(r)
      case Free(Fail()) => f("Fail")
      case Pure(r) => f(s"pure: $r")
    }
  }

  /**
   * StdIO interpreter
   */
  def runStdIO(free: FreeM[Result, String]): Unit = {
    interpreter(println)(free)
  }

  def log(msg: String)(implicit writer: FileWriter): Unit = {
    writer.append(msg + "\n")
  }

  /**
   * FileIO interpreter
   */
  def runFileIO(free: FreeM[Result, String]): Unit = {
    implicit val writer = new FileWriter("free.log", true)
    interpreter(log)(free)
    writer.close()
  }

  def liftF[S[+ _] : Functor, A](f: A => FreeM[Result, A], cmd: A): FreeM[Result, A] = {
    Free(resultFunctor.fmap(Success(cmd))(f))
  }

  def successF(x: String): FreeM[Result, String] = {
    val _success: String => FreeM[Result, String] = x => Free(Success(Pure(x)))
    liftF(_success, x)
  }

  def failF: FreeM[Result, _] = Free(Fail())

  val io: FreeM[Result, String] = for {
    x <- successF("hoge")
    y <- successF("foo")
    z <- failF
  } yield x + y + y

  println("start")
  runStdIO(io)
  println("end")
  println("start")
  runFileIO(io)
  println("end")
}
