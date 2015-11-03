import java.io.{FileWriter, PrintWriter, File}

object ResultFree extends App {

  trait Functor[F[_]] {
    def fmap[A, B](m: F[A])(f: A => B): F[B]
  }

  sealed trait FreeM[S[+ _], +A] {
    def flatMap[B](f: A => FreeM[S, B])(implicit s: Functor[S]): FreeM[S, B]

    def map[B](f: A => B)(implicit s: Functor[S]): FreeM[S, B] = flatMap(a => Pure(f(a)))
  }

  case class Pure[S[+ _], +A](a: A) extends FreeM[S, A] {
    def flatMap[B](f: A => FreeM[S, B])(implicit s: Functor[S]): FreeM[S, B] = f(a)
  }

  case class Free[S[+ _], +A](k: S[FreeM[S, A]]) extends FreeM[S, A] {
    def flatMap[B](f: A => FreeM[S, B])(implicit s: Functor[S]): FreeM[S, B] = Free(s.fmap(k)(_.flatMap(f)))
  }

  sealed trait Result[+A]

  case class Get[+A](a: A) extends Result[A]

  case class Fail[+A]() extends Result[A]

  implicit val resultFunctor = new Functor[Result] {
    def fmap[A, B](a: Result[A])(f: A => B): Result[B] = a match {
      case Get(x) => Get(f(x))
      case Fail() => Fail()
    }
  }


  def liftF[S[+ _] : Functor, A](f: A => FreeM[Result, A], cmd: A): FreeM[Result, A] = {
    Free(resultFunctor.fmap(Get(cmd))(f))
  }

  def interpreter(f: String => Unit)(free: FreeM[Result, String]): Unit = {
    free match {
      case Free(Get(r)) => f(s"Get: $r"); interpreter(f)(r)
      case Free(Fail()) => f("Fail")
      case Pure(r) => f(s"pure: $r")
    }
  }

  def runStdIO(free: FreeM[Result, String]): Unit = {
    interpreter(println)(free)
  }

  def runFileIO(free: FreeM[Result, String]): Unit = {
    def log(msg: String): Unit = {
      val file = new FileWriter("free.log", true)
      file.append(msg + "\n")
      file.close()
    }
    interpreter(log)(free)
  }

  def getF(x: String): FreeM[Result, String] = {
    val _get: String => FreeM[Result, String] = x => Free(Get(Pure(x)))
    liftF(_get, x)
  }

  def failF: FreeM[Result, _] = Free(Fail())

  val io: FreeM[Result, String] = for {
    x <- getF("hoge")
    y <- getF("foo")
    z <- failF
  } yield x + y + y

  println("start")
  runStdIO(io)
  println("end")
  println("start")
  runFileIO(io)
  println("end")
}
