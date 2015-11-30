package domain.TestUser

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


