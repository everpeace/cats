package cats

import simulacrum._

@typeclass trait Monad[F[_]] extends FlatMap[F] with Applicative[F]

// If F is Monad, G is Monad and Traverse,
// we can get a comoposite monad FG for free.
trait CompositeMonadFromTraverse[F[_], G[_]]
  extends Monad[λ[α => F[G[α]]]] with CompositeApplicative[F, G]{
  implicit def F: Monad[F]
  implicit def G: Monad[G]
  implicit def TG: Traverse[G]

  private[this] def dist[C](x: G[F[C]]): F[G[C]] = TG.sequence(x)

  def flatMap[A, B](fa: F[G[A]])(f: (A) => F[G[B]]): F[G[B]] =
    F.map(F.flatten(F.map(map(fa)(f))(dist)))(G.flatten)

  override def apply[A, B](fa: F[G[A]])(f: F[G[(A) => B]]): F[G[B]] =
    F.apply(fa)(F.map(f)(gab => G.apply(_)(gab)))
}

object CompositeMonad {
  implicit def composedMonad[F[_],G[_]](implicit evF:Monad[F], evG:Monad[G], evTG:Traverse[G]):Monad[λ[α => F[G[α]]]] = new CompositeMonadFromTraverse[F,G] {
    implicit def F:Monad[F] = evF
    implicit def G:Monad[G] = evG
    implicit def TG:Traverse[G] = evTG
  }
}
