package learnfp.transformer

import learnfp.functor.{Functor, FunctorOps, Reader}
import learnfp.functor.FunctorOps._
import learnfp.monad.{Monad, MonadOps}
import learnfp.monad.MonadOps._

case class ReaderT[R, M[_], A](runReaderT: R => M[A])(implicit f: Functor[M], m: Monad[M])

object ReaderT {
  implicit def readerTFunctorInstance[R, M[_]](implicit f: Functor[M], m: Monad[M]) = new Functor[({type E[X] = ReaderT[R, M, X]})#E] {
    override def fmap[A, B](a: ReaderT[R, M, A])(fx: A => B): ReaderT[R, M, B] = ReaderT { r =>
      a.runReaderT(r) fmap fx
    }
  }

  implicit def readerTToFunctorOps[R, M[_], A](a:ReaderT[R, M, A])(implicit f: Functor[M], m: Monad[M]) =
    new FunctorOps[A, ({type E[X] = ReaderT[R, M, X]})#E](a)

  implicit def readerTMonadInstance[R, M[_]](implicit f: Functor[M], m: Monad[M]) = new Monad[({type E[X] = ReaderT[R, M, X]})#E] {
    override def pure[A](a: A): ReaderT[R, M, A] = ReaderT { r => m.pure(a) }
    override def flatMap[A, B](ra: ReaderT[R, M, A])(fx: A => ReaderT[R, M, B]): ReaderT[R, M, B] = ReaderT { r =>
      ra.runReaderT(r) flatMap { a =>
        fx(a).runReaderT(r)
      }
    }
  }

  implicit def readerTToMonadOps[R, M[_], A](a: ReaderT[R, M, A])(implicit f: Functor[M], m: Monad[M]) =
    new MonadOps[A, ({type E[X] = ReaderT[R, M, X]})#E](a)

  implicit def readerTMonadTransformerInstance[R, M[_]](implicit f: Functor[M], m: Monad[M]) = new MonadTransformer[M, ({type E[X, Y[_]] = ReaderT[R, Y, X]})#E] {
    def lift[A](a: M[A]): ReaderT[R, M, A] = ReaderT { r => a }
  }

  implicit def readerTToMonadTransOps[R, M[_], A](a: M[A])(implicit m: Monad[M]) = new MonadTransOps[A, M](a)

  def lift[R, M[_], A](a: M[A])(implicit f: Functor[M], m: Monad[M]) = readerTMonadTransformerInstance[R, M].lift(a)
}