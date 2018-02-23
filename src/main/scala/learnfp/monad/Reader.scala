package learnfp.monad

import learnfp.functor.Reader
import learnfp.functor.ReaderInstance._

object ReaderInstance {
  implicit def readerMonadInstance[R] = new Monad[({type E[X] = Reader[R, X]})#E] {
    override def pure[A](a: A): Reader[R, A] = Reader { r => a }
    override def flatMap[A, B](ra: Reader[R, A])(fx: A => Reader[R, B]): Reader[R, B] = Reader { r =>
      fx(ra.run(r)).run(r)
    }
  }

  implicit def readerToMonadOps[R, A](r: Reader[R, A]) = new MonadOps[A, ({type E[X] = Reader[R, X]})#E](r)
}