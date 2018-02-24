package learnfp.applicative

import learnfp.functor.Reader
import learnfp.functor.ReaderInstance._

object ReaderInstance {
  implicit def readerApplicativeInstance[R] = new Applicative[({type E[X] = Reader[R, X]})#E] {
    override def pure[A](a: A): Reader[R, A] = Reader { r => a }
    override def `<*>`[A, B](fx: Reader[R, A => B])(ra: Reader[R, A]): Reader[R, B] = Reader { r =>
      fx.run(r)(ra.run(r))
    }
  }
}