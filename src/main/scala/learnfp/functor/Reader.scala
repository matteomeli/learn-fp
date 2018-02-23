package learnfp.functor

case class Reader[R, A](run: R => A)

object ReaderInstance {
  implicit def readerInstance[R] = new Functor[({type E[X] = Reader[R, X]})#E] {
    override def fmap[A, B](a: Reader[R, A])(fx: A => B): Reader[R, B] = Reader { r =>
      fx(a.run(r))
    }
  }

  implicit def readerToFunctorOps[A, R](r: Reader[R, A]) = new FunctorOps[A, ({type E[X] = Reader[R, X]})#E](r)

  class ReaderFunctorOps[A, B](fx: A => B) {
    def `<$>`[R](r: Reader[R, A]): Reader[R, B] = r fmap fx
  }
  implicit def readerToFunctorFxOps[A, B](fx: A => B) = new ReaderFunctorOps(fx)
}