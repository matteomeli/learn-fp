package learnfp.typeclass

trait Show[A] {
  def show(x:A):String
}

object Printer {
  def show[A](x:A)(implicit showInstance:Show[A]):String = {
    showInstance.show(x)
  }
}

object ShowInstances {
  implicit val intInstance:Show[Int] = new Show[Int] {
    override def show(x: Int): String = f"$x"
  }

  implicit val doubleInstance:Show[Double] = new Show[Double] {
    override def show(x: Double): String = f"$x"
  }

  implicit def listInstance[T](implicit xShow:Show[T]):Show[List[T]] = new Show[List[T]] {
    override def show(xs:List[T]): String = {
      /*def loop(current: List[T], acc: String): String = {
        current match {
          case Nil => acc
          case l :: Nil => acc + f"$l"
          case h :: t  => loop(t, acc + f"$h, ")
        }
      }
      s"[${loop(xs, "")}]"*/
      xs.map(xShow.show(_)).mkString("[", ", ", "]")
    }
  }
}