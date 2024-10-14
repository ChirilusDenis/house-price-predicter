type Mat = List[List[Double]]

class Matrix(m: Option[List[List[Double]]]) {

  def transpose: Matrix = {
    def trans_aux(mat: Mat): Mat = {
      mat match {
        case Nil :: _ => Nil
        case _ => mat.map(_.head) :: trans_aux(mat.map(_.tail))
      }
    }

    m match
      case None => Matrix(None)
      case Some(x) => Matrix(trans_aux(x))
  }

  def map(f: Double => Double): Matrix = {
    m match {
      case None => Matrix(None)
      case Some(x) => Matrix(x.map(_.map(f)))
    }
  }

  def *(other: Matrix): Matrix = {
    (m, other.data) match
      case (Some(m1), Some(m2)) =>
        if (width == other.height)
          Matrix(m1.map(line => other.transpose.data.get
            .map(col => line.zip(col)
              .map(pair => pair._1 * pair._2)
              .foldRight(0.0)(_ + _))))
        else Matrix(None)

      case _ => Matrix(None)
  }

  def ++(x: Double): Matrix = {
    m match {
      case None => Matrix(None)
      case Some(y) => Matrix(y.map(_ :+ x))
    }
  }

  def -(other: Matrix): Matrix = {
    (m, other.data) match {
      case (Some(x), Some(y)) =>
        if (x.size.equals(y.size) & x.head.size.equals(y.head.size))
          Matrix(x.zip(y).map(p => p._1.zip(p._2)).map(
            _.map(pair => pair._1 - pair._2)))
        else Matrix(None)

      case _ => Matrix(None)
    }
  }

  def data: Option[Mat] = m

  def height: Option[Int] = {
    m match {
      case None => None
      case Some(x) => Some(x.size)
    }
  }

  def width: Option[Int] = {
    m match {
      case None => None
      case Some(x) => Some(x.head.size)
    }
  }

  override def toString: String = {
    m match
      case None => ""
      case Some(x) => x.map(_.mkString(" ")).mkString("\n")
  }

}

object Matrix {
  def apply(data: Mat): Matrix = new Matrix(Some(data))
  def apply(data: Option[Mat]): Matrix = new Matrix(data)
  def apply(dataset: Dataset): Matrix = Matrix.apply(dataset.getRows.map(_.map(_.toDouble)))
}
