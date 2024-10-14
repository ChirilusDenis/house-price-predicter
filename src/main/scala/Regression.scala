import scala.annotation.tailrec

object Regression {

  def regression(dataset_file: String,
                attribute_columns: List[String],
                value_column: String,
                test_percentage: Double,
                alpha: Double,
                gradient_descent_steps: Int): (Matrix, Double) = {

    val data = Dataset(dataset_file)

    val toTrain = data.split(test_percentage)._1
    val toTest = data.split(test_percentage)._2

    val toFindTrain = Matrix(toTrain.selectColumn(value_column))
    val toFindTest = Matrix(toTest.selectColumn(value_column))

    val toUseTrain = Matrix(toTrain.selectColumns(attribute_columns)) ++ 1.0
    val toUseTest = Matrix(toTest.selectColumns(attribute_columns)) ++ 1.0

    def initW(size: Int): List[List[Double]] = {
      size match {
        case 0 => Nil
        case _ => List(0.0) :: initW(size - 1)
      }
    }

    val W = Matrix(initW(toUseTrain.width.get))

    @tailrec
    def gradientDescent(W: Matrix, steps: Int): Matrix = {
      steps match {
        case 0 => W
        case _ => {
          val E = toUseTrain * W
//          val error = (toFindTrain - E).map(math.abs(_))
          val error = E - toFindTrain
          val grad = (toUseTrain.transpose * error).map(_ / toUseTrain.height.get)
          val newW = W - grad.map(_ * alpha)
          gradientDescent(newW, steps - 1)
        }
      }
    }

    val aproxW = gradientDescent(W, gradient_descent_steps)
    val prediction = toUseTest * aproxW
    val testError = prediction - toFindTest

    val allErr = testError.data.get.foldRight(0.0)((now: List[Double], acc: Double) =>
      acc + now.head) / testError.height.get.toDouble

    (aproxW, allErr)

  }

  def main(args: Array[String]): Unit = {
    // Exemplu de utilizare
    print(regression("datasets/houseds.csv", List("GrLivArea", "YearBuilt"), "SalePrice", 0.1, 1e-7, 10000))
  }
}