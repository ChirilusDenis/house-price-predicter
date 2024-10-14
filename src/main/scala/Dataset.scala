import scala.io.Source

class Dataset(m: List[List[String]]) {
  val data: List[List[String]] = m
  override def toString: String = data.map(_.mkString(",")).mkString("\n")

  def selectColumn(col: String): Dataset = {
    def aux(acc: List[List[String]], mat: List[List[String]]): List[List[String]] = {
      mat match {
        case Nil :: _ => acc
        case _ => if (mat.map(_.head).head.equals(col)) aux(mat.map(_.head) :: acc, mat.map(_.tail))
        else aux(acc, mat.map(_.tail))
      }
    }

    Dataset(refl(aux(Nil, data)))
  }

  def selectColumns(cols: List[String]): Dataset = {
    def aux(mat: List[List[String]]): List[List[String]] = {
      mat match {
        case Nil :: _ => Nil
        case _ => if (cols.contains(mat.map(_.head).head)) mat.map(_.head) :: aux(mat.map(_.tail))
        else aux(mat.map(_.tail))
      }
    }

    Dataset.apply(refl(aux(data)))
  }
  def split(percentage: Double): (Dataset, Dataset) = {
    val sorted = data.tail.sortWith((l1: List[String], l2: List[String]) => l2.head.compareTo(l1.head) > 0)

    def getNr(percent: Double): Int = {
      val aux = 1 / percent
      if (aux.floor == aux) aux.toInt
      else aux.toInt + 1
    }
    val number = getNr(percentage)

    def aux(acc : (List[List[String]], List[List[String]]),
            sorted : List[List[String]],
            crt : Int) : (List[List[String]], List[List[String]]) = {
      sorted match {
        case Nil => acc
        case x :: xs => if(crt % number == number - 1) aux((acc._1, acc._2 :+ x), xs, crt + 1)
        else aux((acc._1 :+ x , acc._2), xs, crt + 1)
      }
    }

    val separated = aux((Nil, Nil), sorted, 0)
    (Dataset(getHeader :: separated._1), Dataset(getHeader :: separated._2))
  }

  def size: Int = m.tail.size
  def getRows: List[List[String]] = m.tail
  def getHeader: List[String] = m.head

  def refl(mat: List[List[String]]): List[List[String]] = {
    mat match {
      case Nil :: _ => Nil
      case _ => mat.map(_.head) :: refl(mat.map(_.tail))
    }
  }

}

object Dataset {
  def apply(csv_filename: String): Dataset = {
    val data = Source.fromFile(csv_filename).getLines().toList
      .map(_.split(",").toList)
    new Dataset(data)
  }

  def apply(ds: List[List[String]]): Dataset = {
    new Dataset(ds)
  }

}
