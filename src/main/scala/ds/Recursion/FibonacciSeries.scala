package ds.Recursion

object FibonacciSeries extends App{

  def getFibonacciSeries(number: Int): List[Int] = {
    number match {
      case n if n < 1 => throw new NoSuchElementException("Invalid Number !!!")
      case 1 => List(0)
      case 2 => List(0, 1)
      case n => getSeries(n - 2, List(0, 1))
    }
  }

  private def getSeries(number: Int, currentList: List[Int]): List[Int] = {
    number match {
      case 0 => currentList
      case n => getSeries(n - 1, currentList :+ currentList.takeRight(2).sum)
    }
  }

  println(getFibonacciSeries(1))

}