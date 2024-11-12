package ds.Recursion

object Factorial extends App {

  def getFactorial(number: Int, currentValue: Int = 1): Int = {
    number match {
      case 1 => currentValue
      case n => getFactorial(number-1, currentValue*n)
    }
  }

  println(getFactorial(4))

}
