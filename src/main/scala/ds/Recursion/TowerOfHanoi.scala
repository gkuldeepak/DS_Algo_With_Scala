package ds.Recursion

object TowerOfHanoi extends App {

  def towerOfHanoi(discs: Int, source: Char, auxiliary: Char, target: Char): Unit = {
    if(discs == 1) println(s"Transfer disc ${discs} from ${source} to ${target}")
    else {
      towerOfHanoi(discs-1, source, target, auxiliary)
      println(s"Transfer disc ${discs} from ${source} to ${target}")
      towerOfHanoi(discs-1, auxiliary, source, target)
    }
  }

  towerOfHanoi(3, 'A', 'B', 'C')

}
