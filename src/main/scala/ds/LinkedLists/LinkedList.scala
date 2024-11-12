package ds.LinkedLists

trait LinkedList[+T] {

  def value: T

  def next: LinkedList[T]

  def append[S >: T](element: S): LinkedList[S]

  def prepend[S >: T](element: S): LinkedList[S]

  def size: Int

  def deleteHead: LinkedList[T]

  def deleteTail: LinkedList[T]

}

case object EmptyLinkedList extends LinkedList[Nothing]{
  override def value: Nothing = throw new NullPointerException("Empty Linked List Found")

  override def next: LinkedList[Nothing] = throw new NullPointerException("Empty Linked List Found")

  override def append[S >: Nothing](element: S)= new NonEmptyLinkedList[S](element, EmptyLinkedList)

  override def prepend[S >: Nothing](element: S) = new NonEmptyLinkedList[S](element, EmptyLinkedList)

  override def size: Int = 0

  override def deleteHead: LinkedList[Nothing] = throw new NullPointerException("Empty Linked List Found")

  override def deleteTail: LinkedList[Nothing] = throw new NullPointerException("Empty Linked List Found")

}

class NonEmptyLinkedList[T](override val value: T, nextAdd: LinkedList[T]) extends LinkedList[T] {

  override def next: LinkedList[T] = nextAdd

  override def append[S >: T](element: S): LinkedList[S] = new NonEmptyLinkedList[S](value, nextAdd.append(element))

  override def prepend[S >: T](element: S): LinkedList[S] = new NonEmptyLinkedList[S](element, this)

  override def size: Int = calculateSize(nextAdd)

  override def deleteHead: LinkedList[T] = nextAdd

  override def deleteTail: LinkedList[T] = removeTailFromLinkedList(nextAdd)

  private def calculateSize(linkedList: LinkedList[T], sizeCounter: Int = 1): Int = {
    linkedList match {
      case EmptyLinkedList => sizeCounter
      case _: NonEmptyLinkedList[_] => calculateSize(linkedList.next, sizeCounter + 1)
      case _ => 0
    }
  }

  private def removeTailFromLinkedList(currentList: LinkedList[T]): LinkedList[T] = {
    currentList match {
      case EmptyLinkedList => EmptyLinkedList
      case list: NonEmptyLinkedList[_]  if(list.next == EmptyLinkedList) => new NonEmptyLinkedList[T](value, EmptyLinkedList)
      case _ => new NonEmptyLinkedList[T](value, nextAdd.deleteTail)
    }
  }
}

object LinkedList extends App {
  val list = EmptyLinkedList.append(3).append(4).prepend(10)
  println(list.size)
  println(list.deleteTail.size)
}
