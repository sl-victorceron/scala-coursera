package week3.types

import java.util.NoSuchElementException

trait List[T] {

  def isEmpty : Boolean
  def head: T
  def tail: List[T]
  def nth(index: Int): Int

}

class Const[T](val head: T, val tail: List[T]) extends List[T]{
  def isEmpty: Boolean = false

  // my answer
  def nth(index: Int) : T = {
    def dig(pos: Int, list: List[T]) : T = {
      if (index < 0  ) throw new ArrayIndexOutOfBoundsException("no nth of value <0")
      else if (pos == index) return head
      else if (!list.tail.isEmpty) dig(pos+1, list.tail)
      else throw new ArrayIndexOutOfBoundsException("no nth, end of list")
    }

    dig(0, this)
  }
}

class Nil[T] extends  List[T] {
  override def isEmpty: Boolean = true

  override def head: Nothing = throw new NoSuchElementException("Nil.head")

  override def tail: Nothing = throw new NoSuchElementException("Nil.tail")

  override def nth(index: Int): Nothing = throw new ArrayIndexOutOfBoundsException("no nth on empty list")
}
