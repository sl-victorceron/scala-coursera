//package week3.types

import week3.types.{Const, Nil}
//import week3.types.Nil

object ListsTest{
  def nth[T](n: Int, xs: List[T]): T =
   if (n == 0) xs.head
   else nth(n-1, xs.tail)

  private val empty: Nil[Int] = new Nil[Int]
  private val list2: Const[Int] = new Const[Int](1, new Nil[Int])
  private val list3: Const[Int] = new Const[Int](1, new Const[Int](2, new Const[Int](3, new Nil[Int])))


}