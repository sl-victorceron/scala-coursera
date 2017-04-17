
object session2Currying {

  type Set = Int => Boolean

  /**
    * Indicates whether a set contains a given element.
    */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  contains(Set(1,2,3), 1)

  def sum(f: Int => Int) : (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int =
      if (a>b) 0
      else f(a) + sumF(a+1,b)

    sumF
  }

  sum(x => x*x*x)(3,5)
  sum(x => x*x)(3,5)
  sum(x => x)(3,5)



  def otherSum(f: Int => Int)(a:Int, b:Int) : Int =
    if (a > b) 0 else f(a) + otherSum(f)(a+1, b)


  otherSum(x => x)(3,5)
  otherSum(x => x * x)(3,4)



  def product(f:Int => Int) (a:Int, b:Int) : Int =
    if (a > b) 1 else f(a) * product(f)(a+1, b)

  product(x => x)(3, 5)
  product(x => x*x)(3,4)

  def factorial(a:Int): Int =
  product(x=>x)(1,a)

  factorial(6)

  // Function that combines sum & product
  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b:Int) : Int =
  if (a > b) zero
  else combine(f(a),mapReduce(f, combine, zero)(a+1, b))

  def altProduct(f:Int => Int) (a:Int, b:Int) : Int = mapReduce(f, (x, y) => x*y, 1)(a, b)
  altProduct(x => x)(3, 5)
  altProduct(x => x*x)(3,4)

  def altSum(f:Int => Int) (a:Int, b:Int) : Int = mapReduce(f, (x, y) => x+y, 0)(a, b)
  altSum(x => x)(3, 5)
  altSum(x => x*x)(3,4)
}