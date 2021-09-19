/*
 * Author: Lexi LaMonica
 * All rights reserved
*/
object argpass {

  class RefInt(initial: Int) {
    private var n: Int = initial

    def get(): Int = n

    def set(m: Int): Unit = {
      n = m
    }
  }
  def refint1(f: RefInt => Unit): (Int, Int, Int) = {
    val r = new RefInt(0)
    f(r)
    val x: Int = r.get
    f(r)
    val y: Int = r.get
    f(r)
    val z: Int = r.get

    (x, y, z)
  }
  def refint2(f: RefInt => Unit): (Int, Int, Int) = {
    
    val first = new RefInt(0)
    val second = new RefInt(0)
    val third = new RefInt(0)

    f(first)
    f(second)
    f(third)
    val newFirst: Int = first.get
    val newSecond: Int = second.get
    val newThird: Int = third.get

    (newFirst, newSecond, newThird)
  }
  
  def refint3(r: RefInt): RefInt = {
    val sum: RefInt = new RefInt(r.get * 2)
    r.set(r.get + 1)
    return sum
  }
  
  def refint4(r: RefInt, f: RefInt => Unit): Boolean = {
    val first = new RefInt(r.get)
    f(first)
    r.get == first.get
  }
  
  def refint5(xs: List[RefInt]): Unit = {
    xs match {
      case Nil => Nil
      case x :: xs => x.set(10)
    }
  }

}
