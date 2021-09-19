/*
 * Author: Lexi LaMonica
 * All rights reserved
*/
object fp1 {
  
  def fact (n : Int) : Int = {
    if (n <= 1) 
      1
    else 
      n * fact (n - 1)
  }

  val factTest : List[Int] = {
   List(fact(1), fact(2), fact(3), fact(4), fact(5))
  }
  def fib (n : Int) : Int = {
    if(n < 2)
      n
    else 
      fib(n-2) + fib(n-1)
  }
  val p1 : (Int, String) = {
    (7, "hello")
  }
  
  val t1 : (Int, String, Boolean) = {
    (7, "hello", false)
  }
  
  def swap (p:(Int,String)) : (String,Int) = {
    (p._2, p._1)
  }
  
  def sum (xs : List[Int]) : Int = {
    if(xs.isEmpty)
      0
    else if(xs.tail == Nil)
      xs.head
    else
      xs.head + sum(xs.tail)
  }
  
  def sumTailRecursiveAux (accumulator : Int, xs : List[Int]) : Int = {
    xs match {
      case Nil => accumulator
      case y::ys => sumTailRecursiveAux (accumulator + y, ys)
    }
  }

  def sumTailRecursive (xs : List[Int]) : Int = {
    sumTailRecursiveAux(0, xs)
  }
  
  def max (xs : List[Int]) : Int = {
   if(xs.isEmpty){
     0
   }
   else{
     xs.head max max(xs.tail)
   }
  }
  def maxTailRecursiveAux (accumulator : Int, xs : List[Int]) : Int = {
    if(xs == Nil) accumulator
    else{
      accumulator max maxTailRecursiveAux(xs.head, xs.tail)
    }
  }

  def maxTailRecursive (xs : List[Int]) : Int = {
    xs match {
      case Nil => throw new RuntimeException () 
      case y::ys => maxTailRecursiveAux (y, ys)
    }
  }
  
  def otpu (start : Int, end : Int) : List[Int] = {
    if(start < end)
      Nil
      else 
        start :: otpu (start-1, end)
  }
}
