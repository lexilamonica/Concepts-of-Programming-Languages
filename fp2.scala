/*
 * Author: Lexi LaMonica
 * All rights reserved
*/
object fp2 {
  def map[A, B](xs: List[A], f: A => B): List[B] = {
    xs match {
      case Nil => Nil
      case y :: ys => f(y) :: map(ys, f)
    }
  }

  def filter[A](xs: List[A], f: A => Boolean): List[A] = {
    xs match {
      case Nil => Nil
      case y :: ys if f(y) => y :: filter(ys, f)
      case _ :: ys => filter(ys, f)
    }
  }

  def append[A](xs: List[A], ys: List[A]): List[A] = {
    xs match {
      case Nil => ys
      case z :: zs => z :: append(zs, ys)
    }
  }

  def flatten[A](xss: List[List[A]]): List[A] = {
    xss match {
      case Nil => Nil
      case y :: ys => y ::: flatten(ys)
    }
  }

  def foldLeft[A, B](xs: List[A], e: B, f: (B, A) => B): B = {
    xs match {
      case Nil => e
      case y :: ys => foldLeft(ys, f(e, y), f)
    }
  }
  
  def foldRight[A, B](xs: List[A], e: B, f: (A, B) => B): B = {
    xs match {
      case Nil => e
      case y :: ys => f(y, foldRight(ys, e, f))
    }
  }

  def joinTerminateLeft(xs: List[String], term: String): String = {
    xs match {
      case Nil => ""
      case y :: ys => y + foldLeft(ys, term, (a: String, b: String) => a + b + term)
    }
  }

  def joinTerminateRight(xs: List[String], delimiter: String): String = {
    xs match {
      case Nil => ""
      case y :: ys => y + foldRight(ys, delimiter, (a: String, b: String) => delimiter + a + b)
    }
  }
  
  def firstNumGreaterThan(a: Int, xs: List[Int]): Int = {
    xs match {
      case Nil => throw new RuntimeException()
      case y :: ys if y >= a => y
      case _ :: ys => firstNumGreaterThan(a, ys)
    }
  }
 
  def firstIndexNumGreaterThan(a: Int, xs: List[Int]): Int = {
    xs match {
      case Nil => throw new RuntimeException()
      case y :: ys if y >= a => 0
      case _ :: ys => firstIndexNumGreaterThanCount(1, a, ys)
    }
  }

  def firstIndexNumGreaterThanCount(x: Int, a: Int, xs: List[Int]): Int = {
    xs match {
      case Nil => throw new RuntimeException()
      case y :: ys if y >= a => x
      case _ :: ys => firstIndexNumGreaterThanCount(x + 1, a, ys)
    }

  }
}

