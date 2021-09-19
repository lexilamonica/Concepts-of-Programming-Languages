/*
 * Author: Lexi LaMonica
 * All rights reserved
*/
object fp3 {
  def member(a: Int, xs: List[Int]): Boolean = {
    xs match {
      case Nil => false
      case y :: ys if a == y => true
      case _ :: ys => member(a, ys)
    }
  }
  
  def allEqual(xs: List[Int]): Boolean = {
    xs match {
      case Nil => true
      case y :: ys if ys == Nil => true
      case y :: ys if y != ys.head => false
      case _ :: ys => allEqual(ys)
    }
  }
  def stringLengths(xs: List[String]): List[(String, Int)] = {
    xs match {
      case Nil => Nil
      case y :: ys => xs.map(y => (y, y.length()))
    }
  }

  def delete1[X](x: X, ys: List[X]): List[X] = {
    ys match {
      case Nil => List()
      case z :: zs if z != x => z :: delete1(x, zs)
      case _ :: zs => delete1(x, zs)
    }
  }
  
  def delete2[X](x: X, ys: List[X]): List[X] = {
    for (y <- ys; if (y != x))
      yield y
  }

  def delete3[X](x: X, ys: List[X]): List[X] = {
    ys.filter((z: X) => z != x)
  }

  def removeDupes1[X](xs: List[X]): List[X] = {
    xs match {
      case Nil => List()
      case List(y) => List(y)
      case y :: ys if y != ys.head => y :: removeDupes1(ys)
      case y :: ys if !removeDupesAux(y, ys) => y :: removeDupes1(ys)
      case y :: ys => removeDupes1(ys)
    }
  }

  def removeDupesAux[X](x: X, xs: List[X]): Boolean = {
    xs match {
      case Nil => false
      case y :: ys if x == y => true
      case _ :: ys => removeDupesAux(x, ys)
    }
  }
 
  def removeDupes2[X](xs: List[X]): List[(Int, X)] = {
    xs match {
      case Nil => List()
      case y :: ys => removeDupes2Aux(y, ys, 1)
    }
  }
  def removeDupes2Aux[X](x: X, xs: List[X], count: Int): List[(Int, X)] = {
    xs match {
      case Nil if (x != null) => List((count, x))
      case Nil => List()
      case y :: ys if (y != x) => (count, x) :: removeDupes2Aux(y, ys, 1)
      case y :: ys => removeDupes2Aux(y, ys, count + 1)
    }
  }
  
  def splitAt[X](n: Int, xs: List[X]): (List[X], List[X]) = {
    (n, xs) match {
      case (a, Nil) => (Nil, Nil)
      case (-1, lst) => (List(), lst)
      case (n, x :: xs) => {
        val (a1, a2) = splitAt(n - 1, xs)
        if (n > 0) (x :: a1, a2) else (a1, x :: a2)
      }
    }
  }
  def allDistinct(xs: List[Int]): Boolean = {
    xs match {
      case Nil => true
      case y :: ys if member(y, ys) => false
      case y :: ys => allDistinct(ys)
    }
  }
}
