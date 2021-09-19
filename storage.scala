/*
 * Author: Lexi LaMonica
 * All rights reserved
*/
object storage {
  
  val constant5: () => Int = {
    () => 5
  }

  val constant: Int => () => Int = {
    (n:Int) => (() => n)
  }

  val counter0 : () => Int = {
    var counter = 0
    () => {
      val result = counter
      counter = counter + 1
      result
    }
  }

  val counter : Int => () => Int = {
    (n:Int) => {
      var numC = n
      () => {
        val result = numC
        numC = numC + 1
        result
      }
    }
  }

  val getAndSet : Int => (() => Int, Int => Unit) = {
      (n: Int) => {
          var a: Int = n
          def get: () => Int = () => a
          def set: Int => Unit = (b: Int) => {
            a = b
          }
          (get, set)
    }
  }
  val getAndSetSpy : () => (() => Int, Int => (() => Int, Int => Unit)) = {
      () => {
          var res = 0
          val aux: Int => (() => Int, Int => Unit) = {
            (n:Int) =>
            var a = n
            def get: () => Int = () => a
            def set: Int => Unit = (b: Int) => {
              a = b
              res = res + 1
            }
            (get, set)
          }
          val counter: () => Int = () => res
          (counter, aux)
    }
  }
