/*
 * Author: Lexi LaMonica
 * All rights reserved
*/
object subtyping {
  class Counter {
    private var n = 0
    def increment() = {
      n = n + 1
    }
    def decrement() = {
      n = n - 1
    }
    def get(): Int = n
  }
 
  def observeCounter(f: Counter => Unit): Int = {
    class Count extends Counter {
      private var first = 0
      private var second = 0

      override def increment() = {
        first = first + 1
        second = second + 1
      }

      override def decrement() = {
        first = first + 1
        second = second + 1
      }

      override def get() = {
        first
      }
    }
    val third = new Count
    f(third)
    val fourth: Int = third.get()
    fourth
  }
 
  def observeCounterList(f: List[Counter] => Unit): List[Int] = {
    class Count extends Counter {
      private var first = 0
      private var second = 0

      override def increment() = {
        first = first + 1
        second = second + 1
      }

      override def decrement() = {
        first = first + 1
        second = second + 1
      }

      override def get() = {
        first
      }
    }
    val a = new Count
      val b  = new Count
        val c = new Count
    val newVal = List(a,b,c)
    f(newVal)
    val e = List(a.get, b.get, c.get)
    e
  }
  def observeCounterArray(f: Array[Counter] => Unit): Array[Int] = {
    class Count extends Counter {
      private var first = 0
      private var second = 0

      override def increment() = {
        first = first + 1
        second = second + 1
      }

      override def decrement() = {
        first = first + 1
        second = second + 1
      }

      override def get() = {
        first
      }
    }
    List(-1, -1, -1).toArray
  }
}
