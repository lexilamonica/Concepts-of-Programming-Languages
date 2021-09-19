/*
 * Author: Lexi LaMonica
 * All rights reserved
*/
object dynamic {

  trait Internationalization {
    def greet (name : String) : String
  }
  
  def useGreet (i18n : Internationalization, name : String) : String = {
          i18n.greet(name)
  }
  
  def newEnglish () : Internationalization = {
    class newEnglish extends Internationalization {
      def greet(name: String): String = {
        "Hello " + name
      }
    }
      val firstReturnValue = new newEnglish
      return firstReturnValue
  }
  def newFrench () : Internationalization = {
    class newFrench extends Internationalization {
      def greet(name: String): String = {
        "Bonjour " + name
      }
    }
      val secondReturnValue = new newFrench
      return secondReturnValue
  }
}

