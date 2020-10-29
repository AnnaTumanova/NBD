package Assigment1
import scala.annotation.tailrec
import scala.math.abs


object Assigment1 {

    def loopA(strList: List[String]): String = {
      var s = ""
      for (day <- strList)
        s += day + ", "
      s
    }

    def loopB(strList: List[String]): String = {
      var s = ""
      for (day <- strList)
        if (day.startsWith("S"))
          s += day + ", "
      s
    }

    def loopC(strList: List[String]): String = {
      var s = ""
      var i = 0
      while (i < strList.length) {
        s += strList(i) + ", "
        i += 1
      }
      s
    }
  def main(args: Array[String]): Unit = {

    val daysOfWeek = List("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    // Task 1. Create a 7 element list with names of days of the week. Create a function returning a string with comma-separated list elements.
    //for loop
    println(loopA(daysOfWeek))
    //b.	for loop, the string should contain only days with names starting with “S”
    println(loopB(daysOfWeek))
    //while loop
    println(loopC(daysOfWeek))

    def recursiveA(strList: List[String]): String = {
      if (strList.isEmpty) ""
      else strList.head + ", " + recursiveA(strList.tail)
    }

    def recursiveB(strList: List[String]): String = {
      if (strList.isEmpty) ""
      else strList.last + ", " + recursiveB(strList.splitAt(strList.length-1)._1)
    }
    // Task 2. For list from #1 create a function returning a string with comma-separated list elements
    //a.	recursive function
    println(recursiveA(daysOfWeek))
    //b.	recursive function, list elements should be printed from last to first
    println(recursiveB(daysOfWeek))

    def tailRecursion(strList: List[String]): String = {
      @tailrec
      def  iter( str: List[String], result: String): String = {
        if (str.isEmpty) result
        else iter(str.splitAt(1)._2, result + str.head + ", ")
      }
      iter(strList, "")
    }
    // Task 3.	Create a tail-recursive function returning a comma-separated string using list from #1.

    println(tailRecursion(daysOfWeek))


    def increase_by_one( l: List[Int]): List[Int] = {
      l.map(x => x+1)
    }
    // Task 6.	Define a function accepting a list of integers and returning another list with all values increased by 1. Use collection mapping.

    val values = List(1, 2, 3, 4, 5)
    println(increase_by_one(values))

    def absolute_value( l: List[Double]): List[Double] = {
      l.filter(_ <= 12 ).filter(_ >= -5).map(x => abs(x))
    }

    // Task 4.For list from #1 create a function returning a string with comma-separated list elements.
    //a.	foldleft
    println(daysOfWeek.foldLeft("")(_ + _ + ", "))
    //b.	foldright
    println(daysOfWeek.foldRight("")(_ + ", " + _))
    //c.	foldleft
    println(daysOfWeek.filter(_.startsWith("S")).foldLeft("")(_ + _ + ", ") )

    // Task 5.	Create a map with several product names (keys) and their prices (values). Based on this create a second map with 10% price reduction. Use collection mapping.

    val products = Map("tomatos" -> 7.0, "cucumber" -> 3.0, "salt" -> 1.5, "oil" -> 5.0)
    val reducedProducts = products.mapValues(x => x*0.9)
    println(reducedProducts)


    // Task 7.Create a function accepting a list of real numbers and returning a new list, containing absolute values of elements of original list with values in the <-5,12> range

    val numbers = List(9.3, 5.5, 77.66, -33.44, -10, 108.4)
    println(absolute_value(numbers))

    def tuple_print( t: (String,Boolean,Double) ) ={
      println(t._1, t._2, t._3)
    }
    // Task 8.	Define a function accepting tuple with 3 values of different types and printing it

    val test = Tuple3("test",false, 3.3)
    tuple_print(test)

    def remove_zeros( l: List[Int]): List[Int] = {
      if( l.isEmpty ) List()
      else if( l.head != 0 ) List(l.head) ::: remove_zeros(l.tail)
      else remove_zeros(l.tail)
    }
    // Task 9.	Write function accepting a list and resulting the same list without values equal to 0. Do this using recursion.
    val vals= List( 1, 0, 70, 55, 7,0,3,0)
    val notZero = remove_zeros(vals)
    println(notZero)


    def patrn(z: Option[String]) = z match
    {
      case Some(s) => (s)
      case None => ("not found")
    }
    // Task 10.	Present the use of Option (come up with an example, use at least 2 different Option methods).
    val a = Map("John" -> "tester", "Karl" -> "developer")
    println(patrn(a.get("John")))
    println(patrn(a.get("Ola")))
    println(a.getOrElse("Karl", "manager"))
    println(a.getOrElse("Maks", "developer"))
  }


}
