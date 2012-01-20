package munge

class Munger { 

  import scala.io.Source
  import util.control.Exception.allCatch

  val weatherDat = 
    Source.fromFile("src/main/resources/weather.dat").getLines().toList

  val candidates = weatherDat flatMap { 

    line =>

      val tokens = line.split(" +").toList.tail
    
      tokens match { 
	
	case List(Int(id), Double(max), Double(min), _*) => 
	  
	  Some((id, max - min))

	case _ =>

	  None
      }
  }

  println("potential matches " + candidates.length)

  val res = candidates reduce { 

    (result, candidate) =>

      if (result._2 > candidate._2) { 

	result

      } else { 

	candidate
      }
  }

  println("Day " + res._1 + " has the highest spread of " + res._2)

  object Int { 

    def unapply(s : String) : Option[Int] = allCatch opt { s.toInt }
  } 

  object Double { 

    def unapply(s: String) : Option[Double] = allCatch opt { 

      s.replaceAll("[^0-9]","").toDouble 
    }
  }
}
