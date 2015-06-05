  object analyzeMeetings {
    def main(args: Array[String]) {
     if(args.length == 2) {
      println(" Analyzing if the two userId's specified as arguments have met in the building.")
      println("")
      import sys.process._
      ("Rscript analyzeMeetings.R "+ args(0)+" "+ args(1))!
     } else {
      println("Usage: scala analyzeMeetings uid1 uid2")
     }
    }
  }


