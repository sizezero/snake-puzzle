

object Main extends App {
  import org.kleemann.snakepuzzle._
  var i = 1
  allSolutions.foreach { s =>
    println("solution #"+i)
    println(s.toString)
    println
    i = i + 1
  }
}