

object Main extends App {
  import org.kleemann.snakepuzzle._

  def printSolutions(ss: List[Solution]) {
    var i = 1
    ss.foreach { s =>
      println("solution #"+i)
      println(s.toString)
      println
      i = i + 1
    }
  }

  println("All Solutions\n")
  printSolutions(allSolutions)

  println("Pruned Solutions\n")
  printSolutions(prunedSolutions)
}