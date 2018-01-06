

object Main extends App {
  import org.kleemann.snakepuzzle._

  def printSolutionsImperitive(ss: List[Solution]) {
    var i = 1
    ss.foreach { s =>
      println("solution #"+i)
      println(s.toString)
      println
      i = i + 1
    }
  }

  def printSolutionsFunctional(ss: List[Solution]) {
    ss.toStream.zipWithIndex.foreach{ case (s, zeroBased) =>
      val oneBased = zeroBased + 1
      println("solution #"+oneBased)
      println(s.toString)
      println
    }
  }

  println("All Solutions\n")
  printSolutionsFunctional(allSolutions)

  println("Pruned Solutions\n")
  printSolutionsFunctional(prunedSolutions)
}